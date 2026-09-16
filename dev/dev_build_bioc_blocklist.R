# dev_build_bioc_blocklist.R -------------------------------------------------
#
# Provisioning helper: build a *complete* BioC blocklist for PPM, by
# comparing the full BioC universe (available.packages() against the
# configured BioC repo(s)) to the pipeline's Low-risk BioC allowlist.
#
# Why this exists
# ---------------
# `write_qualified_pkg_lists()` writes `blocklist-BioC.txt` as the
# inverse of the *assessed* BioC set (pkgs in qual_metadata whose
# final_decision != "Low"). That's an incomplete blocklist: any BioC
# pkg dropped at remote_reduce (or otherwise never seen by the
# pipeline) is missing from both the allowlist and the blocklist, so
# PPM would happily serve it.
#
# The correct denominator is:
#
#   blocklist = BioC_universe − low_risk_allowlist
#             = (dropped_at_remote_reduce ∪ never_seen) +
#               (assessed_but_not_Low)
#
# This script pulls `BioC_universe` from `available.packages()` against
# whatever repos in the effective `opt_repos` config look like BioC
# (case-insensitive "bioc" substring match on BOTH the alias name and
# the URL — the non-OSS fork collapses the four BioC sub-repos into a
# single entry whose alias/URL both contain "bioc", so this covers it).
#
# Usage
# -----
#   source("dev/dev_build_bioc_blocklist.R")
#
#   res <- build_bioc_blocklist(
#     val_dir  = "/data/shared/riskassessments/R_4.5.2/20260730",
#     out_dir  = "dev/",
#     # opt_repos   = list(CRAN = "...", BioC = "...")  # optional override
#     # config_path = "inst/config.yml"                  # optional override
#   )
#
#   # Or feed a qual_metadata frame directly:
#   qm <- readRDS(".../qual_metadata.rds")
#   res <- build_bioc_blocklist(qual_metadata = qm, out_dir = "dev/")
#
# The script does NOT commit or overwrite the pipeline's own
# blocklist-BioC.txt — it writes a separate CSV so provisioning can
# consume it explicitly.

suppressPackageStartupMessages({
  library(val.pipeline)
})


# Detect which entries of a named `repos` vector look like BioC.
#
# Substring match on "bioc" (case-insensitive) against BOTH the repo
# alias (names(repos)) and the repo URL (values). Non-OSS forks that
# consolidate the four BioC sub-repos into a single entry are covered
# because both their alias and URL contain "bioc".
is_bioc_repo <- function(repos) {
  stopifnot(is.character(repos) || is.list(repos))
  urls <- unname(unlist(repos, use.names = FALSE))
  nms  <- names(repos)
  if (is.null(nms)) nms <- rep("", length(urls))
  grepl("bioc", nms, ignore.case = TRUE) |
    grepl("bioc", urls, ignore.case = TRUE)
}


# Read `opt_repos` from the effective config unless the caller passed
# one in explicitly. Delegates to val.pipeline::pull_config() so we
# honour the same VAL_PIPELINE_CONFIG / CWD / installed lookup chain
# every other helper uses.
.resolve_opt_repos <- function(opt_repos = NULL, config_path = NULL) {
  if (!is.null(opt_repos)) {
    stopifnot(is.list(opt_repos) || is.character(opt_repos))
    return(as.list(opt_repos))
  }
  cfg <- val.pipeline::pull_config(
    val = "opt_repos",
    rule_type = "default",
    config_path = config_path
  )
  if (is.null(cfg) || length(cfg) == 0L) {
    stop("Could not read `opt_repos` from config; pass `opt_repos = ...` ",
         "explicitly.", call. = FALSE)
  }
  as.list(cfg)
}


# Locate qual_metadata.rds inside a run dir.
.resolve_qual_metadata <- function(val_dir = NULL, qual_metadata = NULL) {
  if (!is.null(qual_metadata)) {
    stopifnot(is.data.frame(qual_metadata))
    return(qual_metadata)
  }
  if (is.null(val_dir)) {
    stop("Provide either `val_dir` or `qual_metadata`.", call. = FALSE)
  }
  qm_path <- file.path(val_dir, "qual_metadata.rds")
  if (!file.exists(qm_path)) {
    stop("No qual_metadata.rds under ", val_dir, call. = FALSE)
  }
  qm <- readRDS(qm_path)
  attr(qm, ".source_path") <- qm_path
  qm
}


build_bioc_blocklist <- function(
    val_dir       = NULL,
    qual_metadata = NULL,
    opt_repos     = NULL,
    config_path   = NULL,
    out_dir       = "dev/",
    tag           = format(Sys.time(), "%Y%m%d_%H%M%S"),
    qualified_decision = "Low",
    write_csv     = TRUE,
    min_universe  = 100L
) {

  qm <- .resolve_qual_metadata(val_dir = val_dir,
                               qual_metadata = qual_metadata)
  key_col <- if ("pkg" %in% names(qm)) "pkg"
             else if ("package" %in% names(qm)) "package"
             else stop("qual_metadata has neither a 'pkg' nor 'package' col.",
                       call. = FALSE)
  if (!"final_decision" %in% names(qm)) {
    stop("qual_metadata is missing 'final_decision'.", call. = FALSE)
  }

  repos_lst <- .resolve_opt_repos(opt_repos = opt_repos,
                                  config_path = config_path)

  bioc_mask <- is_bioc_repo(repos_lst)
  if (!any(bioc_mask)) {
    stop("No BioC repos matched (case-insensitive substring 'bioc' against ",
         "alias + URL). opt_repos was:\n",
         paste0("  ", names(repos_lst), " = ", unlist(repos_lst),
                collapse = "\n"),
         call. = FALSE)
  }
  bioc_repos <- unlist(repos_lst[bioc_mask], use.names = TRUE)

  # Prominent log so the reviewer can eyeball the matched set.
  message("---- BioC repos matched (", length(bioc_repos), ") ----")
  for (i in seq_along(bioc_repos)) {
    message(sprintf("  %-16s -> %s",
                    names(bioc_repos)[i] %||% "", bioc_repos[i]))
  }

  # Pull the universe. type = "source" gives us the widest possible net
  # because every BioC pkg has a source tarball, even ones without a
  # binary build. That's exactly the population PPM needs to gate.
  message("---- Pulling BioC universe via available.packages() ...")
  ap <- available.packages(repos = unname(bioc_repos), type = "source")
  if (nrow(ap) == 0L) {
    stop("available.packages() returned 0 rows for the matched BioC repos. ",
         "This is almost certainly a config/network issue; refusing to ",
         "write an empty blocklist.", call. = FALSE)
  }
  if (nrow(ap) < min_universe) {
    stop("BioC universe has only ", nrow(ap), " packages (< ", min_universe,
         " floor). Refusing to write; check your BioC repo config.",
         call. = FALSE)
  }
  message("     universe size: ", nrow(ap), " unique packages")

  universe <- data.frame(
    package      = unname(ap[, "Package"]),
    version      = unname(ap[, "Version"]),
    matched_repo = unname(ap[, "Repository"]),
    stringsAsFactors = FALSE
  )
  # Trim to unique packages, keeping the first repo we saw them in.
  universe <- universe[!duplicated(universe$package), , drop = FALSE]

  # Identify BioC rows in qm.
  qm_pkg <- qm[[key_col]]
  if ("repo_name" %in% names(qm)) {
    qm_bioc_mask <- grepl("bioc", qm$repo_name, ignore.case = TRUE) &
      !is.na(qm$repo_name)
  } else {
    # No repo_name -> assume every assessed pkg that also appears in
    # the BioC universe belongs to BioC. Coarse, but safe: we only use
    # this to build the allowlist, and any misclassification only
    # under-blocklists (never over-blocklists) BioC pkgs.
    message("     qual_metadata has no 'repo_name'; falling back to ",
            "'assessed AND in-universe'")
    qm_bioc_mask <- qm_pkg %in% universe$package
  }

  bioc_assessed <- qm[qm_bioc_mask, , drop = FALSE]
  message("     BioC assessed by pipeline: ", nrow(bioc_assessed))

  allow_pkgs <- bioc_assessed[[key_col]][
    !is.na(bioc_assessed$final_decision) &
      bioc_assessed$final_decision == qualified_decision
  ]
  allow_pkgs <- unique(allow_pkgs)
  message("     BioC Low-risk allowlist  : ", length(allow_pkgs))

  # Build the blocklist frame.
  blocklist <- universe[!(universe$package %in% allow_pkgs), , drop = FALSE]

  # Reason column: assessed-but-not-Low vs not_assessed.
  assessed_lookup <- setNames(
    as.character(bioc_assessed$final_decision),
    bioc_assessed[[key_col]]
  )
  reason <- ifelse(
    blocklist$package %in% names(assessed_lookup),
    paste0("assessed_", assessed_lookup[blocklist$package]),
    "not_assessed"
  )
  blocklist$reason <- reason

  blocklist <- blocklist[order(blocklist$reason, blocklist$package),
                         c("package", "version", "matched_repo", "reason"),
                         drop = FALSE]
  rownames(blocklist) <- NULL

  message("---- Blocklist size: ", nrow(blocklist), " packages ----")
  reason_tbl <- table(blocklist$reason, useNA = "ifany")
  for (r in names(reason_tbl)) {
    message(sprintf("     %-30s %6d", r, reason_tbl[[r]]))
  }

  # Sanity check: an empty blocklist against a non-empty universe means
  # every BioC pkg is Low-risk, which is implausible.
  if (nrow(blocklist) == 0L) {
    warning("Blocklist is empty. That means every one of the ", nrow(universe),
            " BioC packages is on the Low-risk allowlist -- almost certainly ",
            "a bug in the input qual_metadata. Not writing.")
    return(invisible(list(blocklist = blocklist,
                          universe  = universe,
                          allow     = allow_pkgs,
                          csv_path  = NA_character_)))
  }

  csv_path <- NA_character_
  if (isTRUE(write_csv)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    csv_path <- file.path(out_dir,
                          sprintf("blocklist-BioC-full-%s.csv", tag))
    utils::write.csv(blocklist, csv_path, row.names = FALSE)
    message("---- Wrote ", nrow(blocklist), " rows to ", csv_path)
  }

  invisible(list(
    blocklist    = blocklist,
    universe     = universe,
    allow        = allow_pkgs,
    matched_repos = bioc_repos,
    csv_path     = csv_path
  ))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
