#' Build a complete BioC blocklist against the full available.packages() universe
#'
#' Returns the set of Bioconductor packages that should be blocked
#' from a validated repository (e.g. Posit Package Manager), given a
#' pipeline's `qual_metadata`. The blocklist is computed as
#'
#' \preformatted{
#' blocklist = BioC_universe - Low_risk_allowlist
#'           = (dropped_at_remote_reduce U never_seen_by_pipeline) +
#'             (assessed_but_not_Low)
#' }
#'
#' where `BioC_universe` is pulled live via [utils::available.packages()]
#' against whatever repos in the effective `opt_repos` config look
#' like BioC (case-insensitive substring `"bioc"` match on **both** the
#' repo alias and URL). This intentionally covers non-OSS forks that
#' consolidate the four BioC sub-repos into a single entry, since both
#' the alias and URL contain `"bioc"`.
#'
#' Why this exists: [write_qualified_pkg_lists()]'s legacy behaviour
#' was to write `blocklist-BioC.txt` as the inverse of the
#' *assessed* BioC set only. A large fraction of the BioC universe is
#' dropped at `remote_reduce` and never seen by the pipeline; those
#' packages were absent from both the allowlist and the blocklist,
#' so PPM would silently serve them. This function closes that leak.
#'
#' `write_qualified_pkg_lists(use_full_universe = TRUE)` now calls
#' this internally when writing `blocklist-BioC.txt`. Call this
#' directly if you want the blocklist as a data frame with reason
#' codes for auditing or reporting.
#'
#' @param qual_metadata A `qual_metadata` data frame (as produced by
#'   [val_build()]). Must contain `pkg` (or `package`) and
#'   `final_decision`; `repo_name` is used when present.
#' @param opt_repos A named list/character of repo alias -> URL. If
#'   `NULL` (default), read from the effective config via
#'   [pull_config()].
#' @param config_path Optional path to a `config.yml`. Forwarded to
#'   [pull_config()] when `opt_repos` is `NULL`. Prefer the run's
#'   snapshotted config (e.g. `<val_dir>/config.yml`) when auditing a
#'   historical run so BioC versioning is pinned consistently.
#' @param qualified_decision Character(1). The `final_decision` value
#'   that marks a package as qualified. Defaults to the lowest-risk
#'   decision from the config (typically `"Low"`).
#' @param min_universe Integer(1). Minimum acceptable universe size.
#'   Guards against a config typo silently producing an empty
#'   universe. Defaults to `100L`; real BioC universes are 2k+.
#'
#' @return A data frame with columns `package`, `version`,
#'   `matched_repo`, `reason` (`"not_assessed"` or
#'   `"assessed_<risk>"`), sorted by reason then package. Zero rows
#'   is possible but implausible; a warning is emitted in that case.
#'
#' @seealso [write_qualified_pkg_lists()], [is_bioc_repo()].
#'
#' @examples
#' \dontrun{
#' qm <- readRDS("/data/shared/riskassessments/R_4.5.2/20260730/qual_metadata.rds")
#' bl <- build_bioc_blocklist(
#'   qm,
#'   config_path = "/data/shared/riskassessments/R_4.5.2/20260730/config.yml"
#' )
#' table(bl$reason)
#' }
#'
#' @export
build_bioc_blocklist <- function(
    qual_metadata,
    opt_repos          = NULL,
    config_path        = NULL,
    qualified_decision = NULL,
    min_universe       = 100L
) {
  stopifnot(
    is.data.frame(qual_metadata),
    is.numeric(min_universe), length(min_universe) == 1L, min_universe >= 0L
  )

  # Resolve qualified_decision inside the body so it honours the
  # caller's config_path. Evaluating in the signature default would
  # read the session/installed config regardless — silently wrong if
  # a historical/custom config uses a different first decision.
  if (is.null(qualified_decision)) {
    qualified_decision <- pull_config(val = "decisions_lst",
                                      rule_type = "default",
                                      config_path = config_path)[1]
  }
  stopifnot(is.character(qualified_decision),
            length(qualified_decision) == 1L)

  key_col <- if ("pkg" %in% names(qual_metadata)) "pkg"
             else if ("package" %in% names(qual_metadata)) "package"
             else stop("qual_metadata has neither a 'pkg' nor 'package' col.",
                       call. = FALSE)
  if (!"final_decision" %in% names(qual_metadata)) {
    stop("qual_metadata is missing 'final_decision'.", call. = FALSE)
  }

  repos_lst <- .resolve_opt_repos(opt_repos = opt_repos,
                                  config_path = config_path)

  bioc_mask <- is_bioc_repo(repos_lst)
  if (!any(bioc_mask)) {
    stop(
      "No BioC repos matched (case-insensitive substring 'bioc' against ",
      "alias + URL). opt_repos was:\n",
      paste0("  ", names(repos_lst), " = ", unlist(repos_lst),
             collapse = "\n"),
      call. = FALSE
    )
  }
  bioc_repos <- unlist(repos_lst[bioc_mask], use.names = TRUE)

  val_msg("---- BioC repos matched (", length(bioc_repos), ") ----\n",
          paste(sprintf("  %-16s -> %s",
                        names(bioc_repos), unname(bioc_repos)),
                collapse = "\n"),
          "\n", sep = "", min_level = "normal")

  val_msg("---- Pulling BioC universe via available.packages() ...\n",
          min_level = "normal")
  # Pinned to type = "source" because every BioC pkg has a source
  # tarball even without a binary build — widest possible net for the
  # PPM gating population.
  ap <- utils::available.packages(repos = unname(bioc_repos), type = "source")
  if (nrow(ap) == 0L) {
    stop("available.packages() returned 0 rows for the matched BioC repos. ",
         "This is almost certainly a config/network issue; refusing to ",
         "produce an empty blocklist.", call. = FALSE)
  }
  if (nrow(ap) < min_universe) {
    stop("BioC universe has only ", nrow(ap), " packages (< ", min_universe,
         " floor). Refusing to produce; check your BioC repo config.",
         call. = FALSE)
  }
  val_msg("     universe size: ", nrow(ap), " unique packages\n",
          min_level = "normal")

  universe <- data.frame(
    package      = unname(ap[, "Package"]),
    version      = unname(ap[, "Version"]),
    matched_repo = unname(ap[, "Repository"]),
    stringsAsFactors = FALSE
  )
  universe <- universe[!duplicated(universe$package), , drop = FALSE]

  # BioC rows in qm: union of two paths.
  # (a) repo_name matches an alias in bioc_repos — catches URL-only
  #     matches like alias 'sci' -> bioc URL. Substring-on-name alone
  #     would miss those.
  # (b) repo_name contains "bioc" case-insensitively — catches
  #     riskmetric-derived labels like 'BioCsoft' that don't literally
  #     equal the config alias.
  qm_pkg <- qual_metadata[[key_col]]
  if ("repo_name" %in% names(qual_metadata)) {
    rn <- qual_metadata$repo_name
    qm_bioc_mask <- !is.na(rn) &
      (rn %in% names(bioc_repos) | grepl("bioc", rn, ignore.case = TRUE))
  } else {
    val_msg("     qual_metadata has no 'repo_name'; falling back to ",
            "'assessed AND in-universe'\n", min_level = "normal")
    qm_bioc_mask <- qm_pkg %in% universe$package
  }

  bioc_assessed <- qual_metadata[qm_bioc_mask, , drop = FALSE]
  val_msg("     BioC assessed by pipeline: ", nrow(bioc_assessed), "\n",
          min_level = "normal")

  allow_pkgs <- unique(bioc_assessed[[key_col]][
    !is.na(bioc_assessed$final_decision) &
      bioc_assessed$final_decision == qualified_decision
  ])
  val_msg("     BioC ", qualified_decision, "-risk allowlist  : ",
          length(allow_pkgs), "\n", min_level = "normal")

  blocklist <- universe[!(universe$package %in% allow_pkgs), , drop = FALSE]

  assessed_lookup <- stats::setNames(
    as.character(bioc_assessed$final_decision),
    bioc_assessed[[key_col]]
  )
  blocklist$reason <- ifelse(
    blocklist$package %in% names(assessed_lookup),
    paste0("assessed_", assessed_lookup[blocklist$package]),
    "not_assessed"
  )

  blocklist <- blocklist[order(blocklist$reason, blocklist$package),
                         c("package", "version", "matched_repo", "reason"),
                         drop = FALSE]
  rownames(blocklist) <- NULL

  if (nrow(blocklist) == 0L) {
    warning(
      "Blocklist is empty. Every one of the ", nrow(universe),
      " BioC packages is on the Low-risk allowlist -- almost certainly ",
      "a bug in the input qual_metadata.",
      call. = FALSE
    )
  }

  blocklist
}


#' Detect BioC entries in a named repos vector or list
#'
#' Case-insensitive substring match on `"bioc"` against **both** the
#' repo alias (`names(repos)`) and the URL (values). The dual-side
#' match handles two real-world cases the OSS `BioC` alias alone
#' would miss:
#'
#' - Non-OSS forks that consolidate the four BioC sub-repos
#'   (`BioCsoft`, `BioCann`, `BioCexp`, `BioCwork`) into a single
#'   entry whose alias and URL both contain `"bioc"`.
#' - URL-only aliases (e.g. an internal mirror named `sci` whose URL
#'   is a Bioconductor path).
#'
#' Used by [build_bioc_blocklist()] and by
#' [write_qualified_pkg_lists()] when `use_full_universe = TRUE`.
#'
#' @param repos Named character vector or list of `alias -> URL`.
#' @return Logical vector the same length as `repos`.
#' @examples
#' is_bioc_repo(c(CRAN = "https://cran.rstudio.com",
#'                BioC = "https://bioconductor.org/packages/3.22/bioc"))
#' @export
is_bioc_repo <- function(repos) {
  stopifnot(is.character(repos) || is.list(repos))
  urls <- unname(unlist(repos, use.names = FALSE))
  nms  <- names(repos)
  if (is.null(nms)) nms <- rep("", length(urls))
  grepl("bioc", nms, ignore.case = TRUE) |
    grepl("bioc", urls, ignore.case = TRUE)
}


# Read opt_repos from the effective config unless the caller passed
# one in explicitly.
.resolve_opt_repos <- function(opt_repos = NULL, config_path = NULL) {
  if (!is.null(opt_repos)) {
    stopifnot(is.list(opt_repos) || is.character(opt_repos))
    return(as.list(opt_repos))
  }
  cfg <- pull_config(
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
