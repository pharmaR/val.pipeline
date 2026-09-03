
#' Validation: Assess a Package
#'
#' Validation process at the package level. Includes steps to download the
#' package source (preferred), install the package, assess the package using the
#' user-specified metric package (only `riskmetric` is supported currently),
#' apply risk decisions, and build a report. Note: to save time, every
#' package will be assessed using a "pkg_cran_remote" reference initially to see
#' if any primary metrics met the "auto-accept" threshold(s), if applicable. If
#' they did, the 'covr_coverage' computation will be skipped.
#'
#' @param pkg Character(1). Name of package to validate.
#' @param ver Character(1). Version of package to validate.
#' @param avail_pkgs Data frame. Output of `available.packages()`.
#' @param ref Character(1). Either "source" or "remote". Indicates whether to
#'   download the package source from CRAN (or other repo) or to install the
#'   package directly from the repo.
#' @param metric_pkg Character(1). Either "riskmetric", "val.meter", or
#'   "risk.assessr" indicating which package to use for the assessment.
#' @param out_dir Character(1). Directory to store outputs.
#' @param val_date Date. Date of validation. Default is current date.
#' @param verbose Console verbosity control. One of `"quiet"`,
#'   `"minimal"`, `"normal"` (default), or `"verbose"`. See
#'   the `val.pipeline` verbosity docs for tier definitions. Defaults to whatever the
#'   session option `val.pipeline.verbose` is set to (or `"normal"`).
#' @param pkg_idx,pkg_total Integer(1) or `NULL`. Optional
#'   position-in-run counter surfaced on the `verbose = "minimal"`
#'   summary line as `"(<pkg_idx>/<pkg_total>)"`. Populated
#'   automatically when `val_pkg()` is called from inside
#'   [val_build()]'s per-package loop; leave as `NULL` when calling
#'   `val_pkg()` standalone.
#'
#' @importFrom glue glue
#' @importFrom utils download.file untar capture.output
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score all_assessments
#' @importFrom riskreports package_report
#' @importFrom dplyr filter pull select arrange as_tibble
#' @importFrom tools package_dependencies
#' @importFrom withr with_envvar
#'
#' @return A list containing package metadata, assessment results, and
#'   decisions.
#' @export
#' 
val_pkg <- function(
    pkg,
    ver,
    avail_pkgs,
    ref = c("source", "remote"),
    metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
    out_dir,
    val_date = Sys.Date(),
    verbose = NULL,
    pkg_idx = NULL,
    pkg_total = NULL
) {
  # i <- 1 # for debugging
  # pkg <- pkgs[i] # for debugging
  # ver <- vers[i] # for debugging
  
  # assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  apply_verbose(verbose)
  reset_pkg_timings()
  
  pkg_v <- paste(pkg, ver, sep = "_")
  start <- Sys.time()
  start_txt <- format(start, '%H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  val_msg(paste0("\nNew Package: ", pkg, " v", ver," @ ", start_txt,"\n"),
          min_level = "normal")
  
  #
  # ---- Setup ----
  #
  
  # Dirs
  if(ref == "source"){
    tarballs <- file.path(out_dir, 'tarballs')
    sourced <- file.path(out_dir, 'sourced')
    if(!dir.exists(tarballs)) dir.create(tarballs)
    if(!dir.exists(sourced)) dir.create(sourced)
  }
  
  installed <- file.path(out_dir, 'installed')
  assessed <- file.path(out_dir, 'assessed')
  reports <- file.path(out_dir, 'reports')
  if(!dir.exists(installed)) dir.create(installed)
  if(!dir.exists(assessed)) dir.create(assessed)
  if(!dir.exists(reports)) dir.create(reports)
  
  # Where did package come from?
  repo_src_contrib <- avail_pkgs |>
    dplyr::filter(Package %in% pkg) |> 
    dplyr::pull(Repository)  # keep '/src/contrib/` ending
  repo_src <- repo_src_contrib |> dirname() |> dirname() # trim off '/src/contrib'
  repo_name <- get_repo_origin(repo_src = repo_src, pkg_name = pkg)
  # Plain-string label for the source repo (e.g. "CRAN", "BioC",
  # "github_pharmaverse", or "unknown"). Persisted into meta_list below
  # so downstream consumers of qual_metadata.rds (notably
  # write_qualified_pkg_lists()) don't have to re-derive the source by
  # matching URLs against the current session's repos option.
  repo_label <- get_repo_origin(repo_src = repo_src, pkg_name = pkg,
                                names_only = TRUE)
  
  # Decisions
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  
  if(ref == "source") {
    
    #
    # ---- Download Tarball ----
    #
    tarball_url <- file.path(repo_src_contrib, paste0(pkg_v,".tar.gz"))
    dwn_ld <- val_time_block("download",
      try(utils::download.file(tarball_url,
                               file.path(tarballs, basename(tarball_url)),
                               quiet = TRUE, mode = "wb"),
          silent = TRUE))
    if (inherits(dwn_ld, "try-error") | dwn_ld != 0) {
      wrn_msg <- glue::glue("Unable to download the source files for {pkg} from '{tarball_url}'.")
      warning(wrn_msg)
    } else {
      val_msg("\n-->", pkg_v,"downloaded.\n", min_level = "verbose")
    }
    
    #
    # ---- Untar ---- 
    #
    tar_file <- file.path(tarballs, glue::glue("{pkg_v}.tar.gz"))
    val_time_block("untar", utils::untar(tar_file, exdir = sourced))
    val_msg("\n-->", pkg_v,"untarred.\n", min_level = "verbose")
  }
  
  
  #
  # ---- Grab Dependencies ----
  #
  
  # Grab deps: Depends, Imports, LinkingTo, Suggests
  # pkg_base <- avail_pkgs |> dplyr::filter(Package %in% pkg)
  
  # grab depends
  depends <- 
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = c("Depends", "Imports", "LinkingTo"),
      recursive = TRUE
    ) |>
    unlist(use.names = FALSE) 
  
  # grab suggests
  suggests <- 
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = "Suggests",
      recursive = TRUE # this really blows up for almost any pkg
    ) |>
    unlist(use.names = FALSE) 

  # Direct (non-recursive) deps: what's declared in this package's own
  # DESCRIPTION. Used to populate `decision_reason_note` so a
  # dep-driven downgrade names a real DESCRIPTION-level dep instead of
  # a transitive package pulled from the recursive Suggests closure
  # (which for most packages is 1000-2000 pkgs long). See #107.
  depends_direct <-
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = c("Depends", "Imports", "LinkingTo"),
      recursive = FALSE
    ) |>
    unlist(use.names = FALSE)

  suggests_direct <-
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = "Suggests",
      recursive = FALSE
    ) |>
    unlist(use.names = FALSE)


  #
  # ---- Assess ---- 
  #
  
  #
  #### riskmetric ####
  #
  if(metric_pkg == "riskmetric"){
    
    #
    # Update: can't remove these - will ruin some metrics like
    # code coverage & R CMD Check
    #
    # Remove original docs, if they exist, because they force a
    # user prompt when run interactively
    # inst_doc <- file.path(sourced, pkg, "inst", "doc")
    # if(dir.exists(inst_doc) & interactive()) {
    #   unlink(inst_doc, recursive = TRUE, force = TRUE)
    #   cat("\n-->", pkg_v,"removed original inst/doc.\n")
    # }
    
    #
    ### Initial Assessment ###
    #

    # For BioC packages, an initial `pkg_bioc_remote` assessment scrapes
    # the Bioconductor package HTML landing pages, NEWS pages and
    # checkResults pages under bioconductor.org. Air-gapped PPM mirrors
    # do not surface those paths (see dev/air-gapped-bioc-mirror-config.txt),
    # so a family of primary metrics (`has_vignettes`, `has_news`,
    # `license`, `has_maintainer`, `has_bug_reports_url`,
    # `has_source_control`) collapse to NA and the resulting `auto_accepted`
    # signal is almost always FALSE. That defeats the whole point of the
    # initial pass (short-circuiting `covr_coverage` on genuinely low-risk
    # packages) *and* misrepresents the package in a "remote" report.
    #
    # For BioC packages we therefore prefer a disk-only initial ref that
    # riskmetric can service without any HTML scraping: `pkg_install` when
    # the package is present in the pipeline's library (the common case:
    # val_prep_pipeline() has already staged it into .libPaths()[1]),
    # falling back to `pkg_source` when the tarball has been untarred but
    # the package hasn't been installed yet. If neither is available the
    # initial pass is skipped entirely; the final pkg_source assessment
    # then always runs with `covr_coverage` included.
    bioc_pkg <- isTRUE(stringr::str_detect(tolower(repo_name), "bioc"))
    bioc_initial_ref <- if (bioc_pkg) {
      knob <- tryCatch(
        pull_config(val = "bioc_initial_ref", rule_type = "default"),
        error = function(e) NULL
      )
      if (is.null(knob) || !nzchar(as.character(knob))) "install" else as.character(knob)
    } else NA_character_

    installed_here <- if (bioc_pkg) {
      pkg %in% rownames(utils::installed.packages(lib.loc = .libPaths()[1]))
    } else FALSE
    source_here <- if (bioc_pkg && ref == "source") {
      dir.exists(file.path(sourced, pkg))
    } else FALSE

    init_source <- if (!bioc_pkg) {
      "pkg_cran_remote"
    } else if (identical(bioc_initial_ref, "skip")) {
      NA_character_
    } else if (identical(bioc_initial_ref, "remote")) {
      "pkg_bioc_remote"
    } else if (identical(bioc_initial_ref, "source") && source_here) {
      "pkg_source"
    } else if (installed_here) {
      "pkg_install"
    } else if (source_here) {
      "pkg_source"
    } else {
      NA_character_
    }

    do_init <- !is.na(init_source)

    if (do_init) {
      init_pkg_ref <- switch(
        init_source,
        pkg_install = riskmetric::pkg_ref(
          pkg, source = "pkg_install", lib.loc = .libPaths()[1]
        ),
        pkg_source  = riskmetric::pkg_ref(
          file.path(sourced, pkg), source = "pkg_source"
        ),
        riskmetric::pkg_ref(pkg, source = init_source)
      )
      val_msg("\n-->", pkg_v, "initial reference complete (",
              init_source, ").\n",
              min_level = "normal")

      # Pull available {riskmetric} assessments
      init_metrics <- riskmetric::all_assessments()

      # if it's a 'remote_only' pkg, and we only want to assess primary metrics,
      # then we could do that here (below). For now, we'll leave it all since
      # we'll want a report that is the most populated as possible
      # remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
      # if(pkg %in% remote_pkgs) {
      #   # pull primary metrics only
      #   prime_metrics <- build_decisions_df(rule_type = "decide") |>
      #     dplyr::filter(tolower(metric_type) == "primary") |>
      #     dplyr::pull(metric) |>
      #     unique() %>%
      #     paste("assess", ., sep = "_")
      #   init_metrics <- init_metrics[names(init_metrics) %in% prime_metrics]
      # }

      # covr_coverage is expensive; keep it off the initial pass. It runs in
      # the final pkg_source assessment when auto_accepted is FALSE.
      init_metrics$assess_covr_coverage <- NULL

      # When the initial pass is `pkg_bioc_remote`, most riskmetric
      # assessments scrape bioconductor.org via `x$web_html` and return
      # `pkg_metric_error` on air-gapped hosts even with the offline
      # shims in place (the shims fix classification + the Repository
      # URL, but PPM BioC mirrors typically do not serve the `/html/`,
      # `/news/`, `/checkResults/` trees). Restrict the initial pass to
      # a config-defined whitelist so we only spend cycles on metrics
      # that will actually produce a usable score. Set
      # `default: bioc_remote_initial_metrics: ~` (or omit the key) in
      # config.yml to opt out of the whitelist and run every metric.
      if (identical(init_source, "pkg_bioc_remote")) {
        safe_metrics <- pull_config(
          val = "bioc_remote_initial_metrics",
          rule_type = "default"
        )
        if (!is.null(safe_metrics) && length(safe_metrics) > 0) {
          keep <- names(init_metrics) %in% safe_metrics
          if (any(keep)) {
            init_metrics <- init_metrics[keep]
            val_msg(
              "--> ", pkg_v,
              " initial pkg_bioc_remote pass restricted to ",
              length(init_metrics), " metric(s): ",
              paste(names(init_metrics), collapse = ", "), "\n",
              min_level = "normal"
            )
          } else {
            warning(
              "bioc_remote_initial_metrics did not match any assessments ",
              "returned by riskmetric::all_assessments(); falling back ",
              "to the full metric set for ", pkg, ".",
              call. = FALSE
            )
          }
        }
      }

      init_pkg_assessment0 <- val_time_block("assess_initial",
        init_pkg_ref |>
          # dplyr::as_tibble() |> # no tibbles allowed for stip or riskreports
          riskmetric::pkg_assess(assessments = init_metrics))

      # strip assessment of '.recording' attribute:
      init_pkg_assessment <-
        init_pkg_assessment0 |>
        strip_recording()

      init_pkg_scores <- riskmetric::pkg_score(init_pkg_assessment)

      init_assessed_end <- Sys.time()
      init_ass_mins <- difftime(init_assessed_end, start, units = "mins")
      init_ass_mins_txt <- utils::capture.output(init_assessed_end - start)
      val_msg("\n-->", pkg_v, "initial assessment complete.\n",
              min_level = "normal")
      val_msg("----> (", init_ass_mins_txt, ")\n", min_level = "normal")


      # Create workable DF of assessments. `source_ref` records the
      # provenance of the *initial* pass rather than always claiming
      # "remote" -- BioC pkgs will read "install" or "source" now.
      init_source_ref <- switch(init_source,
        pkg_install = "install",
        pkg_source  = "source",
        "remote"
      )
      init_assessment_record <- workable_assessments(
        pkg = pkg,
        ver = ver,
        val_date = val_date,
        metric_pkg = metric_pkg,
        source = list(assessment = init_pkg_assessment, scores = init_pkg_scores),
        source_ref = init_source_ref
      )

      #
      #### Initial Decision
      #
      init_viable_metrics <- init_pkg_scores |>
        dplyr::as_tibble() |>
        t() |>
        as.data.frame() |>
        dplyr::filter(!is.na(V1)) |>
        # make rownames a column
        tibble::rownames_to_column(var = "metric") |>
        dplyr::pull(metric)

      if("r_cmd_check" %in% init_viable_metrics){
        init_vm <- init_viable_metrics[which(init_viable_metrics != "r_cmd_check")]
        init_viable_metrics <- c(init_vm, "r_cmd_check_warnings", "r_cmd_check_errors")
      }

      init_decision <-
        val_decision(
          pkg = pkg,
          source_df = init_assessment_record,
          excl_metrics = NULL, # "covr_coverage", # Subset not really necessary
          decisions = decisions,
          else_cat = decisions[length(decisions)],
          decisions_df = build_decisions_df(
            rule_type = "decide",
            # rule_type = "remote_reduce",  # Could use this one here.
            viable_metrics = init_viable_metrics
            )
        )

      auto_accepted <-
        init_decision |>
        # dplyr::select(package, final_risk, dplyr::ends_with("cataa"))
        dplyr::select(dplyr::ends_with("cataa")) |>
        as.vector() |> unlist() |> any()

      # Should I also consider an auto_fail threshold?
    } else {
      # BioC pkg with no local install or source available (or user asked
      # for "skip"). Skip the initial pass; the final pkg_source assessment
      # below will always include covr_coverage.
      val_msg("\n-->", pkg_v,
              "skipped initial assessment (BioC pkg, no local install/source or bioc_initial_ref='skip').\n",
              min_level = "normal")
      init_pkg_ref        <- NULL
      init_pkg_assessment <- NULL
      init_pkg_scores     <- NULL
      init_assessment_record <- NULL
      init_decision       <- NULL
      init_viable_metrics <- character(0)
      auto_accepted       <- FALSE
      init_ass_mins       <- as.difftime(0, units = "mins")
    }
    
    
    #
    #### Final Assessment ###
    #
    src_ref <- if(ref == "source") 'pkg_source' else 'pkg_cran_remote'
    # Reuse the initial assessment as the final one when the user asked
    # for a remote-only run AND the initial pass actually happened AND
    # it wasn't a stand-in for a source assessment (e.g. BioC + pkg_install
    # or pkg_source used as the initial). Otherwise fall through to a
    # full pkg_source pass.
    reuse_init <- src_ref == "pkg_cran_remote" &&
                  do_init &&
                  !identical(init_source, "pkg_source")

    # Default: no skip report captured. Overwritten below when the
    # non-auto-accept branch runs `assess_covr_coverage` and we
    # capture the paired testthat skip report (#150). Kept declared
    # here at outer scope so the `meta_list` construction downstream
    # can reference `covr_skip_report` without a branch-order
    # dependency.
    covr_skip_report <- NULL

    if (reuse_init) {
      pkg_assessment <- init_pkg_assessment
      pkg_scores <- init_pkg_scores
      val_msg("\n-->", pkg_v, "used initial '", init_source,
              "' assessment as the final remote result.\n",
              min_level = "normal")
      exclude_met <- NULL
    } else {
      
      # Setup 'pkg_source' reference
      pkg_ref <- riskmetric::pkg_ref(file.path(sourced, pkg), source = "pkg_source")
      val_msg("\n-->", pkg_v,"referrenced w/ 'pkg_source'.\n",
              min_level = "normal")
      
      # Pull available {riskmetric} assessments
      assess_metrics <- riskmetric::all_assessments()
      
      if (auto_accepted) {
        # Run assessment WITHOUT "covr_coverage"!
        val_msg("\n-->", pkg_v, "auto-accepted. Will compile final 'pkg_source' assessment w/o 'covr_coverage' metric to save compute time.\n",
                min_level = "normal")
        assess_metrics$assess_covr_coverage <- NULL
        exclude_met <- "covr_coverage"
        
      } else {
        # Run assessment WITH "covr_coverage"
        val_msg("\n-->", pkg_v, "was NOT auto-accepted. Will compile final 'pkg_source' assessment, including 'covr_coverage' metric.\n",
                min_level = "normal")
        exclude_met <- NULL
      }
      
      pkg_assessment0 <- val_time_block("assess_final",
        withr::with_envvar(
          # Layer A env-var normalization for `assess_covr_coverage`
          # (issue #146). `covr::package_coverage()` does not set
          # NOT_CRAN internally — verified against the installed covr
          # namespace — so `testthat::skip_on_cran()` fires during our
          # coverage runs and silently drops a large slice of many
          # packages' test suites. The block is scoped via
          # `with_envvar()` so it never leaks into the parent R
          # session, and it's applied to the *final* pkg_assess() only:
          # the initial pass already excludes `assess_covr_coverage`.
          # Applying it to all metrics in the final pass (rather than
          # just covr_coverage) is deliberate — the env vars are
          # harmless to the other metrics, and this avoids the need to
          # split the pkg_assess() call into two runs.
          new = pull_covr_env_vars(),
          code = pkg_ref |>
            # dplyr::as_tibble() |> # no tibbles allowed for stip or riskreports
            riskmetric::pkg_assess(assessments = assess_metrics)
        ))
      
      # strip assessment of '.recording' attribute:
      pkg_assessment <-  pkg_assessment0 |> 
        strip_recording()
      
      pkg_scores <- riskmetric::pkg_score(pkg_assessment)
      
      # Clean up any new folders created in the working directory that end in '-tests'
      # this is an unfortunate by-produce of riskmetric's processes
      wd_dirs <- list.dirs(getwd(),recursive = FALSE)
      # Find which dirs end in "-test"
      pkg_test_dir <- wd_dirs[grepl("-tests$", wd_dirs)]
      unlink(pkg_test_dir, recursive = TRUE, force = TRUE)

      # Capture a testthat skip report (issue #150). Standalone
      # testthat::test_dir() run under the same env-var block as covr
      # so the skip population matches what `assess_covr_coverage`
      # actually saw. `covr::package_coverage()` runs test files via
      # `sys.source()` — bypassing testthat's reporter chain — so the
      # skip counts are unrecoverable from the covr result itself.
      #
      # Gating: `covr_skip_report:` config block + optional
      # `capture_covr_skip_report` / `covr_skip_report_threshold`
      # overrides from val_pipeline(). `!auto_accepted` is a hard
      # prerequisite (this branch is the one that actually ran
      # `assess_covr_coverage`); inside that population:
      #   * `capture == FALSE`  — skip entirely (fastest).
      #   * `threshold` numeric — capture only when the raw covr
      #     coverage came in below `threshold` (0-100 scale,
      #     default 65 matching the covr_coverage Medium/Low
      #     cutoff). Set threshold to 100 in the arg / config to
      #     capture for every non-auto-accept pkg.
      # See #150.
      skip_cfg <- pull_covr_skip_report_config()
      # Safe covr_coverage lookup — see pkg_assessment_covr_pct()
      # for the vctrs-strict-subset rationale (#161).
      coverage_val <- pkg_assessment_covr_pct(pkg_assessment)
      should_capture <- !auto_accepted && skip_cfg$capture &&
        isTRUE(is.finite(coverage_val) &&
               coverage_val < skip_cfg$threshold)
      if (should_capture && pkg %in% skip_cfg$skip_pkgs) {
        # Belt-and-suspenders: subprocess isolation in
        # `capture_covr_skip_report()` already prevents a crashing
        # child from taking down the worker, but skipping the
        # child spin-up + full test_dir() walk on packages known to
        # crash is still meaningfully cheaper. See #159.
        val_msg(paste0("\n-->", pkg_v,
          " skipping covr_skip_report (pkg on config ",
          "`covr_skip_report$skip_pkgs` list).\n"),
          min_level = "normal")
        should_capture <- FALSE
      }
      if (should_capture) {
        val_msg(paste0("\n-->", pkg_v,
          " capturing covr_skip_report (coverage=",
          if (is.finite(coverage_val)) formatC(coverage_val, digits = 1,
                                               format = "f") else "NA",
          ", threshold=", skip_cfg$threshold, ")\n"),
          min_level = "normal")
        covr_skip_report <- val_time_block("skip_report",
          capture_covr_skip_report(
            pkg_source_path = file.path(sourced, pkg),
            env_vars        = pull_covr_env_vars()
          )
        )
        if (!is.null(covr_skip_report)) {
          attr(pkg_assessment, "covr_skip_report") <- covr_skip_report
        }
      }
    }
    
  
  #
  #### risk.assessr ####
  #
  } 
  # else if(metric_pkg == "risk.assessr") {
  #   requireNamespace("risk.assessr", quietly = TRUE)
  #   if(ref == "source") {
  #     pkg_assessment <- risk.assessr::risk_assess_pkg(tar_file)
  #     # names(pkg_assessment)
  #     # pkg_assessment$results
  #     # pkg_assessment$results$dep # doesn't mention lattice...
  #     # (pkg_assessment$results$download$last_month_download * 12) |> prettyNum(big.mark = ",")
  #     # pkg_assessment$covr_list # failed
  #     # pkg_assessment$tm        # failed
  #     # pkg_assessment$check_list # appears to depend on pkgs installed locally
  #   } else {
  #     pkg_assessment <- risk.assessr::assess_pkg_r_package(pkg, ver)
  #     # names(pkg_assessment0)
  #     # pkg_assessment0$results
  #     # pkg_assessment0$results$dep 
  #     # (pkg_assessment0$results$download$last_month_download * 12) |> prettyNum(big.mark = ",")
  #     # pkg_assessment0$covr_list 
  #     # pkg_assessment0$tm        
  #     # pkg_assessment0$check_list 
  #   }
  #   
  # #
  # #### val.meter ####
  # #
  # } else if(metric_pkg == "val.meter") {
  #   stop("Not yet implemented: val_pkg() using 'val.meter' tooling")
  # } # no else since we assert metric_pkg values at top of val_pkg().
  
  
  assessed_end <- Sys.time()
  ass_mins <- difftime(assessed_end, start, units = "mins")
  ass_mins_txt <- utils::capture.output(assessed_end - start)
  val_msg("\n-->", pkg_v,"assessed.\n", min_level = "normal")
  val_msg("----> (", ass_mins_txt, ")\n", min_level = "normal")
  
  # Create workable DF of assessments
  assessment_record <- workable_assessments(
    pkg = pkg,
    ver = ver,
    val_date = val_date,
    metric_pkg = metric_pkg,
    source = list(assessment = pkg_assessment, scores = pkg_scores),
    source_ref = ref
  )
  
  #
  # ---- Save Assessment artifacts ---- 
  #
  
  assess_record_file <- file.path(assessed, glue::glue("{pkg_v}_assess_record.rds"))
  assessment_file <- file.path(assessed, glue::glue("{pkg_v}_assessments.rds"))
  scores_file <- file.path(assessed, glue::glue("{pkg_v}_scores.rds"))
  
  # assessment_record <- readRDS(assess_record_file) # for debugging
  # pkg_assessment <- readRDS(assessment_file) # for debugging
  # pkg_scores <- readRDS(scores_file) # for debugging
  saveRDS(assessment_record, assess_record_file)
  saveRDS(pkg_assessment, assessment_file)
  saveRDS(pkg_scores, scores_file)
  # cat("\n-->", pkg_v,"assessments & scores saved.\n")
  
  
  
  #
  # ---- Apply Decisions ----
  #
  
  val_msg("\n--> Making a risk decision for", pkg_v,"...\n\n",
          min_level = "verbose")
  
  # Use org-level criterion to set thresholds and Update final decision (if not
  # already 'high risk') AND then filter packages to a final 'qualified' list
  #
  # Note: this needs to happen again because (1) we don't have metrics like
  # 'covr_coverage' represented in our pre-filter, plus with have other
  # non-riskmetric assessments, like 'installed_cleanly' we need to consider.
  # (2) Secondly, because val_filter() (our pre-filtering engine) wasn't run on
  # the intended system (aka, {riskscore} OR the PACKAGES) file, so we have to
  # run val_build() & re-filter.
  
  # if need to read in an assessment:
  # pkg_assessment <- readRDS(assessment_file)
  # pkg_scores <- readRDS(scores_file)
  
  # riskmetric doesn't pick up certain metrics for pkg_ref(source = "pkg_cran_remote")
  # What metrics do we need to remove for the decisioning process?
  viable_metrics <- pkg_scores |>
    dplyr::as_tibble() |>
    t() |>
    as.data.frame() |>
    dplyr::filter(!is.na(V1)) |>
    # make rownames a column
    tibble::rownames_to_column(var = "metric") |>
    dplyr::pull(metric)
  
  if("r_cmd_check" %in% viable_metrics){
    vm <- viable_metrics[which(viable_metrics != "r_cmd_check")]
    viable_metrics <- c(vm, "r_cmd_check_warnings", "r_cmd_check_errors")
  }
  
  decision <- val_time_block("decision",
    val_decision(
      pkg = pkg,
      source_df = assessment_record,
      excl_metrics = exclude_met, # Subset if desired
      decisions = decisions,
      else_cat = decisions[length(decisions)],
      avail_pkgs = avail_pkgs,
      decisions_df = build_decisions_df(
        rule_type = "decide",
        viable_metrics = viable_metrics
        )
    ))
  decision_aa <- decision |>
    dplyr::select(dplyr::ends_with("cataa")) |>
    as.vector() |> unlist() |> any()

  # Initialize `aa_metrics` unconditionally so the eager-evaluated
  # `dplyr::case_when()` RHS below (see `decision_reason_note`) can
  # always reference it even when no auto-accept threshold matched
  # (i.e. when `decision_aa` is FALSE). Without this, packages that
  # don't auto-accept trigger:
  #   Error in dplyr::case_when(): object 'aa_metrics' not found
  # because case_when() evaluates every RHS expression before
  # selecting which one to use.
  aa_metrics <- character(0)

  if(decision_aa) {
    approved_pkgs <- pull_config(val = "approved_pkgs", rule_type = "default")
    aa_metrics <- decision |>
      dplyr::select(dplyr::ends_with("cataa")) |>
      names() %>%
      gsub("_cataa", "", .)

    decision_reason <- dplyr::case_when(
      pkg %in% approved_pkgs ~ "Pre-Approved package",
      length(aa_metrics) > 0 ~ "Auto-Accepted",
      TRUE ~ "Risk Assessment"
    ) 
  } else {
    decision_reason <- "Risk Assessment"
  }

  # Populate decision_reason_note with the specific metrics that drove the
  # decision, depending on which decision_reason applies:
  # - "Auto-Accepted": the metric(s) whose auto_accept condition matched
  # - "Risk Assessment": the metric(s) whose per-metric `_cat` matched the
  #   final risk (only when the package landed above the lowest-risk tier)
  # - "Pre-Approved package" / "Dependency" / other: NA here (Dependency
  #   note is populated downstream in val_build.R / reject_iteration()).
  decision_reason_note <- dplyr::case_when(
    identical(decision_reason, "Auto-Accepted") ~
      paste(aa_metrics, collapse = ", "),
    identical(decision_reason, "Risk Assessment") ~
      extract_risk_drivers(decision, decisions = decisions),
    .default = NA_character_
  )

  # Silent-NA capture. val_decision() can return final_risk = NA when
  # its rule ladder produces no category for this pkg — typically a
  # remote_only pkg whose shrunken viable-metric set means the primary
  # rules score `unknown` AND the secondary rules also don't match.
  # Preserve `decision = NA` per operator preference (don't silently
  # coerce to a tier), but overwrite decision_reason with a distinct
  # tag ("Incomplete Assessment") and stash an `assessment_gaps` list
  # so the summary report can surface which metrics were viable,
  # which categories fired, and why the ladder produced nothing. See
  # #124.
  assessment_gaps <- NULL
  if (is.na(decision$final_risk)) {
    metric_cat_cols <- names(decision)[
      grepl("_cat$", names(decision), perl = TRUE) &
      !grepl("cataa$", names(decision), perl = TRUE)
    ]
    metric_cats <- if (length(metric_cat_cols) > 0L) {
      vapply(metric_cat_cols,
             function(c) as.character(decision[[c]][1]),
             character(1))
    } else {
      character(0)
    }
    prim_cat <- if ("primary_risk_category" %in% names(decision)) {
      as.character(decision$primary_risk_category[1])
    } else {
      NA_character_
    }
    sec_cat <- if ("secondary_risk_category" %in% names(decision)) {
      as.character(decision$secondary_risk_category[1])
    } else {
      NA_character_
    }
    note <- if (identical(prim_cat, "unknown") &&
                  (is.na(sec_cat) || identical(sec_cat, "unknown"))) {
      paste0("Primary rule ladder scored 'unknown' and secondary ",
             "rule ladder also produced no category. Typically a ",
             "remote_only / Bioc pkg whose viable-metric set is too ",
             "thin to trigger any rule.")
    } else if (identical(prim_cat, "unknown")) {
      paste0("Primary rule ladder scored 'unknown'; ",
             "secondary rule ladder produced no matching category.")
    } else {
      paste0("Rule ladder produced no matching category ",
             "(primary = '", prim_cat, "', ",
             "secondary = '", sec_cat, "').")
    }
    assessment_gaps <- list(
      viable_metrics          = viable_metrics,
      metric_cats             = metric_cats,
      primary_risk_category   = prim_cat,
      secondary_risk_category = sec_cat,
      note                    = note
    )
    decision_reason      <- "Incomplete Assessment"
    decision_reason_note <- note
  }

  val_msg("\n-->", pkg_v,"decision reason:\n---->", decision_reason, "\n",
          min_level = "normal")
  if(!is.na(decision_reason_note)) {
    val_msg("---->", pkg_v, "driver metric(s):", decision_reason_note, "\n",
            min_level = "normal")
  }
  
  
  
  #
  # ---- Build Report ----
  #

  # file.edit(system.file("report/package/pkg_template.qmd", package = "riskreports"))
  # `riskreports::package_report()` copies its template files into
  # `options("riskreports_output_dir")`, `file.rename()`s one file to a
  # pkg-specific `prefix_output`, then `file.remove()`s the leftovers.
  # When multiple `val_pkg()` calls run in parallel (workers > 1 in
  # val_build()) they all reach for the same shared `reports/`
  # directory and race on the mid-flight copy/rename/remove sequence,
  # so a sibling worker's `quarto::quarto_render()` blows up mid-flight
  # with 'Error running quarto CLI from R'. Give each package its own
  # scratch render directory so template files don't collide, then
  # copy the produced output(s) back into the shared `reports/`
  # afterwards. The parent workflow already indexes reports by their
  # pkg/version filename, so the final layout is unchanged.
  #
  # Also use a per-invocation unique subdir (via `tempfile()` under
  # `reports/`) so re-running the same package a second time — or
  # cleaning up after a prior run that was killed before its
  # `on.exit()` fired — never collides with a stale scratch tree.
  # A leftover `.render_<pkg>_<ver>/` from a crashed render would
  # otherwise cause `quarto::quarto_render()` to fail with the
  # opaque "Error running quarto CLI from R". See #165.
  pkg_render_dir <- tempfile(
    pattern = paste0(".render_", pkg_v, "_"),
    tmpdir = reports
  )
  dir.create(pkg_render_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(pkg_render_dir, recursive = TRUE, force = TRUE),
          add = TRUE)
  options(riskreports_output_dir = pkg_render_dir)
  pr <- val_time_block("report",
    riskreports::package_report(
      package_name = pkg,
      package_version = ver,
      template_path = system.file("report/package", package = "val.pipeline"),
      output_format = "typst", # Options include html, gfm, and typst. Supplying 'all' does all 3
      # params list: https://github.com/pharmaR/riskreports/blob/main/inst/report/package/pkg_template.qmd
      params = list(
        assessment_path = assessment_file,
        hide_reverse_deps = 'false',
        source = src_ref, # defined above
        repo_url = repo_src,
        val_date = as.character(val_date),
        val_dir = out_dir
      ),
      quiet = TRUE, # To silence quarto output for readability
    ))
  # Move the produced output file(s) up into the shared reports dir so
  # downstream lookups (which key off `reports/`) keep working. `pr` is
  # the character vector of paths riskreports produced; anything else in
  # the scratch dir is a template artifact and gets cleaned up by the
  # on.exit() above.
  if (length(pr) > 0L) {
    dest_files <- file.path(reports, basename(pr))
    file.copy(pr, dest_files, overwrite = TRUE, copy.date = TRUE)
    pr <- dest_files
  }
  # pr
  
  val_msg("\n-->", pkg_v,"Report built.\n", min_level = "normal")
  
  
  
  
  #
  # ---- Save Pkg Meta Bundle ---- 
  #
  
  # Save a list of items beyond the assessment values
  meta_list <- list(
    pkg = pkg,
    ver = ver,
    r_ver = getRversion(),
    # Which val.pipeline built this bundle. Used by the summary
    # report to display the distinct set of package versions that
    # produced a run (a resumed run can span multiple versions if
    # the operator updated the package between sessions). See #130.
    val_pipeline_ver = as.character(utils::packageVersion("val.pipeline")),
    sys_info = list(R.Version()),
    repos = repo_name, # A named character
    # Plain-string label for the source repo (e.g. "CRAN", "BioC",
    # "github_pharmaverse", or "unknown"). See derivation above.
    repo_name = repo_label,
    val_date = val_date,
    ref = ref,
    metric_pkg = metric_pkg,
    # metrics = pkg_assessment, # saved separately for {riskreports}
    decision = decision$final_risk,
    decision_reason = decision_reason,
    decision_reason_note = decision_reason_note,
    final_decision = NA_character_, # Will be set later
    final_decision_reason = NA_character_, # Will be set later
    final_decision_reason_note = NA_character_, # Will be set later
    depends = if(identical(depends, character(0))) NA_character_ else depends,
    suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
    depends_direct  = if(identical(depends_direct,  character(0))) NA_character_ else depends_direct,
    suggests_direct = if(identical(suggests_direct, character(0))) NA_character_ else suggests_direct,
    rev_deps = if(is.null(pkg_assessment$reverse_dependencies)) NA_character_ else pkg_assessment$reverse_dependencies |> as.vector(),
    assessment_runtime = list(txt = ass_mins_txt, mins = ass_mins),
    # Testthat skip report scalars (issue #150). Populated when
    # `assess_covr_coverage` was included in the final pass (i.e.
    # `!auto_accepted`) and the package ships a `tests/testthat/`
    # directory. `NA_integer_` / `NA_real_` for every other case
    # (auto-accepted, tinytest / RUnit / no-tests packages, or a
    # `testthat::test_dir()` failure that `capture_covr_skip_report()`
    # absorbed). `val_finalize()` binds these into `qual_metadata.rds`
    # so the summary report can render cohort-level tables, and the
    # full `covr_skip_report` list (with per-message top reasons) rides
    # as an attribute on `pkg_assessment` for the per-package report.
    # See `capture_covr_skip_report()` in R/utils.R for the shape.
    covr_n_test    = if (is.null(covr_skip_report)) NA_integer_
                     else covr_skip_report$totals$n_test,
    covr_n_skip    = if (is.null(covr_skip_report)) NA_integer_
                     else covr_skip_report$totals$n_skip,
    covr_pct_skip  = if (is.null(covr_skip_report) ||
                         covr_skip_report$totals$n_test == 0L)
                       NA_real_
                     else 100 * covr_skip_report$totals$n_skip /
                       covr_skip_report$totals$n_test,
    # Estimate: what covr_coverage would have been if the skipped
    # test_that() blocks had covered code at the *average* rate of
    # the blocks that did run. `covr_coverage / (1 - pct_skip / 100)`,
    # capped at 100. Rough upper bound — labelled "estimate" in the
    # per-package and summary reports. NA when either coverage or
    # pct_skip is missing (covr_coverage errored, skip capture off
    # for this pkg, tests dir missing, etc.). See
    # `covr_effective_coverage()` in R/utils.R. Issue #150 follow-up.
    covr_effective_coverage = {
      # Safe covr_coverage lookup — see pkg_assessment_covr_pct().
      # The reuse_init / remote_only path bypasses the skip_cfg
      # block above, so meta_list is the first covr access for
      # those pkgs; a plain `pkg_assessment$covr_coverage` here
      # would trip the same vctrs strict-subset error (#161).
      cov_v <- pkg_assessment_covr_pct(pkg_assessment)
      pct_v <- if (is.null(covr_skip_report) ||
                   covr_skip_report$totals$n_test == 0L) NA_real_
               else 100 * covr_skip_report$totals$n_skip /
                     covr_skip_report$totals$n_test
      covr_effective_coverage(coverage_pct = cov_v, pct_skip = pct_v)
    },
    # Diagnostic capture when val_decision()'s rule ladder produced
    # `final_risk = NA` for this pkg -- populated in the "Silent-NA
    # capture" block above. NULL for pkgs with a real decision. See
    # #124.
    assessment_gaps = assessment_gaps,
    # Per-phase elapsed seconds captured via val_time_block() around
    # the fat blocks (download, untar, assess_initial, assess_final,
    # decision, report). Named list; each value is a numeric vector
    # (usually length 1). val_build() aggregates these across the
    # cohort into `timings.csv` under val_dir. See #87.
    timings = get_pkg_timings()
  )
  # meta_list <- readRDS(file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  # meta_list$rev_deps
  saveRDS(meta_list, file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  val_msg("\n-->", pkg_v,"meta bundle saved.\n", min_level = "normal")
  val_pkg_summary_line(pkg, ver, decision$final_risk,
                       elapsed_secs = as.numeric(ass_mins, units = "secs"),
                       pkg_idx = pkg_idx,
                       pkg_total = pkg_total)
  
  return(meta_list)
}


