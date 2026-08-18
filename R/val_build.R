
#' Validation: Build an Assessment Co-hort
#'
#' Build a risk assessment validation for a set of R packages from various
#' sources (CRAN / Bioconductor / GitHub), with the ability to include
#' (optionally recursive) dependencies and suggests, and save the results in a
#' structured directory. The cherry on top is that this build will use logic
#' from val_decision() to not only apply risk decisions too all packages
#' assessed, but goes back around and will re-categorize decisions based on
#' whether any dependencies were categorized as "High Risk" / "Rejected". It
#' also is intelligent enough to sort the list of packages to run those with the
#' most dependencies first, so that if a package fails, it doesn't waste any
#' time running it's reverse dependence. After the pipeline applies a decision
#' onto each package using criteria provided in a config file, it even generates
#' a report detailing specifics of the assessment as supporting evidence. The
#' end result is a directory containing the assessment results and reports for
#' each package evaluated.
#'
#' @param pkg_names Character vector of package names to assess. If NULL
#'   (default), all packages available from the specified repository will be
#'   assessed.
#' @param ref Character string indicating the source of the packages to assess.
#'   Either "source" (default) for source packages, or "remote" for packages
#'   from remote repositories like CRAN/Bioconductor.
#' @param metric_pkg Character string specifying the risk assessment package to
#'   use. Either "riskmetric" (default) & or "val.meter" (not implemented yet).
#' @param deps Character vector specifying which types of dependencies to
#'   include in the assessment. Options are "depends", "suggests", or both
#'   (default). If NULL, only the specified packages from 'pkg_names' will be
#'   assessed without their dependencies.
#' @param deps_recursive Logical indicating whether to include dependencies
#'   recursively. Default is TRUE.
#' @param rev_deps Character or NULL. Types of **reverse** dependencies
#'   to fold into the seed set before forward-dep expansion runs.
#'   Same shorthand as `deps` (`"depends"`, `"suggests"`, or both).
#'   `NULL` (default) means no rev-dep expansion. Only consulted when
#'   `prep = NULL`; a supplied `prep` already carries a resolved
#'   package tree.
#' @param rev_deps_recursive Logical. Whether reverse-dep expansion
#'   walks the transitive tree. Default is `FALSE` because rev-dep
#'   trees can explode (foundational pkgs like `Rcpp` have thousands
#'   of transitive dependents). Ignored when `rev_deps = NULL`.
#' @param val_date Date object or character string representing the date of the
#'   validation build. Default is the current date (Sys.Date()).
#' @param out Character string specifying the output directory for the
#'   validation build. Default is "riskassessment" in the current working
#'   directory.
#' @param replace Logical indicating whether to replace existing assessments for
#'   packages that have already been assessed. Default is FALSE.
#' @param opt_repos Named character vector specifying the repository options for
#'   package installation. Default is CRAN.
#' @param verbose Console verbosity control. One of `"quiet"`,
#'   `"minimal"`, `"normal"` (default), or `"verbose"`. See
#'   the `val.pipeline` verbosity docs for tier definitions. Defaults to whatever the
#'   session option `val.pipeline.verbose` is set to (or `"normal"`).
#'
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @importFrom dplyr filter pull mutate case_when as_tibble bind_rows
#' @importFrom purrr map2 set_names reduce map map_lgl list_flatten
#' @importFrom stringr word
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages capture.output
#'
#' @return A list with a single element `val_dir` (character path to
#'   the run directory). The recovery / two-phase workflow is driven
#'   off the `val_prep` object returned by [val_prep_pipeline()] — not
#'   this return — so when `finalize = FALSE` you invoke
#'   `val_finalize(prep = prep)` rather than reconstructing args from
#'   `val_build()`'s return. The collated frames themselves
#'   (`qual_metadata.rds`, `qual_assessments.rds`) live under
#'   `val_dir` on disk once `val_finalize()` runs; read them off disk
#'   when you need them.
#'
#' @param prep Optional `val_prep` object returned by
#'   [val_prep_pipeline()]. When supplied, `val_build()` skips its own
#'   dependency-tree resolution and directory setup, reusing the
#'   already-sorted `pkgs`, `vers`, `avail_pkgs`, `val_dir` and
#'   `opt_repos` from the prep result. This is the fast path callers
#'   should take when they've already emitted a `pipeline.toml` and
#'   installed the snapshot via `rv`.
#' @param config_path Optional path to a user-supplied `config.yml`.
#'   When provided, every internal `pull_config()` call made during this
#'   run reads from that file instead of the `config.yml` bundled with
#'   `val.pipeline`, and the same file is copied into `val_dir` for
#'   record keeping. The override is scoped to this call: the prior
#'   `val.pipeline.config_path` option is restored on exit.
#' @param workers Integer. Number of parallel workers to use during the
#'   per-package assessment loop. `1L` (default) preserves the original
#'   serial behaviour with dep-skip short-circuiting (dependents of a
#'   failed package are marked "Rejected" without being assessed).
#'   Values greater than `1` fan the loop out via
#'   [future.apply::future_mapply()] under a `future::multisession`
#'   plan; the dep-skip short-circuit is disabled in that mode because
#'   its state cannot cross a parallel worker boundary. Final risk
#'   propagation still runs downstream via [val_decision()], so
#'   package-report accuracy is unaffected — the only tradeoff is that
#'   dependents of failed packages spend CPU time being assessed
#'   instead of being short-circuited. Requires the optional `{future}`
#'   and `{future.apply}` packages when `workers > 1`.
#'
#' @param propagate_libpaths Logical(1). When `TRUE` (default), mirrors
#'   the current session's `.libPaths()` into the `R_LIBS_SITE`
#'   environment variable for the duration of the assessment loop, so
#'   subprocesses spawned by `riskmetric` (e.g. `rcmdcheck::rcmdcheck()`
#'   for the `r_cmd_check` metric, `covr::package_coverage()` for
#'   `covr_coverage`) see the same library search order as the parent R
#'   session. Without this, an interactive `.libPaths()` call in the
#'   parent (e.g. to point at an rv-provisioned library like
#'   `/data/pm/riskassessments/R_.../rv/library/.../`) does not reach
#'   any child R process, so R CMD check fails to locate dependencies
#'   and `r_cmd_check_errors` / `r_cmd_check_warnings` come back as
#'   `NA` for every package with a non-base dependency. Restored on
#'   function exit. Defaults to
#'   `getOption("val.pipeline.propagate_libpaths", TRUE)`; set that
#'   option to `FALSE` (or pass `propagate_libpaths = FALSE`) to opt
#'   out when an operator needs the child library search to stay
#'   isolated from the parent for some reason. See #99.
#'
#' @param mem_watchdog Logical(1). When `TRUE` (default), records
#'   per-package peak RSS to `<val_dir>/mem_watchdog.tsv` and prints a
#'   compact summary (p50 / p95 / max per-worker MB, top-10 heaviest
#'   packages, and — on Linux — a suggested `workers` value for the
#'   next run) at the end of the assessment loop. Skips packages whose
#'   `_meta.rds` was cached / dep-skipped since no real work was done.
#'   Silently no-ops if the peak-RSS sampler is unavailable on the
#'   host. Cost is a single line-append per package. See #122.
#'
#' @param finalize Logical(1). When `TRUE` (default), automatically
#'   calls [val_finalize()] on the run directory once every package
#'   has been assessed, so `qual_assessments.rds`, `qual_metadata.rds`,
#'   `timings.csv`, and the per-package `_meta.rds` decision
#'   propagation all land on disk before `val_build()` returns
#'   (matches pre-0.1.21 behaviour). When `FALSE`, `val_build()`
#'   returns as soon as the per-package assessment loop finishes and
#'   the collated artifacts must be produced by a separate
#'   `val_finalize(val_dir)` call. Use `FALSE` when the assessment
#'   loop is expensive enough that you want an explicit checkpoint,
#'   when you plan to iterate on decision logic against a fixed
#'   assessment corpus, or when you're recovering from an environment
#'   that hangs at the collation step (see #101). Note that the
#'   Posit Package Manager provisioning files
#'   (`qualified-<src>.txt` / `blocklist-<src>.txt`) and the summary
#'   report are never produced by `val_build()` regardless of this
#'   flag — those are always [val_pipeline()]-scope concerns.
#'
#' @export
#' 
val_build <- function(
    pkg_names = NULL, #
    ref = c("source", "remote"),
    metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
    deps = c("depends", "suggests")[1], # deps = c("depends"), deps = NULL
    deps_recursive = TRUE,
    rev_deps = NULL,
    rev_deps_recursive = FALSE,
    val_date = Sys.Date(),
    out = 'riskassessment',
    replace = FALSE,
    opt_repos = 
    c(CRAN = "https://packagemanager.posit.co/cran/latest",
      BioC = 'https://bioconductor.org/packages/3.22/bioc'),
    verbose = NULL,
    prep = NULL,
    config_path = NULL,
    workers = 1L,
    propagate_libpaths = getOption("val.pipeline.propagate_libpaths", TRUE),
    mem_watchdog = TRUE,
    finalize = TRUE
    ){
  
  #
  # Quick Param Run
  #
  
  # ref = "source" # default
  # # ref = "remote",
  # metric_pkg = "riskmetric" # default
  # # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  # # deps = c("depends", "suggests")
  # deps = "depends"  # default
  # # deps = NULL
  # # deps_recursive = FALSE
  # deps_recursive = TRUE # default
  # val_date = Sys.Date() # Sys.Date() # is  default
  # replace = FALSE # default
  # # replace = TRUE
  # out = Sys.getenv("RISK_OUTPATH", unset = getwd())
  # opt_repos = opt_repos
  
  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  if (!is.null(prep) && !inherits(prep, "val_prep")) {
    stop("`prep` must be a `val_prep` object returned by val_prep_pipeline().",
         call. = FALSE)
  }
  workers <- suppressWarnings(as.integer(workers))
  if (length(workers) != 1L || is.na(workers) || workers < 1L) {
    stop("`workers` must be a single positive integer.", call. = FALSE)
  }
  stopifnot(is.logical(finalize), length(finalize) == 1L, !is.na(finalize))
  stopifnot(is.logical(mem_watchdog), length(mem_watchdog) == 1L,
            !is.na(mem_watchdog))
  apply_verbose(verbose)
  configure_bioc_repositories_if_requested(quiet = TRUE)
  configure_riskmetric_offline_if_requested(quiet = TRUE)

  # Mirror the parent session's .libPaths() into R_LIBS_SITE so every
  # subprocess spawned by riskmetric (rcmdcheck::rcmdcheck for the
  # r_cmd_check metric, covr::package_coverage for covr_coverage, ...)
  # sees the same library search order. A fresh R subprocess does NOT
  # inherit interactive .libPaths() from the parent — it rebuilds its
  # search order from R_LIBS_SITE / R_LIBS_USER / R_LIBS + site
  # defaults. Without this mirror, an operator who pointed .libPaths()
  # at an rv-provisioned library (typical for the val.pipeline "install
  # via rv" flow) sees ~65% of packages come back with r_cmd_check_
  # errors/_warnings == NA because R CMD check can't find their deps.
  # Restored on exit via withr::local_envvar. See #99.
  if (isTRUE(propagate_libpaths)) {
    new_r_libs_site <- paste(.libPaths(), collapse = .Platform$path.sep)
    withr::local_envvar(c(R_LIBS_SITE = new_r_libs_site))
    val_msg(paste0("--> Mirrored .libPaths() into R_LIBS_SITE for ",
                   "subprocess visibility (r_cmd_check, covr_coverage, ...).\n"),
            min_level = "normal")
  }

  # Route pull_config() at any depth to the user-supplied config, if any.
  old_cfg <- options()["val.pipeline.config_path"]
  on.exit(options(old_cfg), add = TRUE)
  apply_config_path(config_path)
  
  # store R Version
  r_ver = getRversion()
  
  # Grab val date, output messaging
  val_start <- Sys.time()
  val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  val_date <- as.Date(val_date)
  val_date_txt <- gsub("-", "", val_date)
  val_msg(paste0("\n\n\nNew Validation build: R v", r_ver, " @ ", val_start_txt,"\n\n"),
          min_level = "normal")
  
  
  #
  # ---- Setup ----
  #
  
  # Pull in some config variables
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
  # opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
  
  old <- options()
  on.exit(function() options(old))
  if(ref == 'source') {
    options(repos = opt_repos, pkgType = "source") # , rlang_interactive = FALSE
  } else {
    options(repos = opt_repos) # , rlang_interactive = FALSE
  }
  # options("repos")
  
  #
  # ---- Which pkgs, ordered ----
  #

  if (!is.null(prep)) {
    # Fast path: caller supplied a `val_prep` object from
    # val_prep_pipeline(). Reuse the already-resolved pkgs/vers and the
    # dep-frequency-sorted avail_pkgs (needed later for repo-source lookup),
    # and skip the full dependency-tree resolution.
    pkgs        <- prep$pkgs
    vers        <- prep$vers
    avail_pkgs  <- prep$avail_pkgs
    # Prefer the prep result's snapshot for these too so both legs share
    # a single source of truth.
    if (!is.null(prep$val_date))  val_date  <- prep$val_date
    if (!is.null(prep$val_date))  val_date_txt <- gsub("-", "", val_date)
    if (!is.null(prep$opt_repos)) opt_repos <- prep$opt_repos
  } else {
    tree       <- resolve_pkg_tree(
      pkg_names          = pkg_names,
      deps               = deps,
      deps_recursive     = deps_recursive,
      rev_deps           = rev_deps,
      rev_deps_recursive = rev_deps_recursive
    )
    pkgs       <- tree$pkgs
    vers       <- tree$vers
    avail_pkgs <- tree$avail_pkgs
  }
  pkgs_length <- length(pkgs)
  val_msg("\n-->", pkgs_length, "package(s) to process.\n\n",
          min_level = "minimal")
  
  
  # Prompt the user to confirm they want to continue when assessing a lot of pkgs
  if(interactive() & pkgs_length >= 10) {
    message("Wow, looks like there is more than 10 pkgs to assess. That could take a while. Do you want to continue?")
    continue <- readline(prompt = "Continue: Y/N?")
    if(tolower(continue) == 'n') stop("User chose to stop the validation build.")
  }
  
  
  #
  # ---- Define dirs ----
  #
  
  r_dir <- file.path(out, glue::glue('R_{r_ver}'))
  val_dir <- file.path(r_dir, val_date_txt)
  assessed <- file.path(val_dir, 'assessed') # needed
  
  # create dirs if they don't exist
  if(!dir.exists(out)) dir.create(out)
  if(!dir.exists(r_dir)) dir.create(r_dir)
  if(!dir.exists(val_dir)) dir.create(val_dir)
  if(!dir.exists(assessed)) dir.create(assessed) # needed
  
  #
  # Save the config file to the val_dir for record keeping. Copies the
  # user-supplied config when one was provided, otherwise the packaged one.
  file.copy(
    resolve_config_path(config_path),
    file.path(val_dir, "config.yml"),
    overwrite = TRUE
  )
  
  #
  # ---- Init run log ----
  #
  # Tee every `val_msg()` / `val_print()` / `val_pkg_summary_line()`
  # call to `val_dir/val_pipeline.log` for the duration of this
  # `val_build()` invocation. Console tier and log tier are
  # decoupled -- `verbose = "minimal"` at the console can coexist
  # with `options(val.pipeline.log_level = "verbose")` on disk. See
  # #87.
  log_file <- file.path(val_dir, "val_pipeline.log")
  init_val_log(
    log_file,
    header = paste0("\n=== val_build() @ ",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                    " (R ", getRversion(), ", metric_pkg=", metric_pkg,
                    ", ref=", ref, ", workers=", workers, ") ===\n")
  )
  old_log_opts <- options(val.pipeline.log_file = log_file)
  on.exit(options(old_log_opts), add = TRUE)
  
  #
  # ---- Build pkg bundles ----
  #
  
  # Initiate a list to store pkgs that include the reverse dependencies of pkgs
  # that have failed
  dont_run <- character(0)
  # Track the actual failed package names (not just their rev_deps) so we can
  # name at least one failing dep in `decision_reason_note` for pre-skipped
  # packages downstream. Pkgs are processed in dep-frequency order, so a
  # foundational failing dep will be in `failed_pkgs` by the time any of its
  # rev-deps are visited (see issue #37).
  failed_pkgs <- character(0)
  
  
  # Start bundling.
  #
  # `workers > 1` fans the per-package assessment loop out via
  # `future.apply::future_mapply()`. Dep-skip short-circuiting (setting
  # `dont_run <<- rev_deps` in-loop) cannot cross a parallel worker
  # boundary, so it's *disabled* in parallel mode — every package is
  # assessed. The downstream final-decision propagation in val_decision()
  # still applies the "worst-of-any-dep" rule, so package-report accuracy
  # is preserved; the only tradeoff is that dependents of failed packages
  # spend CPU time being assessed instead of being short-circuited.
  # Callers with cohorts that fail rarely (e.g. an approved-list
  # revalidation) get near-linear speedup; callers with lots of failures
  # may prefer serial (workers = 1) for the short-circuit savings.

  assess_one <- function(pkg, ver, pkg_cnt, is_dep_skip, failed_snapshot) {
    val_msg(paste0("\n\n#", pkg_cnt, " of ", pkgs_length, ":"),
            min_level = "normal")

    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))

    # Watchdog bookkeeping. Only meaningful for packages we're about to
    # do real work on — cached / dep-skipped branches short-circuit
    # before val_pkg() runs and don't move memory, so we skip them.
    # See #122.
    wd_path <- if (isTRUE(mem_watchdog)) {
      file.path(val_dir, "mem_watchdog.tsv")
    } else {
      NULL
    }
    wd_did_work <- FALSE
    wd_errored  <- FALSE
    wd_start    <- Sys.time()

    if (!is_dep_skip) {
      if (!file.exists(pkg_meta_file) | replace) {
        wd_did_work <- TRUE
        pkg_meta <- tryCatch(
          val_pkg(
            pkg = pkg,
            ver = ver,
            avail_pkgs = avail_pkgs,
            ref = if(pkg %in% remote_pkgs) 'remote' else ref,
            metric_pkg = metric_pkg,
            out_dir = val_dir,
            val_date = val_date,
            pkg_idx = pkg_cnt,
            pkg_total = pkgs_length),
          error = function(e) {
            # Don't cancel the whole run just because one pkg blew up
            # somewhere deep in val_pkg() (build_decisions_df() with a
            # collapsed viable-metric set, a covr crash, a corrupt
            # tarball, etc.). Synthesize a High-tier `pkg_meta`
            # bundle keyed on `decision_reason = "Error"` with the
            # captured error message in `decision_reason_note`, save it
            # to disk so downstream collation still sees this pkg, and
            # log the failure at `minimal` so it surfaces even when the
            # caller has muted normal-tier output. Dependents of the
            # erroring pkg get downgraded via reject_iteration() in
            # val_finalize() thanks to the pkg landing in `failed_pkgs`
            # below. See #116.
            err_msg <- conditionMessage(e)
            wd_errored <<- TRUE
            val_msg(paste0("\n\n--> ERROR while assessing ", pkg, " v", ver,
                           ": ", err_msg,
                           "\n     Marking risk as '",
                           decisions[length(decisions)],
                           "' and continuing with the next package.\n\n"),
                    min_level = "minimal")
            repo_src <- avail_pkgs |>
              dplyr::filter(Package %in% pkg) |>
              dplyr::pull(Repository) |>
              dirname() |> dirname()
            repo_name <- tryCatch(
              get_repo_origin(repo_src = repo_src, pkg_name = pkg),
              error = function(e2) NA_character_
            )
            err_meta <- list(
              pkg = pkg,
              ver = ver,
              r_ver = getRversion(),
              sys_info = list(R.Version()),
              repos = repo_name,
              val_date = val_date,
              ref = NA_character_,
              metric_pkg = NA_character_,
              decision = decisions[length(decisions)],
              decision_reason = "Error",
              decision_reason_note = err_msg,
              final_decision = decisions[length(decisions)],
              final_decision_reason = "Error",
              final_decision_reason_note = err_msg,
              depends = NA_character_,
              suggests = NA_character_,
              depends_direct  = NA_character_,
              suggests_direct = NA_character_,
              rev_deps = NA_character_,
              assessment_runtime = list(txt = NA_character_, mins = NA)
            )
            tryCatch(saveRDS(err_meta, pkg_meta_file),
                     error = function(e3) invisible(NULL))
            val_pkg_summary_line(pkg, ver, err_meta$decision,
                                 suffix = "(error)",
                                 pkg_idx = pkg_cnt,
                                 pkg_total = pkgs_length)
            err_meta
          }
        )
      } else {
        val_msg(paste0("\nAttempted New Package: ", pkg, " v", ver,", but already assessed.\n\n"),
                min_level = "normal")
        pkg_meta <- readRDS(pkg_meta_file)

        val_msg("\n-->", pkg_v,"Using assessment previously stored.\n",
                min_level = "normal")
        val_pkg_summary_line(pkg, ver, pkg_meta$decision,
                             suffix = "(cached)",
                             pkg_idx = pkg_cnt,
                             pkg_total = pkgs_length)
      }
    } else {
      # ---- Pkg is in 'dont_run'! ----
      val_msg(paste0("\nAttempted New Package: ", pkg, " v", ver,", but one of it's dependencies already failed so skipping assessment and marking risk as '", decisions[length(decisions)], "'.\n\n"),
              min_level = "normal")

      depends <-
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = c("Depends", "Imports", "LinkingTo"),
          recursive = TRUE
        ) |>
        unlist(use.names = FALSE)

      suggests <-
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = "Suggests",
          recursive = TRUE
        ) |>
        unlist(use.names = FALSE)

      # Direct (non-recursive) deps for `decision_reason_note`. See #107.
      depends_direct <-
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = c("Depends", "Imports", "LinkingTo"),
          recursive = FALSE
        ) |>
        unlist(use.names = FALSE)

      suggests_direct <-
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = "Suggests",
          recursive = FALSE
        ) |>
        unlist(use.names = FALSE)

      repo_src <- avail_pkgs |>
        dplyr::filter(Package %in% pkg) |>
        dplyr::pull(Repository) |>
        dirname() |> dirname()
      repo_name <- get_repo_origin(repo_src = repo_src, pkg_name = pkg)

      # Name the direct DESCRIPTION-level dep(s) that failed, not the
      # recursive Suggests closure (which would list hundreds of
      # transitive pkgs). Scope by `deps` so failing suggests are only
      # named when the caller propagates them (matches the same
      # `"Suggests" %in% deps` gate used in reject_iteration()). See #107.
      note_deps <- if ("Suggests" %in% deps) {
        c(depends_direct, suggests_direct)
      } else {
        depends_direct
      }
      dep_note <- identify_failed_deps(note_deps, failed_snapshot)

      # If the pkg is on the config `approved_pkgs` list, distinguish it
      # from an ordinary dep-driven downgrade so an operator can chase
      # the upstream dep (or drop the pkg from `approved_pkgs`). Keeps
      # this pre-skip branch consistent with reject_iteration()'s
      # narrowed Pre-Approved carve-out (#110).
      approved_pkgs <- pull_config(val = "approved_pkgs", rule_type = "default")
      dep_reason <- if (pkg %in% approved_pkgs) {
        "Pre-Approved (dep failed)"
      } else {
        "Dependency"
      }

      pkg_meta <- list(
        pkg = pkg,
        ver = ver,
        r_ver = getRversion(),
        sys_info = list(R.Version()),
        repos = repo_name,
        val_date = val_date,
        ref = NA_character_,
        metric_pkg = NA_character_,
        decision = decisions[length(decisions)],
        decision_reason = dep_reason,
        decision_reason_note = dep_note,
        final_decision = decisions[length(decisions)],
        final_decision_reason = dep_reason,
        final_decision_reason_note = dep_note,
        depends = if(identical(depends, character(0))) NA_character_ else depends,
        suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
        depends_direct  = if(identical(depends_direct,  character(0))) NA_character_ else depends_direct,
        suggests_direct = if(identical(suggests_direct, character(0))) NA_character_ else suggests_direct,
        rev_deps = NA_character_,
        assessment_runtime = list(txt = NA_character_, mins = NA)
      )
      saveRDS(pkg_meta, pkg_meta_file)
      val_msg("\n-->", pkg_v,"meta bundle saved.\n", min_level = "verbose")
      val_pkg_summary_line(pkg, ver, pkg_meta$decision,
                           suffix = "(dep-skip)",
                           pkg_idx = pkg_cnt,
                           pkg_total = pkgs_length)
    }

    if (isTRUE(mem_watchdog) && wd_did_work) {
      wd_sample <- sample_peak_rss_mb()
      append_watchdog_row(
        wd_path,
        list(
          timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S",
                               tz = "UTC"),
          pkg         = pkg,
          version     = ver,
          worker_pid  = Sys.getpid(),
          peak_rss_mb = wd_sample$peak_rss_mb,
          elapsed_sec = round(as.numeric(
            difftime(Sys.time(), wd_start, units = "secs")), 2),
          sampler     = wd_sample$sampler,
          errored     = wd_errored
        )
      )
    }

    pkg_meta
  }

  if (workers > 1L) {
    if (!requireNamespace("future.apply", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE)) {
      stop("`workers > 1` requires the {future} and {future.apply} packages.",
           call. = FALSE)
    }
    val_msg(paste0("\nRunning ", pkgs_length,
                   " package assessments across ", workers,
                   " parallel worker(s). Dep-skip short-circuit is disabled ",
                   "in parallel mode; final risk propagation still occurs ",
                   "downstream via val_decision().\n"),
            min_level = "normal")

    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

    # `future::multisession` boots each worker in a fresh R session
    # that does NOT inherit the parent's `options()`. Capture the
    # verbose tier the parent resolved via apply_verbose() and re-apply
    # it inside every worker task so val_msg()/val_pkg() honour the
    # caller's `verbose = "minimal"` etc. instead of silently reverting
    # to the "normal" default.
    verbose_tier <- getOption("val.pipeline.verbose", "normal")
    # Same story for the user-supplied config path -- resolve_config_path()
    # inside the worker will otherwise fall back to the packaged config.
    config_path_tier <- getOption("val.pipeline.config_path", NULL)
    # And the run log path + tier -- workers append their `val_msg()`
    # output to the same on-disk log as the parent, and the log-file
    # tier (independent of console tier) needs to survive the
    # multisession boot too. NFSv4.2 O_APPEND is atomic for line-sized
    # writes so concurrent worker appends interleave cleanly. See #87.
    log_file_tier    <- getOption("val.pipeline.log_file", NULL)
    log_level_tier   <- getOption("val.pipeline.log_level", "normal")

    # Pre-filter already-assessed pkgs before dispatch. In parallel
    # mode there's no dep-skip state to update in-loop, so any pkg
    # whose `_meta.rds` is already on disk (and `replace = FALSE`)
    # would just hit the cached branch of `assess_one()` after paying
    # a full future/IPC round-trip and a spam of val_msg lines. Filter
    # those out up front. This turns a crashed 40-hr run's restart
    # from "dispatch 6000 workers that each print a 'cached' line"
    # into "dispatch only the N pkgs that still need real work". #91.
    todo <- seq_along(pkgs)
    if (!replace) {
      pkg_v_all <- paste(pkgs, vers, sep = "_")
      existing_meta <- file.path(assessed,
                                 paste0(pkg_v_all, "_meta.rds"))
      already_done <- file.exists(existing_meta)
      n_skip <- sum(already_done)
      if (n_skip > 0L) {
        val_msg(paste0("\n--> Skipping ", n_skip, " of ", pkgs_length,
                       " package(s) already assessed on disk ",
                       "(`_meta.rds` present under `assessed/`). ",
                       "Set `replace = TRUE` to re-run them.\n"),
                min_level = "minimal")
        todo <- which(!already_done)
      }
    }

    # Round-robin restripe of `todo` so heavy packages (which cluster
    # at the tail of the rev-dep-sorted input order) don't stack up
    # simultaneously across workers and blow the RAM budget. Two
    # flavours:
    #
    # 1. If a prior `mem_watchdog.tsv` is present in `val_dir` (a
    #    re-kick of the same run, or a run copied here from a similar
    #    cohort), sort `todo` by known peak_rss_mb desc first so
    #    the heaviest packages spread cleanly across worker buckets.
    # 2. Otherwise fall back to a pure round-robin over the input
    #    order — still spreads the tail cluster (typically Bioc /
    #    large ML pkgs) across workers even without prior peak data.
    #
    # Combined with `future.scheduling = 1L` below (one future per
    # package), this means at any moment the active worker set spans
    # the weight spectrum instead of piling into the tail all at once.
    # Serial mode (`workers = 1`) is unaffected — dep-skip
    # short-circuiting relies on the caller's rev-dep-sorted order.
    # See #122.
    if (length(todo) > workers) {
      wd_prior_path <- file.path(val_dir, "mem_watchdog.tsv")
      todo_pkgs <- pkgs[todo]
      if (file.exists(wd_prior_path)) {
        prior <- tryCatch(read_mem_watchdog_tsv(wd_prior_path),
                          error = function(e) NULL)
        if (!is.null(prior) && "pkg" %in% names(prior) &&
              "peak_rss_mb" %in% names(prior)) {
          by_pkg <- tapply(prior$peak_rss_mb, prior$pkg, max,
                           na.rm = TRUE)
          w <- unname(by_pkg[todo_pkgs])
          w[!is.finite(w)] <- -Inf
          if (any(w > -Inf)) {
            todo <- todo[order(-w)]
          }
        }
      }
      stride <- (seq_along(todo) - 1L) %% workers
      todo   <- todo[order(stride, seq_along(todo))]
    }

    # Workers discard the meta_list return: val_pkg() has already
    # persisted every artifact we care about (`_meta.rds`,
    # `_assess_record.rds`, `_assessments.rds`, `_scores.rds`) inside
    # `assessed/`. Shipping the full `meta_list` back through the
    # `future` IPC channel and accumulating it into a named list of
    # ~6000 bundles was a multi-GB memory sink on full CRAN+BioC runs
    # and was the primary driver of the OOM crashes users hit mid-run.
    # Downstream collation now streams from disk instead. See #91.
    if (length(todo) > 0L) {
      future.apply::future_mapply(
        FUN = function(pkg, ver, pkg_cnt) {
          options(val.pipeline.verbose = verbose_tier)
          if (!is.null(config_path_tier)) {
            options(val.pipeline.config_path = config_path_tier)
          }
          if (!is.null(log_file_tier) && nzchar(log_file_tier)) {
            options(val.pipeline.log_file  = log_file_tier,
                    val.pipeline.log_level = log_level_tier)
          }
          assess_one(pkg, ver, pkg_cnt,
                     is_dep_skip = FALSE,
                     failed_snapshot = character(0))
          invisible(NULL)
        },
        pkg     = pkgs[todo],
        ver     = vers[todo],
        pkg_cnt = todo,
        SIMPLIFY  = FALSE,
        USE.NAMES = FALSE,
        future.seed = TRUE,
        # One future per package (default is `ceiling(N / workers)`
        # per-chunk). Two reasons this matters:
        #   1. Silent worker die-off. A worker segfaulting or
        #      OOM-killed mid-chunk under the default chunk-size
        #      drops every remaining pkg in that chunk on the floor
        #      -- sometimes silently, sometimes with a FutureError
        #      depending on the future version -- and the parent's
        #      mapply return-value structure doesn't surface the loss
        #      to val_build(). With one-pkg-per-future, a worker
        #      death only loses the single pkg it was actively
        #      assessing; any others reassigned to a healthy worker
        #      still run. The disk-state guard immediately below then
        #      catches the delta. See #120.
        #   2. Heavy-pkg restripe. The interleave above only helps
        #      if workers pick items one-at-a-time; the default
        #      chunker would re-batch a whole stride into a single
        #      worker and defeat the interleave. See #122.
        future.scheduling = 1L
      )
      # Disk-state guard. Even with `future.scheduling = 1L`, a mass
      # worker die-off (e.g. OOM-killer sweeping all sessions) can
      # leave `future_mapply` returning "successfully" (or absorbing
      # the FutureErrors into its results structure) while the parent
      # never realizes only a fraction of `todo` finished. Recount
      # `_meta.rds` files on disk against what we dispatched; if the
      # gap is non-zero raise so val_finalize() doesn't collate a
      # truncated qual_metadata.rds silently. See #120.
      post_meta <- file.path(assessed,
                             paste0(paste(pkgs[todo], vers[todo],
                                          sep = "_"), "_meta.rds"))
      landed <- sum(file.exists(post_meta))
      if (landed < length(todo)) {
        missing_n <- length(todo) - landed
        stop("val_build(workers = ", workers, "): future_mapply returned ",
             "but only ", landed, " of ", length(todo),
             " dispatched package(s) have a `_meta.rds` on disk. ",
             missing_n, " package(s) never completed -- ",
             "typically a worker was OOM-killed or the parent process ",
             "hit a walltime mid-run. Re-run val_pipeline() (or ",
             "val_build()) with the same args and `replace = FALSE` to ",
             "pick up where this run left off; the already-assessed ",
             "packages will be skipped. See #120.",
             call. = FALSE)
      }
    } else {
      val_msg(paste0("\n--> All ", pkgs_length,
                     " package(s) already assessed on disk; ",
                     "skipping the parallel assessment phase and ",
                     "proceeding straight to collation.\n"),
              min_level = "minimal")
    }
  } else {
    # Serial mode: dep-skip short-circuit lives here. We only need
    # `$decision` + `$rev_deps` from each pkg_meta to update the
    # `dont_run` / `failed_pkgs` state; the full meta_list is discarded
    # after each iteration (already saved to disk by `val_pkg()` /
    # the dep-skip branch of `assess_one()`). A plain `for` loop makes
    # the per-iter release explicit and avoids the ~1-3 GB `pkg_bundles`
    # list that used to accumulate on full CRAN+BioC cohorts. See #91.
    for (i in seq_along(pkgs)) {
      pkg <- pkgs[[i]]
      ver <- vers[[i]]
      is_dep_skip <- pkg %in% dont_run
      pkg_meta <- assess_one(pkg, ver, i,
                             is_dep_skip = is_dep_skip,
                             failed_snapshot = failed_pkgs)
      # Silent-NA guard. When val_decision()'s rule ladder collapses
      # for a pkg (typically remote_only pkgs with a shrunken
      # viable-metric set), it returns final_risk = NA without
      # throwing; val_pkg() then persists a bundle with decision = NA.
      # Without this guard, the ! = comparison below evaluates to NA
      # and takes the whole run down. Skip dep propagation for the
      # NA pkg and log at minimal; the report will surface it under
      # "Packages with incomplete assessment". reject_iteration() at
      # finalize time treats NA decisions as non-failing rather than
      # cascading them into dep-driven downgrades. See #124.
      if (!is_dep_skip && is.na(pkg_meta$decision)) {
        val_msg(paste0("\n\n--> WARNING: ", pkg, " v", ver,
                       " has NA decision on its meta bundle ",
                       "(rule ladder produced no category). ",
                       "Skipping dep propagation for this pkg; ",
                       "see the summary report's ",
                       "'Packages with incomplete assessment' ",
                       "section for details.\n\n"),
                min_level = "minimal")
      } else if (!is_dep_skip && pkg_meta$decision != decisions[1]) {
        val_msg(paste0("\n\n--> ", pkg, " v", ver," was assessed with a '",
                       pkg_meta$decision,"' risk. All packages that depend on it will also be marked as '",
                       decisions[length(decisions)],"' risk.\n\n"),
                min_level = "normal")
        dont_run    <- c(dont_run, pkg_meta$rev_deps) |> unique()
        failed_pkgs <- c(failed_pkgs, pkg) |> unique()
      }
      rm(pkg_meta)
    }
  }
  
  # Message
  # dont_run |> length()
  skipped_pkgs <- pkgs[pkgs %in% dont_run]
  val_msg("\n--> All", pkgs_length, "packages processed;",
          skipped_pkgs |> length(),
          "of which were avoided due to a dependency failing it's risk assessment.\n",
          min_level = "minimal")

  #
  # ---- Collation ----
  #
  # As of #101 the collation tail (assessment/meta bundling, dep-driven
  # decision propagation via reject_iteration(), and timings.csv) lives in
  # val_finalize() so callers can recover from a val_build() that hangs or
  # is killed after the per-package assessment loop finishes but before
  # collation completes. Default is TRUE to preserve pre-0.1.21 behaviour;
  # ad-hoc / two-phase callers pass `finalize = FALSE` and run
  # `val_finalize(val_dir)` themselves. Note we deliberately skip the
  # PPM provisioning files + summary report here — those are always
  # val_pipeline()-scope; val_build() only owns the collation half.
  if (isTRUE(finalize)) {
    val_finalize(
      val_dir               = val_dir,
      deps                  = deps,
      val_start             = val_start,
      write_qualified_lists = FALSE,
      render_report         = FALSE,
      verbose               = verbose,
      config_path           = config_path
    )
  } else {
    val_msg(paste0("\n--> Skipped collation (finalize = FALSE). Run ",
                   "val_finalize(\"", val_dir,
                   "\") to collate assessments and propagate decisions.\n"),
            min_level = "normal")
  }

  # Return object. Deliberately narrow — just `val_dir` — because the
  # recovery / two-phase workflow is driven off the `val_prep` object
  # returned by [val_prep_pipeline()], not this return. See #101.
  return(list(
    val_dir = val_dir
  ))
}



