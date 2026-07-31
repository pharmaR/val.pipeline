
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
#' @return A list containing:
#' - val_dir: The directory where the validation build results are stored.
#' - pkg_meta: A data frame summarizing the risk assessment results for all 
#'   packages assessed, including their dependencies and final risk decisions.
#' - pkg_assess: A data frame containing detailed (`riskmetric`) assessment
#'   records for each package.
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
#' @export
#' 
val_build <- function(
    pkg_names = NULL, #
    ref = c("source", "remote"),
    metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
    deps = c("depends", "suggests")[1], # deps = c("depends"), deps = NULL
    deps_recursive = TRUE,
    val_date = Sys.Date(),
    out = 'riskassessment',
    replace = FALSE,
    opt_repos = 
    c(CRAN = "https://packagemanager.posit.co/cran/latest",
      BioC = 'https://bioconductor.org/packages/3.22/bioc'),
    verbose = NULL,
    prep = NULL,
    config_path = NULL,
    workers = 1L
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
  apply_verbose(verbose)
  configure_bioc_repositories_if_requested(quiet = TRUE)
  configure_riskmetric_offline_if_requested(quiet = TRUE)

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
      pkg_names      = pkg_names,
      deps           = deps,
      deps_recursive = deps_recursive
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

    if (!is_dep_skip) {
      if (!file.exists(pkg_meta_file) | replace) {
        pkg_meta <- val_pkg(
          pkg = pkg,
          ver = ver,
          avail_pkgs = avail_pkgs,
          ref = if(pkg %in% remote_pkgs) 'remote' else ref,
          metric_pkg = metric_pkg,
          out_dir = val_dir,
          val_date = val_date,
          pkg_idx = pkg_cnt,
          pkg_total = pkgs_length)
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

      repo_src <- avail_pkgs |>
        dplyr::filter(Package %in% pkg) |>
        dplyr::pull(Repository) |>
        dirname() |> dirname()
      repo_name <- get_repo_origin(repo_src = repo_src, pkg_name = pkg)

      dep_note <- identify_failed_deps(c(depends, suggests), failed_snapshot)

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
        decision_reason = "Dependency",
        decision_reason_note = dep_note,
        final_decision = decisions[length(decisions)],
        final_decision_reason = "Dependency",
        final_decision_reason_note = dep_note,
        depends = if(identical(depends, character(0))) NA_character_ else depends,
        suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
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

    pkg_bundles <- future.apply::future_mapply(
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
      },
      pkg     = pkgs,
      ver     = vers,
      pkg_cnt = seq_along(pkgs),
      SIMPLIFY  = FALSE,
      USE.NAMES = FALSE,
      future.seed = TRUE
    )
    names(pkg_bundles) <- pkgs
  } else {
    pkg_bundles <- purrr::map2(pkgs, vers, function(pkg, ver){
      pkg_cnt <- which(pkgs == pkg)
      is_dep_skip <- pkg %in% dont_run
      pkg_meta <- assess_one(pkg, ver, pkg_cnt,
                             is_dep_skip = is_dep_skip,
                             failed_snapshot = failed_pkgs)
      if (!is_dep_skip && pkg_meta$decision != decisions[1]) {
        val_msg(paste0("\n\n--> ", pkg, " v", ver," was assessed with a '",
                       pkg_meta$decision,"' risk. All packages that depend on it will also be marked as '",
                       decisions[length(decisions)],"' risk.\n\n"),
                min_level = "normal")
        dont_run    <<- c(dont_run, pkg_meta$rev_deps) |> unique()
        failed_pkgs <<- c(failed_pkgs, pkg) |> unique()
      }
      pkg_meta
    }) |>
      purrr::set_names(nm = pkgs)
  }
  
  # Message
  # dont_run |> length()
  skipped_pkgs <- pkgs[pkgs %in% dont_run]
  val_msg("\n--> All", pkgs_length, "packages processed;",
          skipped_pkgs |> length(),
          "of which were avoided due to a dependency failing it's risk assessment.\n",
          min_level = "minimal")

  
  
  
  #
  # ---- Collate Assessment files into DF ----
  #
  
  # # Start bundling rds files
  record_files <- list.files(assessed, pattern = "_assess_record.rds$")
  record_length <- record_files |> length() # assessment file count
  # NB: pass the whole list to `dplyr::bind_rows()` in one call rather than
  # `purrr::reduce(bind_rows)`. Reducing is O(n^2) (the growing accumulator is
  # copied on every step) and dominates wall-clock on ~1000+ pkg runs; a single
  # `bind_rows(list_of_frames)` is O(n). Empty-input guard preserves the prior
  # behaviour where `purrr::reduce(list(), bind_rows)` errored: we now stop()
  # with an actionable message instead of silently writing an empty RDS. #69.
  if (record_length == 0L) {
    stop("No `_assess_record.rds` files found under ", assessed,
         " to collate into `qual_assessments.rds`.", call. = FALSE)
  }
  assessment_bundle <- purrr::map(record_files, function(file){
    # file <- record_files[1] # for debugging
    readRDS(file.path(assessed, file))
  }) |>
    dplyr::bind_rows()
  qual_assessments_file <- file.path(val_dir, "qual_assessments.rds")
  saveRDS(assessment_bundle, qual_assessments_file)
  val_msg(paste0("\n--> Saved assessment records to ",
                 qual_assessments_file, "\n"),
          min_level = "minimal")
  
  
  #
  # ---- Collate Pkg Meta into DF ----
  #
    # For Debugging
    # meta_files <- list.files(assessed, pattern = "_meta.rds$")
    # meta_length <- meta_files |> length() # assessment file count
    # pkg_bundles <- purrr::map(meta_files, function(file){
    #   # file <- meta_files[1] # for debugging
    #   meta_cnt <- which(meta_files == file)
    #   pkg_v <- gsub("_meta.rds", "", file)
    #   pkg <- stringr::word(pkg_v, 1, sep = "_")
    #   ver <- stringr::word(pkg_v, 2, sep = "_")
    #   cat(paste0("\n\n#", meta_cnt, " of ", meta_length, ": ", pkg))
    #   readRDS(file.path(assessed, file))
    # }) 
  
  
  # Reduce package bundles down into a data.frame containing specific info
  # names(pkg_bundles)
  # NB: single `dplyr::bind_rows(list_of_tibbles)` call instead of
  # `purrr::reduce(bind_rows)` — same O(n) vs O(n^2) reason as the assessment
  # collation above (#69).
  pkgs_df0 <- purrr::map( pkg_bundles, ~ {
      # .x <- pkg_bundles$askpass
      x <- purrr::list_flatten(.x)
      # x$depends  <- if(all(is.na(x$depends)))  NA_character_ else paste(x$depends, collapse = ", ")
      # x$suggests <- if(all(is.na(x$suggests))) NA_character_ else paste(x$suggests, collapse = ", ")
      
      x$depends <- list(x$depends)
      x$suggests <- list(x$suggests)
      x$rev_deps <- list(x$rev_deps)
      x$sys_info <- list(x$sys_info)
      # x$repos <- list(x$repos)
      dplyr::as_tibble(x)
    }) |> 
    dplyr::bind_rows()
  
  
  # Interim snapshot BEFORE dependency-based decision propagation runs.
  # Kept as a separate file (qual_metadata0.rds) so the pre-propagation state
  # remains inspectable for debugging decision-graph issues. The final
  # qual_metadata.rds is written after reject_iteration() converges (below).
  qual_metadata0_file <- file.path(val_dir, "qual_metadata0.rds")
  saveRDS(pkgs_df0, qual_metadata0_file)
  val_msg(paste0("\n--> Saved interim pkg metadata to ",
                 qual_metadata0_file, "\n"),
          min_level = "minimal")
  
  
  
  val_msg("\n--> Collated pkg metadata.\n", min_level = "normal")
  
  #
  # ---- Update final decisions ----
  #
  
  # We need to be able to change 'final' decisions (recursively) if a package's
  # dependency doesn't pass. That means, All the packages where decision is NOT
  # marked "Low" need to have their decision matriculate up through their
  # reverse dependencies (rev_deps).
  
  # Steps:
  # 1. identify all packages that are NOT "Low Risk"
  # 2. identify all packages that depend on those packages
  # 3. change their decision the decision of their dependency
  
  # pkgs_df0$decision[1] <- "High" # for debugging
  # reject_iteration() lives in R/utils.R so it is unit-testable in isolation.
  # First iteration:
  # Based off of 'decision', not 'final_decision'
  dec_reject <- decisions[length(decisions)]
  failed <- pkgs_df0$pkg[pkgs_df0$decision != decisions[1]] # start w/ 'decision'
  pkgs_df <- reject_iteration(pkgs_df0, dec_reject, deps, decisions, failed)
  
  # All remaining iterations!
  while(!identical(pkgs_df$pkg[pkgs_df$final_decision != decisions[1]], failed)) {
    # if the list of failed pkgs has changed, then we need to iterate again
    failed <<- pkgs_df$pkg[pkgs_df$final_decision != decisions[1]]
    pkgs_df <<- reject_iteration(pkgs_df, dec_reject, deps, decisions, failed)
  }
  
  val_msg("\n--> Assigned 'final' decisions.\n", min_level = "minimal")
  
  # Save the final qualification frame BEFORE the per-package meta RDS
  # update walk below. Prior versions saved this at the very end of val_build(),
  # which meant any error inside the walk would leave qual_metadata.rds as the
  # interim pkgs_df0 snapshot (final_decision NA for every val_pkg()-assessed
  # row). See #53.
  saveRDS(pkgs_df, file.path(val_dir, "qual_metadata.rds"))
  val_msg(paste0("\n--> Saved qualification evidence to ",
                 file.path(val_dir, "qual_metadata.rds"), "\n"),
          min_level = "minimal")
  
  
  
  
  #
  # ---- Update pkg_meta RDS file ----
  #
  # Which packges had a decision change?
  changed_pkgs <-
    pkgs_df |>
    dplyr::filter(final_decision != decision)

  purrr::pwalk(
    list(changed_pkgs$pkg, changed_pkgs$ver, changed_pkgs$final_decision_reason_note),
    function(pkg, ver, note){
    # i <- 1 # for debugging
    # pkg <- changed_pkgs$pkg[i] # for debugging
    # ver <- changed_pkgs$ver[i] # for debugging
    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))
    pkg_meta_file <- pkg_meta_file[file.exists(pkg_meta_file)]
    if(length(pkg_meta_file) > 0) {
      # update the decision of each reverse dependency pkg
      purrr::walk(pkg_meta_file, function(f){
        dep_meta <- readRDS(f)
        dep_meta$final_decision_reason <- "Dependency"
        dep_meta$final_decision_reason_note <- note
        dep_meta$final_decision <- decisions[length(decisions)]
        saveRDS(dep_meta, f)
        val_msg(paste0("\n\n--> Updated ", dep_meta$pkg, " v", dep_meta$ver," from '", dep_meta$decision,"' to '", dep_meta$final_decision,"' in meta bundle .rds.\n"),
                min_level = "verbose")
      })
    }
  })
  
  val_msg("\n--> Updated", nrow(changed_pkgs),"pkg metadata files.\n",
          min_level = "normal")
  
  #
  # ---- Aggregate per-package timings ----
  #
  # Every val_pkg() bundle carries a `$timings` list keyed by the
  # val_time_block() labels (`download`, `untar`, `assess_initial`,
  # `assess_final`, `decision`, `report`). Explode those into a long
  # data.frame and write it as `timings.csv` under val_dir for
  # later profiling analysis. Skipped pkgs (dep-skip pre-filter) have
  # no timings; they contribute zero rows. See #87.
  timings_df <- purrr::imap(pkg_bundles, function(bundle, pkg_name) {
    tmap <- bundle[["timings"]]
    if (is.null(tmap) || length(tmap) == 0L) return(NULL)
    ver <- bundle[["ver"]]
    if (is.null(ver)) ver <- NA_character_
    purrr::imap(tmap, function(secs, phase) {
      data.frame(
        pkg     = pkg_name,
        ver     = as.character(ver),
        phase   = phase,
        seconds = as.numeric(secs),
        stringsAsFactors = FALSE
      )
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind()
  
  if (nrow(timings_df) > 0L) {
    timings_file <- file.path(val_dir, "timings.csv")
    utils::write.csv(timings_df, timings_file, row.names = FALSE)
    val_msg(paste0("\n--> Wrote per-phase timings for ",
                   length(unique(timings_df$pkg)), " pkg(s) to ",
                   timings_file, "\n"),
            min_level = "minimal")
  }
  
  val_end <- Sys.time()
  val_end_txt <- utils::capture.output(val_end - val_start)
  val_msg("\n--> Build", val_end_txt,"\n", min_level = "minimal")
  
  # Return object 
  return(list(
    val_dir = val_dir#,
    # pkg_meta = pkgs_df,
    # pkg_assess = assessment_bundle
  ))
}



