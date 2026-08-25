
#' Write a Pipeline `.toml` File for `rv`
#'
#' Emits a `pipeline.toml` file describing the packages a
#' `val.pipeline` run intends to assess. The file is meant to be
#' consumed by `rv` so a Posit Package Manager snapshot can be
#' installed before the (expensive) `val_build()` step begins.
#'
#' The output layout matches the example recipe (repositories on
#' one line, dependencies as a multi-line array, one package per
#' line) so long lists stay readable in code review.
#'
#' @param pkgs Character vector of package names to list under
#'   `[project].dependencies`.
#' @param opt_repos Named character vector of repositories. Passed
#'   through to `[project].repositories`. Order is preserved.
#' @param local_repo Optional additional repository to prepend at
#'   position 1 of `[project].repositories` (so `rv` reaches it first
#'   when resolving packages). Intended for a PPM URL that should be
#'   consumed by `rv` while processing the toml *without* polluting
#'   the caller's `opt_repos` (which is used for the assessment
#'   itself). Either:
#'   \itemize{
#'     \item an unnamed `character(1)` URL — the alias defaults to
#'       `"local"`; or
#'     \item a named `character(1)` where the name is used as the alias
#'       (e.g. `c(local = "https://...")` or
#'       `c(internal_ppm = "https://...")`).
#'   }
#'   `NULL` (default) leaves `opt_repos` untouched.
#' @param r_version Character(1) written under `[project].r_version`.
#'   Defaults to the current R major.minor
#'   (e.g. `"4.5"`).
#' @param name Character(1) written under `[project].name`. Defaults
#'   to `"val.pipeline run"`.
#' @param install_suggestions Logical(1). When `TRUE` (the default),
#'   every dependency is rendered as an inline table
#'   `{ name = "pkg", install_suggestions = true }` so `rv` installs
#'   the package *and* its `Suggests` dependencies when materializing
#'   the pipeline snapshot. When `FALSE`, dependencies are rendered as
#'   bare strings (`"pkg"`), matching the pre-0.1.39 behaviour, and
#'   `rv` installs hard deps only.
#'
#'   Defaulting to `TRUE` is intentional and paired with Layer A env-var
#'   normalization (issue #146): with `NOT_CRAN=true` set for the covr
#'   run, tests gated on `testthat::skip_if_not_installed("someSuggest")`
#'   need their Suggests present in the pipeline library or they still
#'   silently skip. Preinstalling Suggests via `rv` closes that gap
#'   before `val_build()` starts, at the cost of a heavier initial
#'   install. `rv` only accepts `install_suggestions` as a per-dependency
#'   field (no top-level toggle exists in the rv schema — verified in
#'   `a2-ai/rv/src/config.rs`), so `TRUE` here means every entry gets
#'   the field set individually. The resulting toml is longer but is
#'   what `rv` expects.
#' @param path Character(1). Where to write the toml file.
#'
#' @return `path`, invisibly.
#'
#' @importFrom tomledit toml write_toml
#' @export
write_pipeline_toml <- function(
  pkgs,
  opt_repos,
  local_repo = NULL,
  r_version = paste(R.Version()$major, R.Version()$minor, sep = "."),
  name      = "val.pipeline run",
  install_suggestions = TRUE,
  path
){
  if (!is.character(pkgs) || length(pkgs) == 0)
    stop("`pkgs` must be a non-empty character vector", call. = FALSE)
  if (is.null(opt_repos) || length(opt_repos) == 0 ||
      is.null(names(opt_repos)))
    stop("`opt_repos` must be a named character vector", call. = FALSE)
  if (!is.logical(install_suggestions) ||
      length(install_suggestions) != 1L ||
      is.na(install_suggestions))
    stop("`install_suggestions` must be TRUE or FALSE", call. = FALSE)
  if (!nzchar(path)) stop("`path` must be a non-empty string", call. = FALSE)

  # Optionally prepend a caller-supplied repo (typically a PPM URL that
  # `rv` needs to see first, but that shouldn't leak into the caller's
  # `opt_repos`). We build a fresh `repos_out` here rather than
  # reassigning `opt_repos` to make it obvious at review time that the
  # caller's object is not mutated — R's copy-on-modify already
  # guarantees this, but the naming makes intent explicit and the
  # regression test in test-write_pipeline_toml.R locks it in.
  repos_out <- opt_repos
  if (!is.null(local_repo)) {
    if (!is.character(local_repo) || length(local_repo) != 1L ||
        !nzchar(local_repo))
      stop("`local_repo` must be a non-empty character(1)", call. = FALSE)
    local_alias <- names(local_repo)
    if (is.null(local_alias) || !nzchar(local_alias)) local_alias <- "local"
    local_entry <- unname(local_repo)
    names(local_entry) <- local_alias
    repos_out <- c(local_entry, opt_repos)
  }

  # Build repositories as an array of inline tables, one per repo, so
  # tomledit renders them as:
  #   repositories = [
  #     { alias = "CRAN", url = "https://..." },
  #     { alias = "BioC", url = "https://..." },
  #   ]
  # rather than a single-line inline-object dict. Order of `repos_out`
  # is preserved.
  repos_lst <- Map(
    function(alias, url) list(alias = alias, url = unname(url)),
    names(repos_out),
    repos_out
  ) |> unname()

  # Build the `dependencies` field. When `install_suggestions` is TRUE
  # each entry is rendered as an inline table so `rv` installs the
  # package plus its Suggests. Rendering as inline tables (rather than
  # strings) is the only rv-supported form for this flag — verified in
  # `a2-ai/rv/src/config.rs` — there is no top-level toggle in the rv
  # schema. When FALSE the entries are bare strings, matching the
  # pre-0.1.39 behaviour.
  if (install_suggestions) {
    deps_out <- lapply(pkgs, function(nm) {
      list(name = nm, install_suggestions = TRUE)
    })
  } else {
    deps_out <- pkgs
  }

  project <- tomledit::toml(
    project = list(
      name         = name,
      r_version    = r_version,
      repositories = repos_lst,
      dependencies = deps_out
    )
  )

  # Write once via tomledit, then re-format so long arrays are one
  # entry per line. tomledit already puts each inline-table on its own
  # line for the repositories block; the remaining tweaks are:
  #   * split the flat `dependencies = ["a", "b"]` array onto lines,
  #   * add a trailing comma after the last repository entry so
  #     re-ordering / adding repos is a one-line diff.
  tomledit::write_toml(project, path)
  readLines(path) |>
    paste0(collapse = "\n") |>
    gsub(pattern = ", \"",  replacement = ",\n\t\"") |>
    gsub(pattern = "\\[\"", replacement = "\\[\n\t\"") |>
    gsub(pattern = "\"\\]", replacement = "\"\n\\]") |>
    # trailing comma after the final `{ ... }` inline table in the
    # repositories array (right before the closing `]`).
    gsub(pattern = "(\\}\\s*)(\\n\\])", replacement = "},\\2") |>
    writeLines(path)

  invisible(path)
}
