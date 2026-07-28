
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
  path
){
  if (!is.character(pkgs) || length(pkgs) == 0)
    stop("`pkgs` must be a non-empty character vector", call. = FALSE)
  if (is.null(opt_repos) || length(opt_repos) == 0 ||
      is.null(names(opt_repos)))
    stop("`opt_repos` must be a named character vector", call. = FALSE)
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

  project <- tomledit::toml(
    project = list(
      name         = name,
      r_version    = r_version,
      repositories = repos_lst,
      dependencies = pkgs
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
