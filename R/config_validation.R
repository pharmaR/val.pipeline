#' Version of the `config.yml` schema this package understands
#'
#' Bumped when the schema changes in a backwards-incompatible way. Configs
#' declaring a `config_version:` older than [CONFIG_VERSION_MIN] are
#' rejected by [validate_config()]; configs declaring a version newer than
#' [CONFIG_VERSION_CURRENT] trigger a warning (the run continues, but the
#' user is asked to upgrade `val.pipeline`).
#'
#' @keywords internal
CONFIG_VERSION_CURRENT <- "1.0.0"

#' Oldest `config_version` supported by this release of `val.pipeline`
#'
#' @keywords internal
CONFIG_VERSION_MIN <- "1.0.0"

# Rule-block section names recognised in config.yml. Anything else at the
# top level triggers a "possible typo" warning.
.known_top_sections <- c("default", "remote_reduce", "decide")

# Keys allowed inside the `default` section.
.known_default_keys <- c(
  "config_version",
  "air_gapped",
  "remote_only",
  "pass_primary",
  "approved_pkgs",
  "decisions_lst",
  "opt_repos"
)

# Keys allowed inside each metric rule block (remote_reduce / decide).
.known_rule_keys <- c(
  "cond", "type", "accept_cats",
  "promote_min", "auto_accept"
)

# Rule "type" values accepted by val_categorize() / val_decision().
.known_rule_types <- c("primary", "secondary")


#' Validate a `val.pipeline` `config.yml`
#'
#' Reads the resolved config (either an explicit path, the session option
#' `val.pipeline.config_path`, or the package's bundled default) and checks
#' that:
#'
#' * a `config_version` is declared and falls within the range this
#'   release of `val.pipeline` understands (see [CONFIG_VERSION_CURRENT]
#'   and [CONFIG_VERSION_MIN]);
#' * every top-level rule-block name is one of `default`, `remote_reduce`,
#'   `decide`;
#' * every key inside `default` is one of the known defaults keys;
#' * every metric rule under `remote_reduce` / `decide` declares a
#'   `cond` list of decision categories (matching `decisions_lst`), a
#'   `type` of `"primary"` or `"secondary"`, and (optionally) a valid
#'   `accept_cats`;
#' * `air_gapped` (if present) is a scalar logical.
#'
#' Typos in keys become warnings pointing at the offending name and the
#' rule block it lives in, rather than silently reverting to defaults.
#'
#' @param config_path Optional path to a `config.yml` file. When `NULL`
#'   (default), the session option `val.pipeline.config_path` is used
#'   (or the package-bundled config as a final fallback).
#' @param strict Logical. When `TRUE`, any warning is escalated to an
#'   error. Defaults to `FALSE` so pipelines can continue with warnings
#'   while still being audited.
#'
#' @return Invisibly, a named list with elements:
#' \describe{
#'   \item{`ok`}{`TRUE` when no errors were raised.}
#'   \item{`config_version`}{The version string declared in the config
#'     (or `NA_character_` when unset).}
#'   \item{`warnings`}{Character vector of warning messages emitted.}
#'   \item{`path`}{The resolved config path.}
#' }
#'
#' @examples
#' \dontrun{
#' validate_config()                  # validate the bundled config
#' validate_config("my/config.yml")   # validate a user-supplied config
#' validate_config(strict = TRUE)     # error on any warning
#' }
#'
#' @export
validate_config <- function(config_path = NULL, strict = FALSE) {
  path <- resolve_config_path(config_path)
  raw <- tryCatch(
    yaml::yaml.load_file(path),
    error = function(e) {
      stop(sprintf("Failed to parse config file '%s': %s",
                   path, conditionMessage(e)), call. = FALSE)
    }
  )

  warnings <- character(0)
  emit <- function(msg) {
    warnings[[length(warnings) + 1L]] <<- msg
    if (isTRUE(strict)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  if (!is.list(raw)) {
    stop(sprintf("Config file '%s' is not a mapping.", path), call. = FALSE)
  }

  # Top-level section names
  unknown_sections <- setdiff(names(raw), .known_top_sections)
  if (length(unknown_sections)) {
    emit(sprintf(
      "Unknown top-level section(s) in config: %s. Known sections: %s.",
      paste(unknown_sections, collapse = ", "),
      paste(.known_top_sections, collapse = ", ")
    ))
  }

  default <- raw[["default"]]
  if (is.null(default) || !is.list(default)) {
    emit("Config is missing a `default:` section.")
    default <- list()
  }

  # config_version presence + compatibility
  declared_version <- default[["config_version"]]
  if (is.null(declared_version)) {
    emit(sprintf(
      "Config is missing `config_version:`. Add `config_version: \"%s\"` under `default:` to silence this warning.",
      CONFIG_VERSION_CURRENT
    ))
    declared_version <- NA_character_
  } else {
    declared_version <- as.character(declared_version)
    check <- .check_config_version(declared_version)
    if (!is.null(check)) emit(check)
  }

  # air_gapped shape
  if (!is.null(default[["air_gapped"]])) {
    ag <- default[["air_gapped"]]
    if (!(is.logical(ag) && length(ag) == 1L && !is.na(ag))) {
      emit(sprintf(
        "`default: air_gapped:` must be a single TRUE/FALSE value, got: %s",
        paste0(deparse(ag), collapse = " ")
      ))
    }
  }

  # Unknown keys under `default`
  unknown_default <- setdiff(names(default), .known_default_keys)
  if (length(unknown_default)) {
    emit(sprintf(
      "Unknown key(s) under `default:` — %s. Known keys: %s.",
      paste(unknown_default, collapse = ", "),
      paste(.known_default_keys, collapse = ", ")
    ))
  }

  # Rule-block validation
  decisions <- default[["decisions_lst"]]
  for (section in intersect(c("remote_reduce", "decide"), names(raw))) {
    .validate_rule_section(raw[[section]], section, decisions, emit)
  }

  invisible(list(
    ok = length(warnings) == 0L,
    config_version = declared_version,
    warnings = warnings,
    path = path
  ))
}


# Compare a declared version against the accepted range. Returns NULL when
# fine, else a warning string.
.check_config_version <- function(declared) {
  parse_ok <- tryCatch({
    package_version(declared)
    TRUE
  }, error = function(e) FALSE)
  if (!parse_ok) {
    return(sprintf(
      "`config_version` value %s is not a valid version string.",
      paste0(deparse(declared), collapse = " ")
    ))
  }
  dv <- package_version(declared)
  if (dv < package_version(CONFIG_VERSION_MIN)) {
    return(sprintf(
      "`config_version` %s is older than the minimum supported version %s. Update the config to the %s schema.",
      declared, CONFIG_VERSION_MIN, CONFIG_VERSION_CURRENT
    ))
  }
  if (dv > package_version(CONFIG_VERSION_CURRENT)) {
    return(sprintf(
      "`config_version` %s is newer than this release of val.pipeline understands (%s). Consider upgrading val.pipeline.",
      declared, CONFIG_VERSION_CURRENT
    ))
  }
  NULL
}


.validate_rule_section <- function(section, section_name, decisions, emit) {
  if (!is.list(section)) return(invisible())
  rule_names <- setdiff(names(section), "inherits")
  for (rn in rule_names) {
    rule <- section[[rn]]
    if (!is.list(rule)) {
      emit(sprintf("Rule `%s: %s:` must be a mapping.", section_name, rn))
      next
    }
    unknown_keys <- setdiff(names(rule), .known_rule_keys)
    if (length(unknown_keys)) {
      emit(sprintf(
        "Unknown key(s) in rule `%s: %s:` — %s. Known keys: %s.",
        section_name, rn, paste(unknown_keys, collapse = ", "),
        paste(.known_rule_keys, collapse = ", ")
      ))
    }
    if (!is.null(rule[["type"]]) &&
        !identical(as.character(rule[["type"]]), "primary") &&
        !identical(as.character(rule[["type"]]), "secondary")) {
      emit(sprintf(
        "Rule `%s: %s:` has invalid `type: %s`. Must be one of: %s.",
        section_name, rn, rule[["type"]],
        paste(.known_rule_types, collapse = ", ")
      ))
    }
    if (!is.null(decisions)) {
      cond_cats <- names(rule[["cond"]])
      bad_cond <- setdiff(cond_cats, decisions)
      if (length(bad_cond)) {
        emit(sprintf(
          "Rule `%s: %s: cond:` references decision categories not in `decisions_lst`: %s.",
          section_name, rn, paste(bad_cond, collapse = ", ")
        ))
      }
      accept <- rule[["accept_cats"]]
      bad_accept <- setdiff(accept, decisions)
      if (length(bad_accept)) {
        emit(sprintf(
          "Rule `%s: %s: accept_cats:` references decision categories not in `decisions_lst`: %s.",
          section_name, rn, paste(bad_accept, collapse = ", ")
        ))
      }
    }
  }
}


#' Whether the current run should enable the air-gapped shims
#'
#' Consulted by [configure_bioc_repositories_if_requested()] and
#' [configure_riskmetric_offline_if_requested()]. Returns `TRUE` when any
#' of the following is set:
#'
#' * The environment variable `VAL_PIPELINE_INTERNAL_BIOC` is truthy
#'   (`"1"`, `"true"`, `"yes"`, `"y"`, `"on"`).
#' * The resolved `config.yml` has `default: air_gapped: true`.
#'
#' The env var wins when both are set, so operators can override the
#' config from a shell.
#'
#' @return `TRUE` / `FALSE` (invisibly for the value).
#'
#' @keywords internal
.val_pipeline_air_gapped_requested <- function() {
  flag <- Sys.getenv("VAL_PIPELINE_INTERNAL_BIOC", unset = "")
  if (nzchar(flag) && tolower(flag) %in% c("1", "true", "yes", "y", "on")) {
    return(TRUE)
  }
  ag <- tryCatch(
    pull_config(val = "air_gapped", rule_type = "default"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  isTRUE(ag)
}
