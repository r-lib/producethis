#' Provide additional settings for deployment
#'
#' These functions will record additional deployment specific settings for the
#' project, on top of the settings that are otherwise recorded in other
#' DESCRIPTION fields.
#'
#' @param ... key-value pairs of settings. See details for allowed names. Set
#' a key to `NULL` to remove it and thus use the default.
#'
#' @return This function is called for it's sideeffect
#'
#' @details
#' Settings are specific to a given deployment target. Currently only Posit
#' Connect is supported.
#'
#' # use_connect_settings()
#' Possible settings for Posit Connect are:
#'
#' * `locked`
#' * `locked_message`
#' * `connection_timeout`
#' * `read_timeout`
#' * `init_timeout`
#' * `idle_timeout`
#' * `max_processes`
#' * `min_processes`
#' * `max_conns_per_process`
#' * `load_factor`
#' * `cpu_request`
#' * `cpu_limit`
#' * `memory_request`
#' * `memory_limit`
#' * `amd_gpu_limit`
#' * `nvidia_gpu_limit`
#' * `default_image_name`
#' * `default_r_environment_management`
#' * `default_py_environment_management`
#'
#' Please consult the API documentation for your Connect instance for
#' description and allowed values for the various settings.
#'
#' @name use_settings
#' @rdname use_settings
#'
NULL

#' @rdname use_settings
#' @export
use_connect_settings <- function(...) {
  settings <- list2(...)
  validate_connect_settings(settings)
  settings[vapply(settings, is.na, logical(1))] <- NA
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  cur_settings <- if (desc$has_fields("Settings/Connect")) eval(parse(text = desc$get_field("Settings/Connect"))) else list()
  if (!is_bare_list(cur_settings)) {
    cli::cli_warn("Ignoring malformed Connect settings in {.file DESCRIPTION}")
    cur_settings <- list()
  }
  settings <- utils::modifyList(cur_settings, settings)
  settings <- glue::glue_collapse(
    glue::glue("      {names(settings)} = {settings}"),
    sep = ",\n"
  )
  settings <- glue::glue("\n    list(\n{settings}\n    )", .trim = FALSE)
  desc$set("Settings/Connect" = settings)
  desc$write()
}

validate_connect_settings <- function(settings) {
  def <- get_connect_defaults()
  if (!all(names(settings) %in% names(def))) {
    cli::cli_abort("Unknown or non-permitted Connect Setting: {.arg {setdiff(names(settings), names(def))}}")
  }
  def <- def[names(settings)]
  def_is_integer <- vapply(def, is.integer, logical(1))
  def_is_numeric <- vapply(def, is.numeric, logical(1))
  def_is_string <- vapply(def, is.character, logical(1))
  def_is_bool <- vapply(def, is.logical, logical(1))
  for (i in seq_along(settings)) {
    if (def_is_integer[i] && !(is.null(settings[[i]]) || is.na(settings[[i]]) || is_scalar_integerish(settings[[i]], TRUE))) {
      cli::cli_abort("{.arg {names(settings)[i]}} must be {.val NULL} or an integer")
    }
    if (def_is_numeric[i] && !(is.null(settings[[i]]) || is.na(settings[[i]]) || is_scalar_double(settings[[i]]))) {
      cli::cli_abort("{.arg {names(settings)[i]}} must be {.val NULL} or a number")
    }
    if (def_is_string[i] && !(is.null(settings[[i]]) || is.na(settings[[i]]) || is_scalar_character(settings[[i]]))) {
      cli::cli_abort("{.arg {names(settings)[i]}} must be {.val NULL} or a string")
    }
    if (def_is_bool[i] && !(is.null(settings[[i]]) || is.na(settings[[i]]) || is_scalar_logical(settings[[i]]))) {
      cli::cli_abort("{.arg {names(settings)[i]}} must be {.val NULL} or a number")
    }
  }
}

get_connect_defaults <- function() {
  list(
    locked = FALSE,
    locked_message = "",
    connection_timeout = NA_integer_,
    read_timeout = NA_integer_,
    init_timeout = NA_integer_,
    idle_timeout = NA_integer_,
    max_processes = NA_integer_,
    min_processes = NA_integer_,
    max_conns_per_process = NA_integer_,
    load_factor = NA_real_,
    cpu_request = NA_real_,
    cpu_limit = NA_real_,
    memory_request = NA_integer_,
    memory_limit = NA_integer_,
    amd_gpu_limit = NA_real_,
    nvidia_gpu_limit = NA_real_,
    default_image_name = NA_character_,
    default_r_environment_management = NA,
    default_py_environment_management = NA
  )
}
