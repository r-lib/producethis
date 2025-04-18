#' Add one or more environment variables to the project
#'
#' Environment variables can be used to control various aspects of the execution
#' of a production job. For environment variables that does not contain any
#' secret information it is beneficial to have them stored in the project to
#' make sure that they are set equally in all environments they are executed.
#' `use_envvars` will write the given environment variables to the `Envvar` field
#' of the DESCRIPTION file. During deployment these values are forwarded to the
#' deployment server. `set_envvars()` will register the environment variables in
#' the current R session for interactive use (this happens automatically in
#' [try_connect_run()]).
#'
#' @param ... name-value pairs giving the name and value of the environment
#' variable(s) to set
#'
#' @return These functions are called for their side effects
#'
#' @export
#'
#' @note **NEVER** store secret/confidential information in this way. This is
#' purely for use for behavior changing environment variables.
#'
use_envvars <- function(...) {
  # TODO: We might want to warn users never to put confidential info into env vars like this
  envs <- list2(...)

  if (length(envs) == 0) return(invisible())

  if (!is_named(envs)) {
    cli::cli_abort("All arguments must be named")
  }
  if (any(!vapply(envs, is_string, logical(1)) & lengths(envs) != 0)) {
    cli::cli_abort("All arguments must be strings")
  }

  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  write_list_to_desc(desc, "Envvar", envs)

  invisible()
}
#' @rdname use_envvars
#' @export
use_local_envvars <- function(...) {
  envs <- list2(...)
  if (length(envs) == 0) return(invisible())

  if (!is_named(envs)) {
    cli::cli_abort("All arguments must be named")
  }
  if (any(!vapply(envs, is_string, logical(1)) & lengths(envs) != 0)) {
    cli::cli_abort("All arguments must be strings")
  }

  if (!fs::file_exists(".Renviron")) {
    cli::cli_bullets(c("v" = "Creating {.file .Renviron}"))
    fs::file_create(".Renviron")
    usethis::use_git_ignore(".Renviron")
  }

  # TODO: Currently just adding to an ever growing list rather than replacing if already exists

  cat(paste0(names(envs), "=", envs), file = ".Renviron", sep = "\n")
  invisible()
}

#' @rdname use_envvars
#' @export
set_envvars <- function() {
  vars <- get_envvars()
  inject(Sys.setenv(!!!vars))
  invisible()
}

get_envvars <- function() {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  vars <- eval_from_desc(desc, "Envvar")
  if (length(vars) != 0 && (!is.list(vars) || is.null(names(vars)) || any(!vapply(vars, is.character, logical(1))) || any(lengths(vars) != 1))) {
    cli::cli_abort("{.field Envvar} must contain a named list of strings")
  }
  vars
}
