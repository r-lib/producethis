#' Check integrity of project
#'
#' The purpose of `check_project()` is to be for production projects what
#' `R CMD check` is for packages. A testing suite that verifies that everything
#' is as it should be to the best of its abilities.
#'
#' @export
#'
check_project <- function() {
  cli::cli_h1("Preparing project for check")
  old_opt <- options("usethis.quiet" = TRUE)
  on.exit(options(old_opt))

  temp_path <- fs::path_temp(fs::path_file(usethis::proj_path()))

  cli::cli_progress_step("Copying project to {.path {temp_path}}")
  fs::dir_copy(usethis::proj_path(), temp_path)

  on.exit(fs::dir_delete(temp_path), add = TRUE)

  cli::cli_progress_step("Instantiating project in new location")

  usethis::with_project(temp_path, {
    cli::cli_h1("Checking")
    envvars <- get_envvars()
    if (length(envvars) > 0) {
      old_env <- Sys.getenv(names(envvars), names = TRUE, unset = NA)
      on.exit(apply_envvar(old_env))
      apply_envvar(envvars)
      set_names(paste0(format(envvars, width = max(nchar(names(envvars)))), ": ", envvars), "*")
      cli::cli_bullets(c(
        "Setting env vars:",
        set_names(paste0(format(envvars, width = max(nchar(names(envvars)))), ": ", envvars), "*")
      ))
    }

    check_folder()

    check_description()

    check_renv()

    check_code()

    check_tests()

    check_deployment()
  })
  invisible(0)
}

check_folder <- function() {
  cli::cli_progress_step("Looking for {.file DESCRIPTION} file")
  if (!fs::file_exists("DESCRIPTION")) {
    cli::cli_progress_done(result = "failed")
    return(2)
  }
  # Look for .Rprofile and content
}

check_description <- function() {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  cli::cli_bullets(c("-" = "Checking project type"))
  if (!desc$has_fields("Type")) {
    cli::cli_bullets(c("!" = "No Type specified"))
  } else if (!tolower(desc$get_field("Type")) %in% c("batch", "app", "api", "report")) {
    cli::cli_bullets(c("!" = "Unknown type specified: {.field {desc$get_field('Type')}}"))
  }
  # TODO: Check that evaluated fields doesn't contain foreign calls
}

check_renv <- function() {

}

check_code <- function() {

}

check_tests <- function() {
  cli::cli_bullets(c("-" = "Checking tests"))
  if (fs::file_exists(usethis::proj_path("tests", "testthat", ext = "R"))) {
    cli::cli_bullets(c("v" = "Running {.file testthat.R}"))
    testthat::test_local()
  } else {
    cli::cli_bullets(c("!" = "No unit tests found"))
  }
}

check_deployment <- function() {
  if (is_deploying_on_connect()) {
    desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
    if (desc$has_fields("Settings/Connect")) {
      id <- cli::cli_progress_step("Checking Connect settings")
      try_fetch({
          settings <- eval_from_desc(desc, "Settings/Connect")
          validate_connect_settings(settings)
        },
        error = function(...) {
          cli::cli_progress_done(id = id, result = "failure")
        }
      )

    }

  }
}

apply_envvar <- function(envvars) {
  if (length(envvars) == 0) {
    return()
  }
  old <- Sys.getenv(names(envvars), names = TRUE, unset = NA)
  set <- !is.na(envvars)
  if (any(set)) do.call("Sys.setenv", as.list(envvars[set]))
  if (any(!set)) Sys.unsetenv(names(envvars)[!set])
  old
}
