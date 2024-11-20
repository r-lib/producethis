#' Mimick a Connect deployment locally
#'
#' This function allows you to make a dry run locally of your production code in
#' a setting that mimicks how it will run on Posit Connect. This is not a
#' complete replica of the environment in which your code will run on Connect,
#' so lack of issues here does not equal lack of issues with deployment. However,
#' it is a good indicator.
#'
#' @param clean Should the temporary folder with the deployment be deleted once
#' the function returns
#'
#' @return This function is called for its side effects
#'
#' @export
#'
try_connect_run <- function(clean = TRUE) {
  temp_path <- fs::path_temp()

  fs::dir_copy(".", temp_path)

  if (clean) {
    on.exit(fs::dir_delete(temp_path))
  }

  # TODO: Make sure the project adheres to renv lockfile
  usethis::with_project(temp_path, {
    type <- desc::desc(usethis::proj_path("DESCRIPTION"))$get_field("Type")

    prepare_for_connect()

    set_envvars()

    switch(type,
      batch = try_connect_run_batch(),
      app = try_connect_run_app(),
      api = try_connect_run_api(),
      # report
      cli::cli_abort("Type {.field {type}} not supported")
    )
  })
}

try_connect_run_batch <- function() {
  rlang::check_installed("shiny")
  shiny::runApp()
}

try_connect_run_app <- function() {
  rlang::check_installed("shiny")
  shiny::runApp()
}

try_connect_run_api <- function() {
  source("script.R", verbose = FALSE)
}
