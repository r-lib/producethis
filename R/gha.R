#' Deploy a project using GitHub actions
#'
#' While all deployments based on producethis uses GitHub actions, this
#' deployment type uses it exclusively, meaning that the code is also executed
#' in an action. This puts some limits on the kind of project that can be
#' deployed, since GHA doesn't provide a web server. Thus, only batch and static
#' reports can be deployed in this way.
#'
#' @inheritParams use_github_connect
#' @param result_branch The branch to push the content of `output_dir` to after
#' the execution ends
#' @param output_dir The directory any output from the execution is expected to
#' end up in. Do note that it is the responsibility of the production code to
#' actually write to this folder
#'
#' @return This function is called for its side effects
#' @export
#'
use_github_gha <- function(from = c("main", "master"), result_branch = "gh-results", output_dir = "output") {
  fs::dir_create(fs::path(".github", "workflows"))
  # TODO: Add support for publishing from tags
  usethis::use_template(
    "gha.yaml",
    ".github/workflows/gha.yaml",
    package = "producethis",
    data = list(
      branch = paste(from, collapse = ", ")
    )
  )
  use_gha_settings(result_branch = result_branch, output_dir = output_dir)
}

#' Prepares a production project for deployment on GitHub Actions
#'
#' This function generates all the necessary files for your project to be
#' executed on GitHub Actions. It is expected that this function
#' is not called interactively, but rather as part of an automation using e.g.
#' GitHub Actions. This function will create an action
#'
#' @return This function is called for its sideeffects
#' @export
prepare_for_gha <- function() {
  type <- desc::desc(usethis::proj_path("DESCRIPTION"))$get_field("Type")
  switch(
    tolower(type),
    batch = prepare_for_gha_batch(),
    # report
    cli::cli_abort("Type {.field {type}} not supported for GitHub Actions deployment")
  )
  create_execution_action()
}

create_execution_action <- function() {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  has_schedule <- desc$has_fields("Schedule")
  schedule <- if (has_schedule) get_cron_schedule() else NULL
  envvars <- eval_from_desc(desc, "Envvar")
  envvars <- Map(\(x, y) list(name = x, value = y), x = names(envvars), y = envvars)
  settings <- eval_from_desc(desc, "Settings/gha")
  usethis::use_template(
    "gha_execute.yaml",
    ".github/workflows/gha_execute.yaml",
    package = "producethis",
    data = list(
      result_branch = settings$result_branch %||% "gh-results",
      output_dir = settings$output_dir %||% "output",
      has_schedule = has_schedule,
      has_push = !has_schedule,
      schedule = schedule,
      envvars = envvars,
      secrets = settings$secrets
    )
  )
}

prepare_for_gha_batch <- function() {
  prepare_for_connect_batch()
}
