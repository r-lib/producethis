#' Create a new production project
#'
#' This function works like [usethis::create_project()], but extends the
#' behavior to match the setup advocated by producethis, most importantly the
#' existance of a `DESCRIPTION` file with the `Type` field set to the type of
#' production job this project will be.
#'
#' @inheritParams usethis::create_project
#' @param type A string giving the type of project to create. See
#' [dployr::project_category()] for a description of each type.
#' @inheritDotParams usethis::create_project -path
#' @param report_type If `type` is `"report"`, what kind of report
#'
#' @return Path to the newly created project, invisibly.
#'
#' @export
#'
create_production <- function(path, type, ..., open = rlang::is_interactive()) {
  type <- tolower(type[1])
  category <- try_fetch(
    dployr::project_category(type),
    error = function(...) {
      cli::cli_abort("Unknown type {.val {type}}. Aborting")
    }
  )
  proj <- usethis::create_project(path, ..., open = FALSE)

  usethis::with_project(proj, {
    usethis::use_directory("exec")

    deps <- dployr::create(
      type,
      basename(path),
      cli::cli_bullets,
      function(...) fs::file_create(fs::path(...))
    )

    usethis::use_description(fields = list(
      Type = type,
      Imports = c(deps, "dployr"),
      Suggests = paste(c(
        "devtools",
        "pak",
        "producethis"
      ), collapse = ",\n    ")
    ), check_name = FALSE, roxygen = FALSE)

    cli::cli_bullets(c("v" = "Creating {.file .Renviron}"))
    fs::file_create(".Renviron")
    usethis::use_git_ignore(".Renviron")

    usethis::use_template("main.R", "main.R", package = "producethis")
  }, quiet = TRUE)

  if (open) {
    usethis::proj_activate(path)
  }

  invisible(proj)
}
