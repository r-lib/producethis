#' Create a new production project
#'
#' This function works like [usethis::create_project()], but extends the
#' behavior to match the setup advocated by producethis, most importantly the
#' existance of a `DESCRIPTION` file with the `Type` field set to the type of
#' production job this project will be.
#'
#' @inheritParams usethis::create_project
#' @param type A string giving the type of project to create. Currently `"batch"`,
#' `"app"`, `"api"`, and `"report"` is recognized.
#' @inheritDotParams usethis::create_project -path
#'
#' @return Path to the newly created project, invisibly.
#'
#' @export
#'
create_production <- function(path, type = c("batch", "app", "api", "report"), ..., open = rlang::is_interactive(), report_type = "default") {
  type <- tolower(type[1])
  proj <- usethis::create_project(path, ..., open = FALSE)

  usethis::with_project(proj, {
    usethis::use_description(fields = list(
      Type = type,
      Suggests = paste(c(
        "devtools",
        "pak",
        "producethis"
      ), collapse = ",\n    ")
    ))

    cli::cli_bullets(c("v" = "Creating {.file .Renviron}"))
    fs::file_create(".Renviron")
    usethis::use_git_ignore(".Renviron")

    cli::cli_bullets(c("v" = "Creating {.file .Rprofile}"))
    prepare_rprofile()

    usethis::use_directory("target")

    switch(type,
      batch = {
        cli::cli_bullets(c("v" = "Creating {.file script.R}"))
        fs::file_create(fs::path("target", "script", ext = "R"))
      },
      api = {
        cli::cli_bullets(c("v" = "Creating {.file plumber.R}"))
        fs::file_create(fs::path("target", "plumber", ext = "R"))
      },
      app = {
        cli::cli_bullets(c("v" = "Creating {.file app.R}"))
        fs::file_create(fs::path("target", "app", ext = "R"))
      },
      report = {
        cli::cli_bullets(c("v" = "Setting up Quarto project"))
        quarto::quarto_create_project(type = report_type, dir = "target", no_prompt = TRUE)
      }
    )
  }, quiet = TRUE)

  if (open) {
    usethis::proj_activate(path)
  }

  invisible(proj)
}
