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
create_production <- function(path, type = c("batch", "app", "api", "report"), ...) {
  type <- tolower(type[1])
  proj <- usethis::create_project(path, ...)

  usethis::with_project(proj, {
    usethis::use_description(fields = list(
      Type = type,
      Suggests = paste(c(
        "devtools",
        "pak",
        "producethis"
      ), collapse = ",\n    ")
    ))

    switch(type[1],
      batch = usethis::use_directory("batch"),
      api = usethis::use_directory("API")
    )
  }, quiet = TRUE)

  invisible(proj)
}
