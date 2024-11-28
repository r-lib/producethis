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
  # Make sure the current project is set
  usethis::proj_get()

  temp_path <- fs::path_temp("connect")

  fs::dir_copy(".", temp_path)

  if (clean) {
    on.exit(fs::dir_delete(temp_path))
  }

  # TODO: Make sure the project adheres to renv lockfile
  usethis::with_project(temp_path, {
    type <- tolower(desc::desc(usethis::proj_path("DESCRIPTION"))$get_field("Type"))
    envvars <- as.character(unlist(get_envvars()))

    prepare_for_connect()

    sink <- fs::file_temp()
    on.exit(fs::file_delete(sink), add = TRUE)

    process <- switch(type,
      batch = callr::r(\() utils::getFromNamespace("try_connect_run_batch", "producethis")(), env = unlist(envvars)),
      app = callr::r_bg(\() utils::getFromNamespace("try_connect_run_app", "producethis")(), env = unlist(envvars), stdout = sink, stderr = sink),
      api = callr::r_bg(\() utils::getFromNamespace("try_connect_run_api", "producethis")(), env = unlist(envvars), stdout = sink, stderr = sink),
      # report
      cli::cli_abort("Type {.field {type}} not supported")
    )

    if (type %in% c("app", "api")) {
      on.exit(process$kill(), add = TRUE, after = FALSE)

      while(TRUE) {
        if (!process$is_alive()) {
          return(process$get_result())
        }
        output <- readLines(sink)
        if (type == "app" && any(grepl("Listening on ", output))) {
          break
        }
        if (type == "api" && any(grepl("Running plumber API ", output))) {
          break
        }
      }

      url <- "http://127.0.0.1:3760"
      if (type == "api") url <- paste0(url, "/__docs__/")
      utils::browseURL(url)

      skip <- 0
      while(TRUE) {
        output <- readLines(sink)
        if (skip != 0) output <- output[-seq_len(skip)]
        if (length(output != 0)) {
          skip <- skip + length(output)
          cat(output, sep = "\n")
        }
      }
    }
  })
}

try_connect_run_batch <- function() {
  source("script.R", verbose = FALSE)
}

try_connect_run_app <- function() {
  rlang::check_installed("shiny")
  shiny::runApp(host = "127.0.0.1", port = 3760)
}

try_connect_run_api <- function() {
  rlang::check_installed("plumber")
  plumber::plumb() |> plumber::pr_run(host = "127.0.0.1", port = 3760)
}
