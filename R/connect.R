#' Publish to Posit Connect from a github repository
#'
#' This function sets up the `connect.yaml` action that powers the connect
#' deployment from github. Further, it takes the local `CONNECT_SERVER` and
#' `CONNECT_API_KEY` environment variables and, if they exist, sets them as
#' secrets on the remote, for the action to use.
#'
#' @param from A character vector giving which branch(es) to publish from
#'
#' @return This function is called for its side effects
#'
#' @export
#'
use_github_connect <- function(from = c("main", "master")) {
  fs::dir_create(fs::path(".github", "workflows"))
  # TODO: Add support for publishing from tags
  usethis::use_template(
    "connect.yaml",
    ".github/workflows/connect.yaml",
    package = "producethis",
    data = list(branch = paste(from, collapse = ", "))
  )
  remote <- get_git_remote()

  key <- get_github_public_key(remote$repository, remote$host)

  success <- vapply(c("CONNECT_SERVER", "CONNECT_API_KEY"), function(x) {
    set_github_secret(x, remote$repository, remote$host, key)
  }, logical(1))

  if (any(!success)) {
    cli::cli_bullets(c(
      "Navigate to {.url {remote$host}/{remote$repository}/settings/secrets/actions}",
      "!" = "Set the {.field CONNECT_SERVER} secret to the URL of the Posit Connect server to deploy to",
      "!" = "Set the {.field CONNECT_API_KEY} secret to your API key for the server"
    )[c(TRUE, !success)])
  }
}

#' Prepares a production project for deployment on connect
#'
#' This function generates all the necessary files for your project to be
#' deployed on connect, most importantly the `manifest.json` file, as well as
#' the orchestration file that runs your code. It is expected that this function
#' is not called interactively, but rather as part of an automation using e.g.
#' GitHub Actions.
#'
#' @return This function is called for its sideeffects
#' @export
prepare_for_connect <- function() {
  type <- desc::desc(usethis::proj_path("DESCRIPTION"))$get_field("Type")
  switch(
    tolower(type),
    batch = prepare_for_connect_batch(),
    app = prepare_for_connect_app(),
    api = prepare_for_connect_api(),
    # report
    cli::cli_abort("Type {.field {type}} not supported")
  )
}

#' Deploys and updates project on Connect
#'
#' This function is called once the project is ready for deployment, having
#' already called [prepare_for_connect()] and pushed to the deployment branch.
#' It is highly recommended that you do not deploy from your local machine but
#' rather from a versioned repository such as GitHub.
#'
#' @param branch The branch to deploy from. It is expected that it exists and
#' contain the necessary files for Connect
#'
#' @return This function is called for its sideeffects
#' @export
deploy_repo_to_connect <- function(branch = "gh-connect") {
  server <- Sys.getenv("CONNECT_SERVER", NA_character_)
  if (is.na(server)) {
    cli::cli_abort(c(
      "No Posit Connect server provided",
      i = "Please set the {.field CONNECT_SERVER} environment variable to the deployment server url"
    ))
  }
  cli::cli_bullets(c(">" = "Connecting to Posit Connect at {.url {server}}"))
  try_fetch(
    client <- connectapi::connect(),
    error = function(cnd, ...) {
      cli::cli_abort("Failed to connect to {.url {server}}", parent = cnd)
    }
  )

  cli::cli_bullets(c(">" = "Determining Git remote location"))
  repo <- get_git_remote()
  git_url <- glue("{repo$host}/{repo$repository}")
  cli::cli_bullets(c(v = "Found remote at {.url {git_url}}"))

  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))

  cli::cli_bullets(c(">" = "Looking for already deployed version"))
  guid <- get_repo_guid(client, git_url)
  is_published <- !is.null(guid)
  if (!is_published) {
    cli::cli_bullets(c(i = "Project is undeployed. Deploying now"))
    content <- connectapi::deploy_repo(
      client,
      repository = git_url,
      branch = branch,
      subdirectory = ".",
      name = desc$get_field("Package")
    )
  } else {
    cli::cli_bullets(c(v = "Found deployed project with guid: {guid}"))
    content <- connectapi::content_item(client, guid)
  }

  # Set standard info: Name, Title, Description
  cli::cli_bullets(c(">" = "Updating project name, title, and description based on DESCRIPTION file"))
  connectapi::content_update(content, name = desc$get_field("Package"), title = desc$get_field("Title"), description = desc$get_field("Description"))

  # Set vanity URL
  if (desc$has_fields("URL")) {
    url <- trimws(strsplit(desc$get_field("URL"), ",", fixed = TRUE)[[1]])
    url <- url[grep(client$server, url)]
    if (length(url) == 0) {
      cli::cli_bullets(c(">" = "Unsetting vanity URL"))
      connectapi::delete_vanity_url(content)
    } else {
      url <- sub(paste0(client$server, "/"), "", url[1], fixed = TRUE)
      cli::cli_bullets(c(">" = "Setting vanity URL to {.field url}"))
      connectapi::set_vanity_url(content, url)
    }
  } else {
    cli::cli_bullets(c(">" = "Unsetting vanity URL"))
    connectapi::delete_vanity_url(content)
  }

  # Set tags
  cli::cli_bullets(c(">" = "Updating project tags"))
  current_tags <- content$tags()
  lapply(current_tags, function(tag) {
    content$tag_delete(tag$id)
  })
  if (desc$has_fields("Tags")) {
    tags <- desc$get_field("Tags")
    lapply(tags, function(tag) {
      tag <- strsplit(tag, ";\\s*")[[1]]
      connectapi::set_content_tag_tree(content, !!!tag)
    })
  }

  # Set additional stuff related to the environment using content_update
  cli::cli_bullets(c(">" = "Updating deployment settings"))
  settings <- get_connect_defaults()
  local_settings <- eval_from_desc(desc, "Settings/Connect")
  validate_connect_settings(local_settings)
  settings <- utils::modifyList(settings, local_settings)
  connectapi::content_update(content, !!!settings)

  # Set environment variables
  # TODO: We need to make sure these values are propagated to user session and GHA
  cli::cli_bullets(c(">" = "Updating environment variables"))
  env <- connectapi::get_environment(content)
  vars <- eval_from_desc(desc, "Envvar")
  if (length(vars) != 0 && (!is.list(vars) || is.null(names(vars)) || any(!vapply(vars, is.character, logical(1))) || any(lengths(vars) != 1))) {
    cli::cli_abort("{.field Envvar} must contain a named list of strings")
  }
  connectapi::set_environment_all(env, !!!vars)

  # Set scheduling info
  cli::cli_bullets(c(">" = "Updating scheduling"))
  schedule <- get_connect_schedule()
  if (tolower(desc$get_field("Type")) %in% c("script", "report")) {
    cur_schedule <- connectapi::get_variant_schedule(connectapi::get_variant_default(content))
    connectapi::set_schedule_remove(cur_schedule)
    if (!is.null(schedule)) {
      connectapi::set_schedule(cur_schedule, !!!schedule)
    }
  } else if (!is.null(schedule)) {
    cli::cli_bullets("!" = "Ignoring schedule for {.field {desc$get_field('Type')}} deployments")
  }

  # Set access
  cli::cli_bullets(c(">" = "Updating access permissions"))
  if (desc$has_fields("Access/Global")) {
    global <- desc$get_field("Access/Global")
    global_sanitized <- gsub("\\W", "_", tolower(global))
    allowed <- c("all", "logged_in", "acl")
    if (!global_sanitized %in% allowed) {
      cli::cli_abort(c(
        "Unknown Global Access type {.field {global}}",
        i = "Use one of {.or {.field {allowed}}}"
      ))
    }
    connectapi::content_update_access_type(content, global_sanitized)
  } else {
    connectapi::content_update_access_type(content, "acl")
  }
  current_access <- connectapi::get_content_permissions(content, FALSE)
  for (i in seq_len(nrow(current_access))) {
    switch(current_access$principal_type[i],
      user = connectapi::content_delete_user(content, current_access$principal_guid[[i]]),
      group = connectapi::content_delete_group(content, current_access$principal_guid[[i]])
    )
  }
  if (desc$has_fields("Access/Users")) {
    users <- trimws(strsplit(desc$get_field("Access/Users"), ",")[[1]])
    for (user in users) {
      user <- try_fetch(
        connectapi::user_guid_from_username(client, user),
        error = function(...) {
          cli::cli_abort("Unknown Connect user: {.field {user}}")
        }
      )
      connectapi::content_add_user(content, user)
    }
  }
  if (desc$has_fields("Access/Groups")) {
    groups <- trimws(strsplit(desc$get_field("Access/Groups"), ",")[[1]])
    for (group in groups) {
      group_search <- client$GET("v1/groups", query = list(prefix = group))
      search_names <- vapply(group_search$results, `[[`, character(1), "name")
      if (length(search_names) == 0 || !any(group == search_names)) {
        cli::cli_abort("Unknown Connect group: {.field {group}}")
      }
      group <- group_search$results[[which(group == search_names)]]$guid
      connectapi::content_add_group(content, group)
    }
  }

  # Set image thumbnail
  cli::cli_bullets(c(">" = "Updating thumbnail"))
  if (fs::file_exists(usethis::proj_path("logo.png"))) {
    connectapi::set_image_path(content, usethis::proj_path("logo.png"))
  } else if (connectapi::has_image(content)) {
    connectapi::delete_image(content)
  }

  # Pushing to Connect
  cli::cli_bullets(c(">" = "Requesting Posit Connect to fetch from remote"))
  connectapi::deploy_repo_update(content)
  cli::cli_bullets(c(v = "Project deployed"))
}

is_deploying_on_connect <- function() {
  as.vector(fs::file_exists(usethis::proj_path(".github", "workflows", "connect", ext = "yaml")))
}

get_repo_guid <- function(client, git_url) {
  matches <- client$GET("v1/search/content", query=list(q=git_url))

  if (matches$total == 0) return(NULL)

  for (match in matches$results) {
    match_url <- client$GET(glue("applications/{match$guid}"))$git$repository_url
    if (tolower(match_url) == tolower(git_url)) return(match$guid)
  }

  NULL
}

prepare_for_connect_batch <- function() {
  usethis::use_template("batch_script.R", "script.R", package = "producethis")
  rsconnect::writeManifest(appPrimaryDoc = "script.R", appMode = "quarto-static")
}

prepare_for_connect_app <- function() {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  if (desc$has_fields("AppFun")) {
    app_fun <- desc$get_field("AppFun")
  } else {
    app_fun <- "app"
  }
  usethis::use_template(
    "app_script.R",
    "app.R",
    package = "producethis",
    data = list(app = app_fun)
  )

  fs::file_create(usethis::proj_path("R", "_disable_autoload", ext = "R"))

  rsconnect::writeManifest(appMode = "shiny")
}

prepare_for_connect_api <- function() {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  usethis::use_template(
    "api_script.R",
    "entrypoint.R",
    package = "producethis",
    data = list(
      title = desc$get_field("Title"),
      description = desc$get_field("Description"),
      version = desc$get_version(),
      name = paste(desc$get_author()$given, desc$get_author()$family),
      email = desc$get_author()$email,
      url = desc$get_field("BugReports")
    )
  )
  rsconnect::writeManifest(appMode = "api")
}

proj_uses_connect <- function() {
  unname(fs::file_exists(usethis::proj_path(".github", "workflows", "connect", ext = "yaml")))
}
