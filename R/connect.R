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
    # app
    # api
    # report
    stop("Type not supported")
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
  client <- connectapi::connect()
  repo <- get_git_remote()
  git_url <- glue("{repo$host}/{repo$repository}")
  guid <- get_repo_guid(client, git_url)
  is_published <- !is.null(guid)
  if (!is_published) {
    content <- connectapi::deploy_repo(
      client,
      repository = git_url,
      branch = branch,
      name = basename(repo)
    )
  } else {
    content <- connectapi::content_item(client, guid)
  }

  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))

  # Set standard info: Name, Title, Description
  connectapi::content_update(content, name = desc$get_field("Package"), title = desc$get_field("Title"), description = desc$get_field("Description"))

  # Set vanity URL
  if (desc$has_fields("URL")) {
    url <- url <- desc$get_field("URL")
    url <- sub(paste0(client$server, "/"), "", url, fixed = TRUE)
    connectapi::set_vanity_url(content, url)
  } else {
    connectapi::delete_vanity_url(content)
  }

  # Set tags
  if (desc$has_fields("Tags")) {
    tags <- desc$get_field("Tags")
    current_tags <- content$tags()
    lapply(current_tags, function(tag) {
      content$tag_delete(tag$id)
    })
    lapply(tags, function(tag) {
      tag <- strsplit(tag, ";\\s*")[[1]]
      connectapi::set_content_tag_tree(content, !!!tag)
    })
  }

  # Set additional stuff related to the environment using content_update

  # Set scheduling info

  connectapi::deploy_repo_update(content)
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
