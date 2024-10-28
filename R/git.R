get_git_remote <- function() {
  git_repo <- Sys.getenv('GITHUB_REPOSITORY')
  git_url <- Sys.getenv('GITHUB_SERVER_URL')
  if (git_repo != "" && git_url != "") {
    # We are likely running as a GHA
    return(list(
      host = git_url,
      repository = git_repo
    ))
  }
  # We piggyback on usethis' git logic for consistency
  settings <- getFromNamespace("target_repo", "usethis")()
  list(
    host = settings$host_url,
    repository = settings$repo_spec
  )
}
