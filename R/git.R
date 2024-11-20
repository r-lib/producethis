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
  settings <- utils::getFromNamespace("target_repo", "usethis")()
  list(
    host = settings$host_url,
    repository = settings$repo_spec
  )
}

get_github_public_key <- function(repository, host, call = caller_env()) {
  host <- sub("^https?://", "", host)
  key <- httr2::request(glue::glue("https://api.{host}/repos/{repository}/actions/secrets/public-key")) |>
    httr2::req_method("GET") |>
    httr2::req_auth_bearer_token(gh::gh_token()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (!all(names(key) %in% c("key_id", "key"))) {
    cli::cli_abort("Failed to get the public key for {.field {repository}}", call = call)
  }

  key
}

set_github_secret <- function(secret, repository, host, key) {
  host_base <- sub("^https?://", "", host)

  secret_val <- Sys.getenv(secret)

  if (secret_val == "") {
    return(FALSE)
  }

  secret_val <- sodium::simple_encrypt(
    charToRaw(secret_val),
    base64enc::base64decode(key$key)
  )
  secret_val <- base64enc::base64encode(secret_val)

  httr2::request(glue::glue("https://api.{host_base}/repos/{repository}/actions/secrets/{secret}")) |>
    httr2::req_method("PUT") |>
    httr2::req_auth_bearer_token(gh::gh_token()) |>
    httr2::req_body_json(list(
      key_id = key$key_id,
      encrypted_value = secret_val
    )) |>
    httr2::req_perform()

  cli::cli_bullets(c(v = "Setting {.field {secret}} secret on {host}/{repository}"))
  TRUE
}
