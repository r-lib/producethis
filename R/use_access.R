#' Set access rights on your project
#'
#' This function is used to record the permission levels for the deployment.
#' The validity of the given values depend on the deployment type and the
#' users/groups that have been registered there. It will only be checked at
#' deployment time. In general, permission is understood as "viewing right",
#' since editing all should happen through the git repository anyway.
#'
#' @param users A character vector of user names that should have access to the
#' deployment
#' @param groups A character vector of group names that should have access to
#' deployment
#' @param global A global setting for permissions. For Posit Connect this can
#' be `"all"` or `"logged-in"`.
#'
#' @return The function is called for its side effects.
#'
#' @export
#'
use_access <- function(users = NULL, groups = NULL, global = NULL) {
  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  if (!is.null(global)) {
    desc$set("Access/Global" = global)
  } else {
    desc$del("Access/Global")
  }
  if (!is.null(users)) {
    if (desc$has_fields("Access/Users")) {
      users <- unique(c(trimws(strsplit(desc$get_field("Access/Users"))[[1]]), users))
    }
    desc$set("Access/Users" = paste(users, collapse = ",\n    "))
  }
  if (!is.null(groups)) {
    if (desc$has_fields("Access/Groups")) {
      groups <- unique(c(trimws(strsplit(desc$get_field("Access/Groups"))[[1]]), groups))
    }
    desc$set("Access/Groups" = paste(groups, collapse = ",\n    "))
  }
  desc$write()
}
