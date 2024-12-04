eval_from_desc <- function(desc, field) {
  if (!desc$has_fields((field))) {
    return(NULL)
  }
  expr <- parse_expr(desc$get_field(field))
  check_safe_expr(expr)
  eval_bare(expr, getNamespace("producethis"))
}

check_safe_expr <- function(x) {
  if (is_syntactic_literal(x) || is_symbol(x)) {
    return()
  }
  if (is_call(x)) {
    if (!is_safe_call(x[[1]])) {
      cli::cli_abort("{.fun {x[[1]]}} is not a permitted call in {.file DESCRIPTION}")
    }
    for (i in seq_along(x)[-1]) {
      check_safe_expr(x[[i]])
    }
  }
}

is_safe_call <- function(x) {
  if (any(x == parse_exprs(c("list", "c", "person", "factor")))) {
    return(TRUE)
  }
  if (any(x == parse_exprs(getNamespaceExports("producethis")))) {
    return(TRUE)
  }
  return(FALSE)
}

write_list_to_desc <- function(desc, field, x) {
  cur_settings <- eval_from_desc(desc, field) %||% list()
  if (!is_bare_list(cur_settings)) {
    cli::cli_warn("Ignoring malformed {.field {field}} in {.file DESCRIPTION}")
    cur_settings <- list()
  }
  x <- utils::modifyList(cur_settings, as.list(x))

  x <- call_args(parse_expr(deparse1(x)))
  args <- lapply(x, deparse1)
  arg_names <- vapply(names(x), function(x) capture.output(as.symbol(x)), character(1))

  x <- glue::glue_collapse(
    glue::glue("      {arg_names}{ifelse(nchar(arg_names) != 0, ' = ', '')}{args}"),
    sep = ",\n"
  )
  x <- glue::glue("\n    list(\n{x}\n    )", .trim = FALSE)
  inject(desc$set(!!!list2(!!field := x)))
  desc$write()
  invisible()
}
