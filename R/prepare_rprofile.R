prepare_rprofile <- function() {
  file <- usethis::proj_path(".Rprofile")
  if (!fs::file_exists(file)) {
    fs::file_create(file)
  }
  cat('proj_opts <- eval(parse_expr(read.dcf("DESCRIPTION", "Settings/R")))\n', file = file, append = TRUE)
  cat('if (is.list(proj_opts) options(proj_opts)\n', file = file, append = TRUE)

  cat("\n\npkgload::load_all()\n", file = file, append = TRUE)
}
