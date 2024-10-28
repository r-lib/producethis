#' ---
#' title: "R script file"
#' ---
#'

pkgload::load_all()

scripts <- list.files("batch", full.names = TRUE, recursive = TRUE)

for (file in scripts) {
  source(file)
}
