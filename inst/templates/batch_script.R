#' ---
#' title: "R script file"
#' ---
#'

scripts <- list.files("target", full.names = TRUE, recursive = TRUE)

for (file in scripts) {
  source(file)
}
