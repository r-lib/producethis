#' ---
#' title: "R script file"
#' ---
#'

scripts <- list.files("exec", full.names = TRUE, recursive = TRUE)

for (file in scripts) {
  source(file)
}
