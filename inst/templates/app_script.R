#' ---
#' title: "R app file"
#' ---
#'

# Prevent shiny::runApp from trying to source files in R/
options(shiny.autoload.r = FALSE)

pkgload::load_all()

{{app}}()
