#' ---
#' title: "R api file"
#' ---
#'

pkgload::load_all()

plumber::plumb(dir = "API/") |>
  plumber::pr_set_api_spec(
    function(spec) {
      spec$info$title <- "{{title}}"
      spec$info$description <- "{{description}}"
      spec$info$version <- "{{version}}"
      spec$info$contact <- list(
        name = "{{name}}",
        url = "{{url}}",
        email = "{{email}}"
      )
      spec
    }
  )

