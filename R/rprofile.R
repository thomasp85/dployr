create_and_load_profile <- function() {
  loader <- paste0(
    'proj_opts <- eval(parse(text = read.dcf("DESCRIPTION", "Settings/R")))',
    'if (is.list(proj_opts)) options(proj_opts)',
    'pkgload::load_all()',
    collapse = "\n"
  )
  cat("\n", loader, file = ".Rprofile", sep = "", append = TRUE)

  eval(parse(text = loader), envir = baseenv())
}
