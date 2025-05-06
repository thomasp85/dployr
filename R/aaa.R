require_package <- function(package, message = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      package,
      " ",
      message %||% "must be available for project execution",
      call. = FALSE
    )
  }
}
