get_type <- function(desc) {
  if (is.null(desc$Type)) {
    stop("'DESCRIPTION' missing 'Type' field. Aborting execution")
  }
  switch(
    tolower(desc$Type),
    shiny = ,
    plumber = ,
    api = ,
    app = ,
    rmd_shiny = ,
    quarto_shiny = ,
    dynamic = "dynamic",
    quarto = ,
    rmarkdown = ,
    book = ,
    website = ,
    site = ,
    blog = ,
    report = ,
    document = ,
    doc = ,
    static = "static",
    script = ,
    batch = "batch",
    stop("Unknown project type: '", desc$Type, "'. Aborting execution")
  )
}
