get_type <- function(desc) {
  if (is.null(desc$Type)) {
    stop("'DESCRIPTION' missing 'Type' field. Aborting execution")
  }
  project_category(desc$Type)
}

#' Get the main category of a project based on its type
#'
#' Return the category assiciated with a given project type.
#'
#' @param type The type name of the project, as provided in the `Type` field of
#' the project
#'
#' ## Types and categories
#' producethis/dployr recognises three main project types:
#'
#' * `static`: The project is executed and as a result produces one or more
#'   files which will subsequently be made availabe
#' * `dynamic`: The project is executed and as a result starts a web server with
#'   dynamic content
#' * `batch`: The project is executed and is afterwards considered "finished".
#'   If information is generated it needs to be stored somewhere else.
#'
#' Inside these categories are a range of types that maps to specific runtimes
#' or execution engines. The project type is given in the `Type` field of the
#' `DESCRIPTION` file
#'
#' ### static
#' * `quarto`: Used for a default quarto project. Will use
#'   `quarto::quarto_render()` to render the content of `exec/`
#' * `report`: Used for a single quarto document. Will use
#'   `quarto::quarto_render()` to render the `.qmd` file in `exec/`
#' * `website`: Used for a quarto website project. Will use
#'   `quarto::quarto_render()` to render the content of `exec/`
#' * `blog`: Used for a quarto blog project. Will use
#'   `quarto::quarto_render()` to render the content of `exec/`
#' * `book`: Used for a quarto book project. Will use
#'   `quarto::quarto_render()` to render the content of `exec/`
#' * `manuscript`: Used for a quarto manuscript project. Will use
#'   `quarto::quarto_render()` to render the content of `exec/`
#' * `rmd_report`: Used for a single RMarkdown document. Will use
#'   `rmarkdown::render()` to render the `.Rmd` file in `exec/`
#' * `rmd_website` Used for general Rmarkdown websites. Will use
#'   `rmarkdown::render_site()` to render the content of `exec/`
#' * `rmd_blog` Used for a blogdown project. Will use `blogdown::build_site()` to
#'   render the content of `exec/`
#' * `rmd_book` Used for a bookdown project. Will use `bookdown::render_book()` to
#'   render the content of `exec/`
#' * `static`: Used for a generalized static project. Will execute the R files
#'   in `exec/` and expect the content to be served to be placed according to
#'   `distDir` in the deployment [config]
#'
#' ### dynamic
#' * `shiny`: Used for a shiny app. Will use `shiny::shinyAppDir()` to create a
#'   shiny app from `exec/` and then launch it with `shiny::runApp()`
#' * `plumber`: Used for a plumber API. Will use `plumber::plumb()` to create an
#'   api from `exec/` and launch it with `plumber::pr_run()`
#' * `plumber2`: Used for a plumber2 API. Are treated like `api`/`app` in that
#'   it requires a `_server.yml` file that defines its execution
#' * `dynamic`: Used for a generalized app or api. Will require a
#'   `_server.yml` file to specify how to launch it
#' * `rmd_shiny`: Used for a shiny-backed Rmarkdown document. Will use
#'   `rmarkdown::run()` to render and serve a dynamic document
#' * `quarto_shiny`: Used for a shiny-backed Quarto document. Will use
#'   `quarto::quarto_serve()` to render and serve a dynamic document.
#'
#' ### batch
#' * `batch`: Batch project which will run each script in `exec/` in turn and
#'   then quit.
#'
#' @export
#'
project_category <- function(type) {
  switch(
    tolower(type),
    shiny = ,
    plumber = ,
    plumber2 = ,
    rmd_shiny = ,
    quarto_shiny = ,
    dynamic = "dynamic",
    quarto = ,
    manuscript = ,
    book = ,
    rmd_book = ,
    website = ,
    rmd_website = ,
    blog = ,
    rmd_blog = ,
    report = ,
    rmd_report = ,
    static = "static",
    script = ,
    batch = "batch",
    stop("Unknown project type: '", type, "'. Aborting execution")
  )
}
