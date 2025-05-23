#' Create skeleton for a new project
#'
#' This function sets up the `exec/` folder according to the project type and
#' returns the names of the packages the project type requires. It is not
#' intended to be called directly but instead orchestrated by
#' `producethis::create_production()`
#'
#' @param type The project type to create. See [project_category()] for an
#' overview of the different possible types
#' @param title The title of the project. Will be used in various project types
#' to fill in titles
#' @param message_fun A function (usually `cli::cli_bullets()`) used to create
#' status messages. Passed in as an argument to avoid additional dependencies
#' for dployr
#' @param file_creator A function (usually `\(...) fs::file_create(fs::path(...))`)
#' used to create files. Passed in as an argument to acoid additional
#' dependencies
#'
#' @return A character vector giving the package dependencies for the project
#'
#' @keywords internal
#' @export
#'
create <- function(
  type,
  title = "default",
  message_fun = function(...) NULL,
  file_creator = function(...) NULL
) {
  switch(
    tolower(type),

    # Dynamic projects
    shiny = {
      message_fun(c("v" = "Creating {.file exec/app.R}"))
      file_creator("exec", "app", ext = "R")

      "shiny"
    },
    plumber = {
      message_fun(c("v" = "Creating {.file exec/plumber.R}"))
      file_creator("exec", "plumber", ext = "R")

      "plumber"
    },
    plumber2 = {
      require_package("plumber2")
      message_fun(c("v" = "Creating {.file exec/main_router.R}"))
      file_creator("exec", "script", ext = "R")
      message_fun(c("v" = "Creating {.file exec/_server.yml}"))
      plumber2::create_server_yml("exec", path = "exec")

      "plumber2"
    },
    dynamic = {
      message_fun(c("v" = "Creating empty {.file exec/_server.yml}"))
      writeLines("engine: ", "exec/_server.yml")

      character(0)
    },
    rmd_shiny = {
      message_fun(c("v" = "Creating {.file exec/index.Rmd}"))
      writeLines(c(
        "---",
        paste0("title: ", title),
        "runtime: shiny",
        "---"
      ), "exec/index.Rmd", sep = "\n")

      c("rmarkdown", "shiny")
    },
    quarto_shiny = {
      message_fun(c("v" = "Creating {.file exec/index.qmd}"))
      writeLines(c(
        "---",
        paste0("title: ", title),
        "server: shiny",
        "---"
      ), "exec/index.qmd", sep = "\n")

      c("quarto", "shiny")
    },

    # Static projects
    book = ,
    website = ,
    blog = ,
    manuscript = ,
    quarto = {
      require_package("quarto")
      if (type == "quarto") type <- "default"
      message_fun(c("v" = "Setting up Quarto {type} project"))
      quarto::quarto_create_project(title, type = type, dir = "exec", no_prompt = TRUE)

      "quarto"
    },
    report = {
      message_fun(c("v" = "Creating {.file exec/index.qmd}"))
      writeLines(c(
        "---",
        paste0("title: ", title),
        "---"
      ), "exec/index.qmd", sep = "\n")

      "quarto"
    },
    rmd_report = {
      message_fun(c("v" = "Creating {.file exec/index.Rmd}"))
      writeLines(c(
        "---",
        paste0("title: ", title),
        "---"
      ), "exec/index.Rmd", sep = "\n")

      "rmarkdown"
    },
    rmd_website = {
      message_fun(c("v" = "Creating {.file exec/index.Rmd}"))
      writeLines(c(
        "---",
        paste0("title: ", title),
        "---"
      ), "exec/index.Rmd", sep = "\n")
      message_fun(c("v" = "Creating {.file exec/_site.yml}"))
      writeLines(c(
        paste0("name: ", title)
      ), "exec/_site.yml", sep = "\n")

      "rmarkdown"
    },
    rmd_book = {
      require_package("bookdown")
      bookdown::create_bs4_book("exec")

      c("bookdown", "rmarkdown")
    },
    rmd_blog = {
      require_package("blogdown")
      message_fun(c("v" = "Creating blogdown site skeleton"))
      blogdown::new_site("exec", netlify = FALSE, serve = FALSE)

      c("blogdown", "rmarkdown")
    },
    static = {
      message_fun(c("v" = "Creating {.file exec/static.R}"))
      file_creator("exec", "static", ext = "R")

      character(0)
    },
    script = ,
    batch = {
      message_fun(c("v" = "Creating {.file exec/script.R}"))
      file_creator("exec", "script", ext = "R")

      character(0)
    },
    stop("Unknown project type: '", type, "'. Aborting execution")
  )
}
