#' Execute a producethis project
#'
#' This function assumes the current working directory is the root of a project
#' created according to producethis conventions. If this is the case it will
#' execute the project according to its type and the content in `./exec`
#'
#' @return This function is called for its side effects
#'
#' @export
#'
execute <- function() {
  if (!file.exists("DESCRIPTION")) {
    stop("No 'DESCRIPTION' file. Aborting execution")
  }
  if (!dir.exists("exec")) {
    stop("No './exec' directory. Aborting execution")
  }

  create_and_load_profile()

  desc <- as.list(read.dcf("DESCRIPTION")[1, ])
  type <- get_type(desc)
  if (!is.null(desc$ExecFun)) {
    execute_fun(desc, type)
  } else if (file.exists("exec/_server.yml")) {
    execute_server(desc, type)
  } else {
    switch(
      type,
      batch = execute_batch(desc),
      dynamic = execute_dynamic(desc),
      static = execute_static(desc)
    )
  }
}

execute_fun <- function(desc) {
  exec_fun <- get0(desc$ExecFun, mode = "function")
  if (is.null(exec_fun)) {
    stop(
      "`",
      desc$ExecFun,
      "()` could not be found. Aborting execution",
      call. = FALSE
    )
  }
  message("Executing `", desc$ExecFun, "()`")
  exec_fun()
}

execute_server <- function(desc) {
  srvyml <- "exec/_server.yml"
  engine <- grep(
    "^engine:",
    readLines(srvyml),
    ignore.case = TRUE,
    value = TRUE
  )
  engine <- trimws(sub("^engine:(.*)", "\\1", engine, ignore.case = TRUE))

  require_package(
    engine,
    "is not available. The package specified as `engine` in `_server.yml` must be installed"
  )

  engine_fun <- get0("launch_server", asNamespace(engine), mode = "function")
  if (is.null(engine_fun)) {
    stop(
      "The `",
      engine,
      "` package does not contain a `launch_server()` function",
      call. = FALSE
    )
  }

  message("Creating ", type, " based on `./exec/_server.yml`")
  engine_fun(srvyml)
}

execute_batch <- function(desc) {
  if (!is.null(desc$ExecFun)) {
    execute_fun(desc)
  } else {
    scripts <- list.files("exec", full.names = TRUE, recursive = TRUE)
    scripts <- scripts[grep("\\.R$", scripts, ignore.case = TRUE)]
    message(
      "Project contains ",
      length(scripts),
      " script",
      if (length(scripts) > 1) "s"
    )
    for (script in scripts) {
      message("Executing '", script, "'")
      source(script)
    }
  }
}

execute_dynamic <- function(desc) {
  options(shiny.autoload.r = FALSE)
  # TODO: How does port, host, etc get passed from execution env to this call

  if (!is.null(desc$ExecFun)) {
    execute_fun(desc)
  } else if (file.exists("exec/_server.yml")) {
    execute_server(desc)
  } else if (tolower(desc$Type) == "shiny") {
    require_package("shiny")

    message("Creating app from content of `./exec`")

    shiny::runApp(
      shiny::shinyAppDir("exec/"),
      launch.browser = FALSE
    )
  } else if (tolower(desc$Type) == "plumber") {
    require_package("plumber")

    if (!is.null(desc$`Authors@R`)) {
      contact <- eval(parse(text = desc$`Authors@R`))
      contact <- contact[vapply(
        contact,
        function(p) "cre" %in% p$role,
        logical(1)
      )]
    } else {
      contact <- as.person(desc$Maintainer)
    }

    plumber::plumb(dir = "exec/") |>
      plumber::pr_set_api_spec(
        function(spec) {
          spec$info$title <- desc$Title
          spec$info$description <- desc$Description
          spec$info$version <- desc$Version
          spec$info$contact <- list(
            name = paste(contact$given, contact$family),
            url = desc$BugReports,
            email = contact$email
          )
          spec
        }
      ) |>
      plumber::pr_run()
  } else if (tolower(desc$Type) == "rmd_shiny") {
    require_package("rmarkdown")
    require_package("shiny")

    rmarkdown::run(
      file = NULL,
      dir = "exec"
    )
  } else if (tolower(desc$Type) == "quarto_shiny") {
    require_package("quarto")
    require_package("shiny")

    quarto::quarto_serve(
      "exec/index.qmd",
      browse = FALSE
    )
  } else {
    # Shouldn't happen
    stop("Unknown dynamic project type: ", desc$Type)
  }
}

execute_static <- function(desc) {
  output <- NULL
  is_quarto <- file.exists("exec/_quarto.yml") ||
    tolower(desc$Type) == "quarto" ||
    any(grepl("\\.qmd$", list.files("exec"), ignore.case = TRUE))

  if (is_quarto) {
    require_package("quarto")

    if (!file.exists("exec/_quarto.yml")) {
      # Promote to quarto project
      writeLines(
        c(
          "project:",
          "  type: default"
        ),
        "exec/_quarto.yml"
      )
    }

    # Create a dployr profile for controlling output dir
    writeLines(
      c(
        "project:",
        "  output-dir: dployr_dist"
      ),
      "exec/_quarto-deployr.yml"
    )

    quarto::quarto_render("exec", quarto_args = "--profile dployr")

    output <- "exec/dployr_dist/"
  } else if (tolower(desc$Type) == "book") {
    require_package("bookdown")

    bookdown::render_book("exec", output_dir = "dist")
  } else if (tolower(desc$Type) == "blog") {
    require_package("blogdown")

    setwd("exec")
    blogdown::build_site(build_rmd = TRUE)
    setwd("../")

    output <- "exec/public/"
  } else if (file.exists("exec/_site.yml") || tolower(desc$Type) %in% c("website", "site")) {
    require_package("rmarkdown")

    if (!file.exists("exec/_site.yml")) {
      stop("Missing `_site.yml` file in `./exec/`. Aborting execution")
    }
    settings <- readLines("exec/_site.yml")
    output_dir <- grepl("^output_dir:", settings)
    if (any(output_dir)) {
      settings[output_dir] <- "output_dir: dployr_dist"
    } else {
      settings <- c("output_dir: dployr_dist", settings)
    }
    writeLines(settings, "exec/_site.yml")

    rmarkdown::render_site("exec")

    output <- "exec/dployr_dist/"
  } else if (
    tolower(desc$Type) %in% c("rmarkdown", "report", "document", "doc")
  ) {
    require_package("rmarkdown")

    input <- grep(
      "\\.(r|md|rmd)$",
      list.files("exec", full.names = TRUE),
      value = TRUE
    )

    if (length(input) == 0) {
      stop(
        "No input found for `rmarkdown::render()` in `./exec`. Aborting execution"
      )
    }

    rmarkdown::render(input[1], output_dir = "dist")
  }

  if (!is.null(output)) {
    dir.create("dist")
    file.copy(
      list.files(
        output,
        full.names = T,
        all.files = TRUE,
        no.. = TRUE
      ),
      "dist/",
      recursive = TRUE
    )
  }
}
