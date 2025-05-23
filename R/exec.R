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
  config <- config_get()
  if (!is.null(desc$ExecFun)) {
    execute_fun(desc, type, config)
  } else {
    switch(
      type,
      batch = execute_batch(desc, config),
      dynamic = execute_dynamic(desc, config),
      static = execute_static(desc, config)
    )
  }
}

execute_fun <- function(desc, type, config) {
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

execute_batch <- function(desc, config) {
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

execute_dynamic <- function(desc, config) {
  options(shiny.autoload.r = FALSE)

  if (file.exists("exec/_server.yml")) {
    srvyml <- "exec/_server.yml"
    engine <- yaml::read_yaml(srvyml)$engine

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

    message("Creating ", desc$Type, " based on `./exec/_server.yml`")
    engine_fun(srvyml, port = config$port, host = config$host)
  } else if (tolower(desc$Type) == "shiny") {
    require_package("shiny")

    message("Creating app from content of `./exec`")

    shiny::runApp(
      shiny::shinyAppDir("exec/"),
      launch.browser = FALSE,
      port = config$port,
      host = config$host,
      workerId = config$workerId
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
      contact <- utils::as.person(desc$Maintainer)
    }

    pr <- plumber::plumb(dir = "exec/")
    pr <- plumber::pr_set_api_spec(pr, function(spec) {
      spec$info$title <- desc$Title
      spec$info$description <- desc$Description
      spec$info$version <- desc$Version
      spec$info$contact <- list(
        name = paste(contact$given, contact$family),
        url = desc$BugReports,
        email = contact$email
      )
      spec
    })
    plumber::pr_run(pr, host = config$host, port = config$port)
  } else if (tolower(desc$Type) == "rmd_shiny") {
    require_package("rmarkdown")
    require_package("shiny")

    rmarkdown::run(
      file = NULL,
      dir = "exec",
      shiny_args = list(
        port = config$port,
        host = config$host,
        workerId = config$workerId
      ),
      render_args = list(
        envir = globalenv()
      ),
      auto_reload = FALSE
    )
  } else if (tolower(desc$Type) == "quarto_shiny") {
    require_package("quarto")
    require_package("shiny")

    quarto::quarto_serve(
      "exec/index.qmd",
      render = TRUE,
      browse = FALSE,
      port = config$port,
      host = config$host
    )
  } else {
    # Shouldn't happen
    stop("Unknown dynamic project type: ", desc$Type)
  }
}

execute_static <- function(desc, config) {
  output <- NULL
  is_quarto <- file.exists("exec/_quarto.yml") ||
    tolower(desc$Type) %in% c("quarto", "website", "blog", "book", "manuscript", "report") ||
    any(grepl("\\.qmd$", list.files("exec"), ignore.case = TRUE))

  if (is_quarto) {
    require_package("quarto")

    if (!file.exists("exec/_quarto.yml")) {
      # Promote to quarto project
      type <- tolower(desc$Type)
      if (type == "quarto") type <- "default"
      writeLines(
        c(
          "project:",
          paste0("  type: ", type)
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
      "exec/_quarto-dployr.yml"
    )

    quarto::quarto_render(
      "exec",
      profile = "dployr",
      output_format = config$staticFormat,
      execute_params = config$params,
      quarto_args = config$quartoArgs,
      pandoc_args = config$pandocArgs
    )

    output <- "exec/dployr_dist/"
  } else if (tolower(desc$Type) == "rmd_book") {
    require_package("bookdown")
    require_package("knitr")

    knitr::opts_chunk$set(warning = FALSE, error = FALSE)

    bookdown::render_book(
      "exec",
      output_dir = "dist",
      output_format = config$staticFormat
    )
  } else if (tolower(desc$Type) == "rmd_blog") {
    require_package("blogdown")
    require_package("knitr")

    knitr::opts_chunk$set(warning = FALSE, error = FALSE)

    setwd("exec")
    blogdown::build_site(build_rmd = TRUE, hugo_args = config$hugoArgs)
    setwd("../")

    output <- "exec/public/"
  } else if (tolower(desc$Type) == "rmd_website") {
    require_package("rmarkdown")
    require_package("knitr")

    knitr::opts_chunk$set(warning = FALSE, error = FALSE)

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

    rmarkdown::render_site(
      "exec",
      output_format = config$staticFormat,
      envir = globalenv()
    )

    output <- "exec/dployr_dist/"
  } else if (tolower(desc$Type) == "rmd_report") {
    require_package("rmarkdown")
    require_package("knitr")

    knitr::opts_chunk$set(warning = FALSE, error = FALSE)

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

    rmarkdown::render(
      input[1],
      output_dir = "dist",
      output_format = config$staticFormat,
      params = config$params,
      envir = globalenv()
    )
  } else if (tolower(desc$Type) == "static") {
    execute_batch(desc, config)
  } else {
    # Shouldn't happen
    stop("Unknown static project type: ", desc$Type)
  }

  if (!is.null(output)) {
    dir.create(config$distDir, showWarnings = FALSE, recursive = TRUE)
    file.copy(
      list.files(
        output,
        full.names = T,
        all.files = TRUE,
        no.. = TRUE
      ),
      config$distDir,
      recursive = TRUE
    )
  }
}
