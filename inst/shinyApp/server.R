shinyServer(function(input, output, session){

  iv <- InputValidator$new()
  iv$add_rule(
    "ext",
    sv_regex("^[a-zA-Z0-9\\+]+$", "Only alphanumeric or 'c++'")
  )
  iv$add_rule("pattern", sv_required())
  iv$add_rule("depth", sv_integer())
  iv$enable()

  shinyDirChoose(
    input, "folder", roots = roots
  )

  output$wd <- renderText({
    path <- parseDirPath(roots, input[["folder"]])
    if(length(path) == 0L){
      getwd()
    }else{
      setwd(path)
      path
    }
  })

  Tabsets <- reactiveVal(character(0L))
  Editors <- reactiveVal(character(0L))

  observeEvent(input$closetab, {
    print(input$closetab)
    # file <- Tabsets()[input$closetab]
    index <- match(input$closetab, names(Tabsets()))
    Tabsets(Tabsets()[-index])
    print(Tabsets())
    if(length(Tabsets()) == 0L) {
      removeUI("#tabset-tabbable")
    }else{
      removeVerticalTab("tabset", index)
    }
  })

  observeEvent(input$filewithline, {
    print(Tabsets())
    notabset <- length(Tabsets()) == 0L
    file <- input$filewithline$file
    line <- input$filewithline$line
    updated <- FALSE
    if(!is.element(file, Tabsets())){
      outputId <- paste0("editor", length(Editors()) + 1L)
      Editors(c(Editors(), outputId))
      names(file) <- outputId
      Tabsets(c(Tabsets(), file))
      ext <- tolower(tools::file_ext(file))
      acemode <- switch(
        ext,
        c = "c_cpp",
        cpp = "c_cpp",
        "c++" = "c_cpp",
        dockerfile = "dockerfile",
        frag = "glsl",
        h = "c_cpp",
        hpp = "c_cpp",
        css = "css",
        f = "fortran",
        f90 = "fortran",
        gitignore = "gitignore",
        hs = "haskell",
        html = "html",
        java = "java",
        js = "javascript",
        jsx = "jsx",
        json = "json",
        jl = "julia",
        tex = "latex",
        md = "markdown",
        map = "json",
        markdown = "markdown",
        rmd = "markdown",
        mysql = "mysql",
        ml = "ocaml",
        perl = "perl",
        pl = "perl",
        php = "php",
        py = "python",
        r = "r",
        rd = "rdoc",
        rhtml = "rhtml",
        rnw = "latex",
        ru = "ruby",
        rs = "rust",
        scala = "scala",
        scss = "scss",
        sh = "sh",
        sql = "sql",
        svg = "svg",
        txt = "text",
        ts = "typescript",
        vb = "vbscript",
        xml = "xml",
        yaml = "yaml",
        yml = "yaml"
      )
      content <- paste0(readLines(file), collapse = "\n")
      tab <- verticalTabPanel(
        title = file,
        aceEditor(
          outputId = outputId,
          value = content,
          mode = ifelse(is.null(acemode), "plain_text", acemode),
          theme = "cobalt",
          tabSize = 2,
          height = "40vh"
        ),
        box_height = NULL
      )
      if(notabset){
        tabset <- verticalTabsetPanel(tab, id = "tabset")
        insertUI(
          "#editors",
          where = "afterBegin",
          ui = tabset
        )
      }else{
        appendVerticalTab(
          "tabset",
          tab
        )
        updateVerticalTabsetPanel(
          session,
          "tabset",
          selected = file[[1L]]
        )
      }
      session$sendCustomMessage("closeButton", outputId)
      print(Tabsets())
      updated <- TRUE
    }
    index <- match(file, Tabsets())
    editor <- names(Tabsets())[index]
    btnid <- paste0("btn_", editor)
    if(is.null(input[[btnid]])){
      print("INSERT")
      insertUI(
        sprintf("#%s .ace_scroller", editor),
        "beforeEnd",
        actionButton(
          btnid, "Save", icon = icon("save"),
          class = "btn-success",
          style = "position: absolute; bottom: 2px; right: 2px;",
          onmousedown = "event.stopPropagation();",
          onclick = sprintf(
            'Save(event, "%s", "%s");', URLencode(content), basename(file)
          )
        )
      )
    }
    if(!updated){
      updateVerticalTabsetPanel(
        session,
        "tabset",
        selected = file
      )
    }
    session$sendCustomMessage("goto", list(editor = editor, line = line))
  })

  observeEvent(input[["save"]], {
    filename <- input[["save"]][["name"]]
    editor <- input[["save"]][["editor"]]
    session[["sendCustomMessage"]](
      "save", list(name = filename, content = input[[editor]])
    )
  })

  output[["results"]] <- renderFIF({
    req(input[["run"]])
    findInFiles(
      ext = isolate(input[["ext"]]),
      pattern = isolate(input[["pattern"]]),
      depth = isolate(input[["depth"]]),
      wholeWord = isolate(input[["wholeWord"]]),
      ignoreCase = isolate(input[["ignoreCase"]])
    )
  })

})


