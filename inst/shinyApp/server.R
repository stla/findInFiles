shinyServer(function(input, output, session){

  setwd("C:/SL/MyPackages/findInFiles")

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
      tab <- verticalTabPanel(
        title = file,
        aceEditor(
          outputId = outputId,
          value = paste0(readLines(file), collapse = "\n"),
          mode = "r",
          theme = "cobalt",
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
    if(!updated){
      updateVerticalTabsetPanel(
        session,
        "tabset",
        selected = file
      )
    }
    session$sendCustomMessage("goto", list(editor = editor, line = line))
  })

  output[["results"]] <- renderFIF({
    req(input[["pattern"]])
    findInFiles(
      ext = isolate(input[["ext"]]),
      pattern = input[["pattern"]],
      depth = isolate(input[["depth"]]),
      wholeWord = isolate(input[["wholeWord"]]),
      ignoreCase = isolate(input[["ignoreCase"]])
    )
  })

})


