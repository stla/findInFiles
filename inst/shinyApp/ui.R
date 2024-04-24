shinyUI(fluidPage(
  tags$head(
    tags$script(src = "shinyFIF.js"),
    tags$link(rel = "stylesheet", href = "shinyFIF.css")
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$fieldset(
        tags$legend(
          actionButton("btnwd", "Current folder", class = "btn-info btn-sm")
        ),
        textOutput("wd"),
        shinyDirButton(
          "folder",
          label = "Change",
          title = "Choose a folder",
          buttonType = "primary",
          class = "btn-block btn-sm",
          onclick = '$("#results").empty();'
        )
      ),
      br(),
      smallInput(textInput(
        "ext", "Extensions (comma-separated):",
        value = "R"
      )),
      textInput(
        "pattern", "Pattern to search:"
      ),
      smallInput(numericInput(
        "depth", "Depth; blank for unlimited:",
        value = 1, min = 0, step = 1
      )),
      actionButton(
        "run",
        tags$span(
          "Find in files", class = "mylabel"
        ),
        class = "btn-danger btn-block"
      ),
      br(),
      dropMenu(
        actionButton(
          "menu", "Pattern options", class = "btn-success btn-block btn-sm"
        ),
        splitLayout(
          checkboxInput(
            "wholeWord", "Whole word"
          ),
          checkboxInput(
            "ignoreCase", "Ignore case"
          )
        ),
        awesomeRadio(
          "patternType",
          "Pattern type",
          choices = list(
            "Basic regular expression" = "basic",
            "Fixed strings" = "fixed",
            "Extended regular expression" = "extended",
            "Perl regular expression" = "perl"
          )
        ),
        placement = "top",
        maxWidth = "600px"
      ),
      smallInput(numericInput(
        "maxCount", "Maximum number of results; blank for unlimited:",
        value = 100, min = 1, step = 50
      ))
    ),
    mainPanel(
      width = 9,
      style = "display:flex; flex-flow:column; height: 95vh",
      jqui_resizable(tags$div(
        id = "editors",
        # conditionalPanel(
        #   "output.folderOK",
        #   style = "display: none;",
        # verticalTabsetPanel(
        #   id = "tabset"
        # )
        # )
      ), options = list(
        handles = "s",
        alsoResize = ".ace_editor"
      )),
      br(),
      FIFOutput("results", height = "100%"),
      br()
    )
  )
))

