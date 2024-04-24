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
      textInput(
        "ext", "Extension:",
        value = "R"
      ),
      textInput(
        "pattern", "Pattern:"
      ),
      numericInput(
        "depth", "Depth; set -1 for unlimited:",
        value = 1, min = -1, step = 1
      ),
      wellPanel(
        style = "border-color: #e4c4c4",
        actionButton(
          "run", "Find",
          class = "btn-danger btn-block"
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput(
            "wholeWord", "Whole word"
          )
        ),
        column(
          6,
          checkboxInput(
            "ignoreCase", "Ignore case"
          )
        )
      ),
      numericInput(
        "maxCount", "Max number of results; set 0 for unlimited:",
        value = 100, min = 0, step = 50
      )
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

