
shinyUI(fluidPage(
  tags$head(
    tags$script(src = "shinyFIF.js"),
    tags$script(onKeyDown),
    tags$style(HTML(css))
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "ext", "Extension",
        choices = c("R", "js", "css")
      ),
      tags$div(
        class = "form-group shiny-input-container",
        tags$label(
          class = "control-label",
          "Pattern"
        ),
        tags$input(
          type = "text",
          class = "form-control",
          onkeydown = "onKeyDown(event);",
          placeholder = "Press Enter when ready"
        )
      ),
      numericInput(
        "depth", "Depth (set -1 for unlimited depth)",
        value = -1, min = -1, step = 1
      ),
      checkboxInput(
        "wholeWord", "Whole word"
      ),
      checkboxInput(
        "ignoreCase", "Ignore case"
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

