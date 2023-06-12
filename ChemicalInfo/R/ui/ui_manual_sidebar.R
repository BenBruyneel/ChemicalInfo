
ui_manual_sidebar <- tagList(
  br(),
  tags$div(class = "boxLeft",
           textInput(inputId = "chemicalName",
                     label = "Name",
                     width = "100%",
                     value = "")
  ),
  tags$div(class = "boxLeft",
           textInput(inputId = "chemicalCAS",
                     label = "CAS",
                     width = "100%",
                     value = "")
  ),
  tags$div(class = "boxLeft",
           textInput(inputId = "chemicalFormula",
                     label = "Chemical Formula",
                     width = "100%",
                     value = "")
  ),
  tags$div(class = "boxLeft",
           textAreaInput(inputId = "chemicalSmiles",
                         label = "Canonical SMILES",
                         value = "",
                         width = "100%",
                         height = "96px",
                         resize = "vertical")
  ),
  br(),
  tags$div(class = "boxMiddle",
           actionButton(inputId = "CopyFromInfo",
                        label = "Copy Info",
                        width = "125px",
                        icon = icon("arrow-left"),
                        class = "btn-primary")
  ),
  br(),
  tags$div(class = "boxMiddle",
           actionButton(inputId = "generateManual",
                        label = "Generate",
                        width = "125px",
                        icon = icon("file-contract"),
                        class = "btn-primary")
  ),
  br(),
  HTML("<hr style = 'border-color: black'>"),
  tags$div(class = "boxMiddle",
           br()
  )
)