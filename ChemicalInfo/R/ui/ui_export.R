
ui_export <- tagList(
  br(),
  br(),
  br(),
  tags$div(class = "boxMiddle",
           downloadButton(outputId = "toExcel",
                          label = "Download",
                          width = "125px",
                          class = "btn-primary",
                          icon = icon("file-arrow-down"))
  )
)