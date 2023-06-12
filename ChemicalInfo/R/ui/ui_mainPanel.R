
ui_mainPanel <- tagList(
  br(),
  br(),
  fluidPage(
    fluidRow(
      gt_output(outputId = "generalInfo")
    ),
    hr(),
    fluidRow(
      column(4,
             uiOutput(outputId = "isotopeClusterTable")
      ),
      column(4,
             uiOutput(outputId = "chargeStatesTable")
      ),
      column(4,
             uiOutput(outputId = "adductsTable")
      )
    ),
    hr(),
    fluidRow(class = "boxMiddle",
             chemdoodle_viewerOutput(outputId = "structurePlot",
                                     width = "600px",
                                     height = "600px")
    ),
    hr(),
    fluidRow(class = "boxMiddle",
             column(12,
                    uiOutput(outputId = "additionalInfo")
             )
    )
  )
)