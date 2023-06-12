
ui_isotopeCluster_sidebar <- tagList(
  tags$div(class = "boxMiddle",
           br()
  ),
  tags$div(
    selectWidgetsUI(
      id = "isotopeCLuster",
      widgets = list(widgetsUI(widget = checkboxInput,
                               name = "positive",
                               insideTagsDiv = "boxMiddle",
                               label = "Positive",
                               width = "150px",
                               value = TRUE),
                     widgetsUI(widget = selectInput, name = "adduct",
                               insideTagsDiv = "boxMiddle",
                               label = "Adduct", width = "150px",
                               choices = c("-","H"),
                               selected = "H"),
                     widgetsUI(widget = sliderInput, name = "resolution",
                               insideTagsDiv = "boxMiddle",
                               label = "Resolution", width = "150px",
                               min = 1000, max = 1E6, step = 1000, value = 20000),
                     widgetsUI(widget = sliderInput, name = "threshold",
                               insideTagsDiv = "boxMiddle",
                               label = "Threshold (%)", width = "150px",
                               min = 0.0001, max = 1, step = 0.0001, value = 0.1),
                     widgetsUI(widget = sliderInput, name = "mzAccuracy",
                               insideTagsDiv = "boxMiddle",
                               label = "m/z Accuracy", width = "150px",
                               min = 1, max = 10, step = 1, value = 4),
                     widgetsUI(widget = sliderInput, name = "abundanceAccuracy",
                               insideTagsDiv = "boxMiddle",
                               label = "Abundance Accuracy", width = "150px",
                               min = 0, max = 10, step = 1, value = 2)
      ),
      checked = TRUE,
      checkedLabel = "Calculate isotope cluster",
      insideTagsDiv = "boxMiddle"
    )
  )
)