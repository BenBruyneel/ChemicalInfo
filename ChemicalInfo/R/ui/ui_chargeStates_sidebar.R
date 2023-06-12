
ui_chargeStates_sidebar <- tagList(
  tags$div(class = "boxMiddle",
           br()
  ),
  tags$div(
    selectWidgetsUI(id = "chargeStates",
                    widgets = list(
                      widgetsUI(widget = checkboxInput,
                                name = "positive",
                                insideTagsDiv = "boxMiddle",
                                label = "Positive",
                                width = "150px",
                                value = TRUE),
                      widgetsUI(widget = checkboxInput,
                                name = "monoisotopic",
                                insideTagsDiv = "boxMiddle",
                                label = "Mono Isotopic m/z",
                                width = "150px",
                                value = TRUE),
                      widgetsUI(widget = selectInput, name = "adduct",
                                insideTagsDiv = "boxMiddle",
                                label = "Adduct", width = "150px",
                                choices = c("-","H"),
                                selected = "H"),
                      widgetsUI(widget = sliderInput, name = "chargeStates",
                                insideTagsDiv = "boxMiddle",
                                label = "Charge States", width = "150px",
                                min = 1, max = 50, value = c(1,4)
                      ),
                      widgetsUI(widget = sliderInput, name = "mzAccuracy",
                                insideTagsDiv = "boxMiddle",
                                label = "m/z Accuracy", width = "150px",
                                min = 1, max = 10, step = 1, value = 4)
                    ),
                    checked = TRUE,
                    checkedLabel = "Calculate Charge states",
                    insideTagsDiv = "boxMiddle"
    )
  )
)