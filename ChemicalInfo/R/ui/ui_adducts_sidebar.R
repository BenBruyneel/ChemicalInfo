
ui_adducts_sidebar <- tagList(
  br(),
  selectHandsonTableUI(id = "adducts",
                       checkedLabel = "Adducts",
                       handsonTableClass = "rhandsonFont"),
  br(),
  tags$div(class = "boxMiddle",
           checkboxInput(inputId = "adductsMonoisotopic",
                         label = "Mono Isotopic m/z",
                         width = "150px",
                         value = TRUE)
  ),
  br(),
  tags$div(class = "boxMiddle",
           sliderInput(inputId = "adductsAccuracy",
                       label = "m/z Accuracy",
                       width = "150px",
                       min = 1, max = 10, step = 1, value = 4)
  )
)