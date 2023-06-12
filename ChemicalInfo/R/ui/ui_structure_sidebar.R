

ui_structure_sidebar <- tagList(
  br(),
  tags$div(class = "boxMiddle",
           sliderInput(inputId = "structureWidth",
                       width = "150px",
                       label = "Width",
                       min = 100, max = 750, step = 50, value = 500)
  ),
  br(),
  tags$div(class = "boxMiddle",
           sliderInput(inputId = "structureHeight",
                       width = "150px",
                       label = "Height",
                       min = 100, max = 750, step = 50, value = 500)
  ),
  br(),
  tags$div(class = "boxMiddle",
           sliderInput(inputId = "structurePlotSize",
                       width = "150px",
                       label = "Size structure plot",
                       min = 1, max = 100, step = 0.1, value = 32)
  ),
  br(),
  tags$div(class = "boxMiddle",
           sliderInput(inputId = "atomsFontSize2D",
                       width = "150px",
                       label = "Atoms Font Size",
                       min = 1, max = 48, step = 1, value = 12)
  ),
  tags$div(class = "boxMiddle",
           checkboxInput(inputId = "showAllHydrogens",
                         width = "150px",
                         label = "Show Hydrogens",
                         value = T)
  ),
  tags$div(class = "boxMiddle",
           checkboxInput(inputId = "showAllC",
                         width = "150px",
                         label = "Show All Carbons",
                         value = F)
  ),
  tags$div(class = "boxMiddle",
           checkboxInput(inputId = "showTerminalC",
                         width = "150px",
                         label = "Show Terminal Carbons",
                         value = T)
  )
)