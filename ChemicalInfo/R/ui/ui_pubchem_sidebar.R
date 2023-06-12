
ui_pubchem_sidebar <- tagList(
  CAS_UI("cas1", casInputvalue = "",
         useBox = T, height = "300px", width = "200px"),
  tags$div(class = "boxMiddle",
           br()
  ),
  selectHandsonTableUI(id = "additional",
                       checkedLabel = "Other Data",
                       handsonTableClass = "rhandsonFont"),
  tags$div(class = "boxMiddle",
           br()
  ),
  tags$div(class = "boxMiddle",
           actionButton(inputId = "generate",
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