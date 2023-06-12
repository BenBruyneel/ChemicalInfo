
selectHandsonTableUI <- function(id,
                                 checked = TRUE,
                                 checkedLabel = NULL,
                                 mainClass = "boxMiddle",
                                 resetIcon = icon("arrow-rotate-right"),
                                 resetButtonClass = "btn-custom-tiny",
                                 resetButtonPositionClass = "btn-custom-tiny-position",
                                 handsonTableClass = ""){
  ns <- NS(id)
  result <- tagList(
    tags$div(class = mainClass,
             tags$div(
               checkboxInput(inputId = ns("active"),
                             label = checkedLabel,
                             width = "125px",
                             value = checked)
             ),
             tags$div(class = resetButtonPositionClass,
                      actionButton(inputId = ns("reset"),
                                   label = NULL,
                                   icon = resetIcon,
                                   class = resetButtonClass),
             )
    ),
    tags$div(class = mainClass,
      tags$div(class = handsonTableClass,
               rHandsontableOutput(ns("table"))
      )
    )
  )
  return(result)
}

selectHandsonTableServer <- function(id,
                                     theTable,
                                     displayTable){
  
  moduleServer(
    id,
    function(input, output, session){
      
      rv <- reactiveValues()
      rv$theTable <- theTable
      rv$reset <- TRUE
      
      output$table <- renderRHandsontable({
        if (!input$active){
          rv$theTable <- hot_to_r(input$table)
          shinyjs::disable("reset")
        } else {
          shinyjs::enable("reset")
        }
        if (rv$reset){
          isolate(rv$reset <- FALSE)
          return(displayTable(theTable = rv$theTable)(!input$active))
        } else {
          return(displayTable(theTable = rv$theTable)(!input$active))
        }
      })
      
      observeEvent(input$reset, {
        rv$theTable <- theTable
        rv$reset <- TRUE
      })
      
      returnValue <- reactive({
        if (input$active){
          return(hot_to_r(input$table))
        } else {
          return(NA)
        }
      })
      
      return(returnValue)
      
    }
  )
}