
widgetUI <- function(id, widget, insideTagsDiv = NA, ...){
  ns <- NS(id)
  if (identical(insideTagsDiv, NA)){
    result <- tagList(
      widget(inputId = ns("widget"), ...)
    )
  } else {
    result <- tagList(
      tags$div(class = insideTagsDiv,
               widget(inputId = ns("widget"), ...)
      )
    )
  }
  return(result)
}

selectWidgetUI <- function(id, widget,
                           checked = TRUE,
                           checkedLabel = NULL,
                           insideTagsDiv = NA,
                           ...){
  ns <- NS(id)
  if (identical(insideTagsDiv, NA)){
    result <- tagList(
      checkboxInput(inputId = ns("active"),
                    label = checkedLabel,
                    value = checked),
      widgetUI(id = id,
               widget = widget,
               ...)
    )
  } else {
    result <- tagList(
      tags$div(class = insideTagsDiv,
        checkboxInput(inputId = ns("active"),
                      label = checkedLabel,
                      value = checked)
      ),
      widgetUI(id = id,
               widget = widget,
               insideTagsDiv = insideTagsDiv,
               ...)
    )
  }
  return(result)
}

selectWidgetServer <- function(id){

  moduleServer(
    id,
    function(input, output, session){

      observeEvent(input$active,{
        if (input$active){
          shinyjs::enable("widget")
        } else {
          shinyjs::disable("widget")
        }
      })
      
      returnValue <- reactive({
        if (input$active){
          return(input$widget)
        } else {
          return(NA)
        }
      })
      
      return(returnValue)

    }
  )
}