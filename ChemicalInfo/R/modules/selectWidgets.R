
widgetsUI <- function(widget, name, insideTagsDiv = NA, ...){
  function(id){
    ns <- NS(id)
    if (identical(insideTagsDiv, NA)){
      result <- tagList(
        widget(inputId = ns(name), ...)
      )
    } else {
      result <- tagList(
        tags$div(class = insideTagsDiv,
                 widget(inputId = ns(name), ...)
        )
      )
    }
    return(result)
  }
}

# widgets must be a list of widgetsUI (function factories!)

selectWidgetsUI <- function(id, widgets,
                            checked = TRUE,
                            checkedLabel = NULL,
                            insideTagsDiv = NA,
                            ...){
  ns <- NS(id)
  if (identical(insideTagsDiv, NA)){
    result <- tagList(
      checkboxInput(inputId = ns("active"),
                    label = checkedLabel,
                    value = checked)
    )
  } else {
    result <- tagList(
      tags$div(class = insideTagsDiv,
               checkboxInput(inputId = ns("active"),
                             label = checkedLabel,
                             value = checked)
      )
    )
  }
  for (counter in 1:length(widgets)){
    result <- tagAppendChild(result, widgets[[counter]](id = id))
  }
  return(result)
}

# widgets = character vector with names of the widgets

selectWidgetsServer <- function(id, widgets){
  
  moduleServer(
    id,
    function(input, output, session){
      
      observeEvent(input$active,{
        if (input$active){
          for (counter in 1:length(widgets)){
            shinyjs::enable(widgets[counter])
          }
        } else {
          for (counter in 1:length(widgets)){
            shinyjs::disable(widgets[counter])
          }
        }
      })
      
      returnValue <- reactive({
        if (input$active){
          result <- list()
          for (counter in 1:length(widgets)){
            result[[counter]] <- input[[widgets[counter]]]
          }
          names(result) <- widgets
          return(result)
        } else {
          return(NA)
        }
      })
      
      return(returnValue)
      
    }
  )
}