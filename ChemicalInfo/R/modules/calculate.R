library(shiny)
library(shinyjs)

calculateUI <- function(id,
                         calculateClass = "boxMiddle"){
  ns <- NS(id)
  result <- tagList(
    br(),
    tags$div(class = calculateClass,
             textInput(inputId = ns("sampleMZ"),
                       label = "m/z",
                       width = "125px")
    ),
    br(),
    tags$div(class = calculateClass,
             textInput(inputId = ns("referenceMZ"),
                       label = "Reference m/z",
                       width = "125px")
    ),
    br(),
    tags$div(class = calculateClass,
             sliderInput(inputId = ns("accuracyDa"),
                         label = "Reference m/z",
                         min = 0,
                         max = 10,
                         value = 4,
                         width = "125px")
    ),
    br(),
    tags$div(class = calculateClass,
             sliderInput(inputId = ns("accuracyPPM"),
                         label = "Reference m/z",
                         min = 0,
                         max = 10,
                         value = 1,
                         width = "125px")
    ),
    HTML("<hr style = 'border-color: black'>"),
    tags$div(class = calculateClass,
             textInput(inputId = ns("deviationDa"),
                       label = "Deviation (Da)",
                       width = "125px"),
    ),
    br(),
    tags$div(class = calculateClass,
             tags$div(class = calculateClass,
                      textInput(inputId = ns("deviationPPM"),
                                label = "Deviation (ppm)",
                                width = "125px")
    )
    )
  )
  return(result)
}

calculateServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      shinyjs::disable("deviationDa")
      shinyjs::disable("deviationPPM")
      
      observeEvent(list(input$sampleMZ,
                        input$referenceMZ,
                        input$accuracyDa,
                        input$accuracyPPM),{
                          result <- abs(as.numeric(input$sampleMZ)) - abs(as.numeric(input$referenceMZ))
                          if (!identical(result, NA)){
                              updateTextInput(session = session, inputId = "deviationDa",
                                              value = ifelse(abs(result) != Inf,
                                                             BBPersonalR::formatDigits(digits = as.integer(input$accuracyDa))(result),
                                                             ""))
                          } else {
                            updateTextInput(session = session, inputId = "deviationDa", value = "")
                          }
                          result <- ((abs(as.numeric(input$sampleMZ)) - abs(as.numeric(input$referenceMZ)))/abs(as.numeric(input$referenceMZ)))*1E6
                          if (!identical(result, NA)){
                            updateTextInput(session = session, inputId = "deviationPPM",
                                            value = ifelse(abs(result) != Inf,
                                                           BBPersonalR::formatDigits(digits = as.integer(input$accuracyPPM))(result),
                                                           ""))
                          } else {
                            updateTextInput(session = session, inputId = "deviationPPM", value = "")
                          }
      })
      
      returnValue <- reactive({
        suppressMessages(
          result <- list(
            mz = input$sampleMZ,             # note : character vector
            referenceMz = input$referenceMZ, # note : character vector
            accuracyDalton = input$accuracyDa,
            accuracyPPM = input$accuracyPPM,
            deviationDalton = as.numeric(input$sampleMZ) - as.numeric(input$referenceMZ),
            deviationPPM = ((as.numeric(input$sampleMZ) - as.numeric(input$referenceMZ))/as.numeric(input$referenceMZ))*1E6
          )
        )
        return(result)
      })
      
      return(returnValue)
    }
  )
}