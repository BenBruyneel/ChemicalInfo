library(shiny)
library(shinyjs)
library(webchem)

CAS_UI <- function(id,
                   casLabel = "CAS #",
                   casInputvalue = "",
                   casWidth = "125px",
                   casSubmitWidth = "125px",
                   casSubmitIcon = icon("magnifying-glass"),
                   casSubmitLabel = "Search",
                   casSubmitClass = "btn-primary",
                   casClass = "boxMiddle",
                   pcidLabel = "Pubchem ID",
                   pcidInputValue = "",
                   pcidChoices =  c(""),
                   pcidselected = NULL,
                   pcidWidth = "125px",
                   useBox = FALSE,
                   ...){
  ns <- NS(id)
  result <- 
    tagList(
      tabBox(id = ns("page"), width = "100%",
             tabPanel(title = "ID",
                      tags$div(class = casClass,
                               br()
                      ),
                      tags$div(class = casClass,
                               h5(pcidLabel)
                      ),
                      tags$div(class = casClass,
                               textInput(inputId = ns("manualPcID"),
                                         label = NULL,
                                         value = pcidInputValue,
                                         width = pcidWidth)
                      )
             ),
             tabPanel(title = "CAS" ,
                      tags$div(class = casClass,
                               br()
                      ),
                      tags$div(class = casClass,
                               h5(casLabel)
                      ),
                      tags$div(class = casClass,
                               textInput(inputId = ns("cas"),
                                         label = NULL,
                                         value = casInputvalue,
                                         width = casWidth)
                      ),
                      tags$div(class = casClass,
                               actionButton(inputId = ns("submit"),
                                            label = casSubmitLabel,
                                            width = casSubmitWidth,
                                            icon = casSubmitIcon,
                                            class = casSubmitClass)
                      ),
                      tags$div(class = casClass,
                               br()
                      ),
                      tags$div(class = casClass,
                               h5(pcidLabel)
                      ),
                      tags$div(class = casClass,
                               selectInput(inputId = ns("pubchemID"),
                                           label = NULL,
                                           choices = pcidChoices,
                                           width = pcidWidth)
                      )
             )
      )
    )
  if (useBox){
    result <- box(
      result,
      ...
    )
  }
  return(result)
}


CAS_Server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      
      shinyjs::disable("pubchemID")

      observeEvent(input$cas,{
        if (nchar(input$cas) == 0){
          shinyjs::disable("submit")
        } else {
          shinyjs::enable("submit")
        }
      })
      
      observeEvent(input$submit,{
         result <- get_cid(input$cas, from = "xref/rn", verbose = TRUE)
         if (!is.na(result$cid[1])){
           updateSelectInput(session = session, inputId = "pubchemID",choices = result$cid)
           shinyjs::enable("pubchemID")
         } else {
           updateSelectInput(session = session, inputId = "pubchemID",choices = c(""))
           shinyjs::disable("pubchemID")
         }
      })
      
      observeEvent(input$pubchemID,{
        updateTextInput(session = session,
                        inputId = "manualPcID",
                        value = input$pubchemID)
      })
      
      returnValue <- reactive({
        if (!(nchar(input$manualPcID) == 0)){
          return(input$manualPcID)
        } else {
          return(NA)
        }
      })

      return(returnValue)

    }

  )
}