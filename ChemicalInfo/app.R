library(shiny)
library(shinydashboard)
library(shinyjs)
library(rhandsontable)
library(gt)
library(htmlwidgets)
library(chemdoodle)

source("R/massSpectrometry/chemistry.R")
purrr::walk(list.files("R/modules/", pattern = "*.R", full.names = T), ~source(.x))
purrr::walk(list.files("R/", pattern = "*.R", full.names = T), ~source(.x))
purrr::walk(list.files("R/ui/", pattern = "*.R", full.names = T, recursive = T), ~source(.x))

ui <- fluidPage(

    shinyjs::useShinyjs(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    ui_titles,

    fluidPage(
        sidebarPanel(width = 3,
                     class = "sidebarpanel",
                     position = "left",
                     br(),
                     tabsetPanel(
                       tabPanel(
                         title = "PubChem",
                         ui_pubchem_sidebar
                       ),
                       tabPanel(
                         title = "Manual",
                         ui_manual_sidebar
                       ),
                       tabPanel(
                         title = "Calculate",
                         calculateUI(id = "deviationCalculate")
                       ),
                       tabPanel(
                         title = "Export",
                         ui_export
                       )
                     )
        ),
        mainPanel(width = 6,
                  ui_mainPanel
        ),
        sidebarPanel(width = 3,
                     class = "sidebarpanel",
                     position = "right",
                     br(),
                     tabsetPanel(
                       id = "optionsTabs",
                       tabPanel(title = "Isotope Cluster",
                                ui_isotopeCluster_sidebar
                       ),
                       tabPanel(title = "Charge States",
                                ui_chargeStates_sidebar
                       ),
                       tabPanel(title = "Adducts",
                                ui_adducts_sidebar
                       ),
                       tabPanel(title = "Structure",
                                ui_structure_sidebar
                       )
                     ),
                     hr()
        )
    )
)

server <- function(input, output, session) {

  # ---- initialize reactiveValues ----
  
  rv <-reactiveValues()
  rv$generalInfoTable <- data.frame(parameter = c("Name","CAS","PubChem ID","Formula","Exact Mass","Average Mass","Smiles"),
                                    value = rep("",7))
  
  rv$additionalInfo <- NA
  
  rv$smiles <- ""
  
  # server definitions
  
  serverSourceFiles <- list.files(path = "R/server/", pattern = "*.R",
                                  full.names = TRUE)
  if (length(serverSourceFiles) > 0){
    serverSourceText <- paste0("source('",serverSourceFiles,"', local = TRUE)")
    for (counter in 1:(length(serverSourceText))){
      eval(parse(text = serverSourceText[[counter]]))
    }
  }

}

shinyApp(ui = ui, server = server)
