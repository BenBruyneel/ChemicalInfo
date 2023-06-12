
# ---- Deactivate buttons ----

shinyjs::disable("generate")
shinyjs::disable("generateManual")
shinyjs::disable("CopyFromInfo")
shinyjs::disable("toExcel")

# ---- rhandsontable definitions ----

# function factory defining how to display rhandsontable 'adducts'
rhtAdducts <- function(theTable = startAdducts){
  function(readOnly){
    rhandsontable(theTable,
                  rowHeaders = FALSE, readOnly = readOnly,
                  colHeaders = c("Name","Formula","#", "Charge", "Charge #")) %>%
      hot_col(col = "Name", halign = "htleft") %>%
      hot_col(col = "Formula", halign = "htLeft") %>%
      hot_col(col = "#", type = "dropdown", source = as.character(1:50), halign = "htCenter", default = "1") %>%
      hot_col(col = "Charge", type = "dropdown", source = c("+","-"), halign = "htCenter", default = "+") %>%
      hot_col(col = "Charge #", type = "dropdown", source = c("1","2","3","4","5","6"), default = "1", halign = "htCenter", readOnly = T) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c("60%","65%","60%","50%","1%"), manualColumnResize = TRUE, manualColumnMove = FALSE)
  }
}

# function factory defining how to display rhandsontable 'additional data'
rhtAdditional <- function(theTable = startAdditionalInfo){
  force(theTable)
  function(readOnly){
    rhandsontable(theTable,
                  rowHeaders = FALSE, colHeaders = FALSE, readOnly = readOnly) %>%
      hot_col(col = 1, halign = "htLeft") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c("150%"), manualColumnResize = TRUE, manualColumnMove = FALSE)
  }
}

# ---- initialize modules ----

testCAS <- CAS_Server("cas1")  

isotopeClusterWidget <- selectWidgetsServer("isotopeCLuster", widgets = c("positive", "adduct", "resolution", "threshold", "mzAccuracy", "abundanceAccuracy"))

chargeStatesWidget <- selectWidgetsServer("chargeStates", widgets = c("positive","monoisotopic","adduct","chargeStates","mzAccuracy"))

adductsWidget <- selectHandsonTableServer(id = "adducts",
                                          theTable = startAdducts,
                                          displayTable = rhtAdducts)

additionalWidget <- selectHandsonTableServer(id = "additional",
                                             theTable = startAdditionalInfo,
                                             displayTable = rhtAdditional)

calculateWidget <- calculateServer(id = "deviationCalculate")

# ---- 'general' observers ----

observe({
  toggleState(id = "generate", condition = (testCAS() != "") & !identical(testCAS(),NA))
})

observeEvent(input$chemicalFormula, {
  if (nchar(input$chemicalFormula) > 0){
    shinyjs::enable("generateManual")
  } else {
    shinyjs::disable("generateManual")
  }
})

observeEvent(adductsWidget(),{
  if (identical(adductsWidget(), NA)){
    shinyjs::disable("adductsAccuracy")
    shinyjs::disable("adductsMonoisotopic")
  } else {
    shinyjs::enable("adductsAccuracy")
    shinyjs::enable("adductsMonoisotopic")
  }
})

# strictly speaking, not needed
observeEvent(rv$generalInfoTable,{
  if (!identical(rv$generalInfoTable, NA)){
    if (nrow(rv$generalInfoTable)>0){
      shinyjs::enable("CopyFromInfo")
    } else {
      shinyjs::disable("CopyFromInfo")
      shinyjs::disable("toExcel")
    }
  } else {
    shinyjs::disable("CopyFromInfo")
    shinyjs::disable("toExcel")
  }
})

# ---- initialize ui ----

# this command only because rhandsonTable will not get initialized when has not in focus at least once !!
updateTabsetPanel(session = session,
                  inputId = "optionsTabs",
                  selected = "Adducts")