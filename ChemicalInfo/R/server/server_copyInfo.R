
observeEvent(input$CopyFromInfo,{
  updateTextInput(session = session,
                  inputId = "chemicalName",
                  value = rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Name"])
  updateTextInput(session = session,
                  inputId = "chemicalCAS",
                  value = rv$generalInfoTable$value[rv$generalInfoTable$parameter == "CAS"])
  updateTextInput(session = session,
                  inputId = "chemicalFormula",
                  value = removeMarkdown(rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"]))
  updateTextAreaInput(session = session,
                      inputId = "chemicalSmiles",
                      value = rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Smiles"])
  
})
