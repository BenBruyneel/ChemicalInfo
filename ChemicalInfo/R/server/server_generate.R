observeEvent(input$generate, {
  pubchemInfo <- list()
  isolate(pubchemInfo$pubchemID <- testCAS())
  pubchemInfo$pubchemID <- str_trim(pubchemInfo$pubchemID, side = "both")
  pubchemInfo$pubchem <- pc_prop(cid = pubchemInfo$pubchemID, verbose = TRUE) # NA if bad request (400)
  if (!identical(pubchemInfo$pubchem, NA)){
    pubchemInfo$CAS <- NULL
    pubchemInfo$name <- NULL
    result <- pc_sect(id = pubchemInfo$pubchemID, section = "CAS", verbose = TRUE)
    if (ncol(result)>0){
      pubchemInfo$CAS <- result$Result[[1]]
    }
    result <- pc_sect(id = pubchemInfo$pubchemID, section = "synonyms", verbose = TRUE)
    if (ncol(result)>0){
      pubchemInfo$name <- result$Name[1]
    }
    if (!("MolecularFormula" %in% colnames(pubchemInfo$pubchem))){
      pubchemInfo$pubchem$MolecularFormula = ""
    }
    if (!("CanonicalSMILES" %in% colnames(pubchemInfo$pubchem))){
      pubchemInfo$pubchem$CanonicalSMILES <- ""
    }
    rv$generalInfoTable <- data.frame(parameter = c("Name","CAS","PubChem ID","Formula","Exact Mass","Average Mass","Smiles"),
                                      value = c(ifelse(is.null(pubchemInfo$name),
                                                       "", pubchemInfo$name),
                                                ifelse(is.null(pubchemInfo$CAS),
                                                       "", pubchemInfo$CAS),
                                                ifelse(is.na(pubchemInfo$pubchemID),
                                                       "", pubchemInfo$pubchemID),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "",
                                                       formulaString(stringToFormula(pubchemInfo$pubchem$MolecularFormula), useMarkdown = T)),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", stringToFormula(pubchemInfo$pubchem$MolecularFormula) %>% formulaToMass(enviPat = TRUE) %>% BBPersonalR::formatDigits(4)()),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", stringToFormula(pubchemInfo$pubchem$MolecularFormula) %>% formulaToMass(enviPat = TRUE, exact = FALSE) %>% BBPersonalR::formatDigits(4)()),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", pubchemInfo$pubchem$CanonicalSMILES)))
    rv$smiles <- pubchemInfo$pubchem$CanonicalSMILES
    rv$additionalInfo <- getAdditionalInfo(id = pubchemInfo$pubchemID, additionalInfo = additionalWidget())
    shinyjs::enable("toExcel")
  }
})

observeEvent(input$generateManual, {
  pubchemInfo <- list()
  pubchemInfo$pubchem <- ""
  if (!identical(pubchemInfo$pubchem, NA) & validFormula(removeAllbutLettersDigits(input$chemicalFormula), string = T)){
    pubchemInfo$CAS <- input$chemicalCAS
    pubchemInfo$name <- input$chemicalName
    pubchemInfo$pubchem <- list()
    pubchemInfo$pubchem$MolecularFormula <- removeAllbutLettersDigits(input$chemicalFormula)
    pubchemInfo$pubchem$CanonicalSMILES <- input$chemicalSmiles
    rv$generalInfoTable <- data.frame(parameter = c("Name","CAS","PubChem ID","Formula","Exact Mass","Average Mass","Smiles"),
                                      value = c(ifelse(is.null(pubchemInfo$name),
                                                       "", pubchemInfo$name),
                                                ifelse(is.null(pubchemInfo$CAS),
                                                       "", pubchemInfo$CAS),
                                                " -- ",
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", formulaString(stringToFormula(pubchemInfo$pubchem$MolecularFormula), useMarkdown = T)),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", stringToFormula(pubchemInfo$pubchem$MolecularFormula) %>% formulaToMass(enviPat = TRUE) %>% BBPersonalR::formatDigits(4)()),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", stringToFormula(pubchemInfo$pubchem$MolecularFormula) %>% formulaToMass(enviPat = TRUE, exact = FALSE) %>% BBPersonalR::formatDigits(4)()),
                                                ifelse(identical(pubchemInfo$pubchem, NA),
                                                       "", pubchemInfo$pubchem$CanonicalSMILES)))
    rv$smiles <- pubchemInfo$pubchem$CanonicalSMILES
    rv$additionalInfo <- NA
    shinyjs::enable("toExcel")
  }
})
