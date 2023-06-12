
output$isotopeClusterTable <- renderUI({
  req((rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"] != "") &
        !identical(isotopeClusterWidget(), NA))
  gt_output(outputId = "isotopeClusterTable_gt")
})

output$chargeStatesTable <- renderUI({
  req((rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"] != "") &
        !identical(chargeStatesWidget(), NA))
  gt_output(outputId = "chargeStatesTable_gt")
})

output$adductsTable <- renderUI({
  req((rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"] != "") &
        !identical(adductsWidget(), NA))
  gt_output(outputId = "adductsTable_gt")
})

output$additionalInfo <- renderUI({
  req(!identical(rv$additionalInfo, NA))
  gt_output(outputId = "additionalInfoTable")
})

output$isotopeClusterTable_gt <- render_gt({
  isotopeInfo <- isotopeClusterWidget()
  if (!identical(isotopeInfo, NA)){
    generateIsotopePatternGT(chemicalFormula = removeMarkdown(rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"]),
                             charge = ifelse(isotopeInfo$positive,1,-1),
                             adduct = ifelse(isotopeInfo$adduct == "-",
                                             NA, isotopeInfo$adduct),
                             resolution = isotopeInfo$resolution,
                             detect = "centroid",
                             threshold = isotopeInfo$threshold,
                             mzAccuracy = isotopeInfo$mzAccuracy,
                             abundanceAccuracy = isotopeInfo$abundanceAccuracy)
  }
})

output$chargeStatesTable_gt <- render_gt({
  chargeStateInfo <- chargeStatesWidget()
  if (!identical(chargeStateInfo, NA)){
    generateChargeTablesGT(aFormula = removeMarkdown(rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"]) |> stringToFormula(),
                           adductFormula = ifelseProper(chargeStateInfo$adduct == "H",
                                                        c(H=1),
                                                        emptyFormula()),
                           adductsNr = ifelseProper(chargeStateInfo$positive,
                                                    chargeStateInfo$chargeStates[1]:chargeStateInfo$chargeStates[2],
                                                    rev(-chargeStateInfo$chargeStates[2]:-chargeStateInfo$chargeStates[1])),
                           exact = chargeStateInfo$monoisotopic,
                           adductCharge = ifelse(chargeStateInfo$positive, +1, -1),
                           useMarkdown = T,
                           mzAccuracy = chargeStateInfo$mzAccuracy)
  }
})

output$adductsTable_gt <- render_gt({
  adductsInfo <- adductsWidget()
  if (!identical(adductsInfo, NA)){
    adductsInfo$adductFormula <- map_chr(adductsInfo$adductFormula, ~removeAllbutLettersDigits(.x))
    generateAdductTableGT(aFormula = removeMarkdown(rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"]) |> stringToFormula(),
                          createTable = adductsInfo,
                          exact = input$adductsMonoisotopic,
                          mzAccuracy = input$adductsAccuracy,
                          useMarkdown = T)
  }
})

output$additionalInfoTable <- render_gt({
  if (!identical(rv$additionalInfo, NA)){
    getAdditionalInfoTable(rv$additionalInfo)
  }
})