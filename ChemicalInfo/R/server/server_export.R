
output$toExcel <- downloadHandler(
  filename = function(){
    paste(c("ChemicalInfo-",as.character(Sys.time()), ".xlsx"), collapse = "")
  },
  content = function(file) {
    
    temp_file <- "www/temp"
    
    # rv$generalInfoTable
    isolate(
      formulaX <- removeMarkdown(rv$generalInfoTable$value[rv$generalInfoTable$parameter == "Formula"]) |> stringToFormula()
    )
    isolate(isotopesX <- isotopeClusterWidget())
    isolate(chargeStatesX <- chargeStatesWidget())
    isolate(adductsX <- adductsWidget())
    isolate(generalInfoX <- rv$generalInfoTable)
    generalInfoX$value[generalInfoX$parameter == "Formula"] <- removeMarkdown(generalInfoX$value[generalInfoX$parameter == "Formula"])
    colnames(generalInfoX) <- c("General Info"," ")
    isolate(additionalInfoX <- rv$additionalInfo)
    isolate(additionalInfoTitles <- additionalWidget())
    if (!identical(additionalInfoTitles, NA)){
      additionalInfoTitles <- additionalInfoTitles$additionalData
    }

    if ((formulaX |> formulaString() != "") & !identical(isotopesX, NA)){
      acc <- c(isotopesX$mzAccuracy, isotopesX$abundanceAccuracy)
      isotopesX <- as.data.frame(generateIsotopePattern(chemicalFormula = formulaX |> formulaString(),
                                                        charge = ifelse(isotopesX$positive,1,-1),
                                                        adduct = ifelse(isotopesX$adduct == "-",
                                                                        NA, isotopesX$adduct),
                                                        resolution = isotopesX$resolution,
                                                        detect = "centroid",
                                                        threshold = isotopesX$threshold)[[1]])
      isotopesX[,1] <- map_chr(isotopesX[,1], ~formatDigits(acc[1])(.x))
      isotopesX[,2] <- map_chr(isotopesX[,2], ~formatDigits(acc[2])(.x))
    } else {
      isotopesX <- NA
    }
    if ((formulaX |> formulaString() != "") & !identical(chargeStatesX, NA)){
      acc <- chargeStatesX$mzAccuracy
      chargeStatesX <- generateChargeTables(aFormula = formulaX,
                                            adductFormula = ifelseProper(chargeStateInfo$adduct == "H",
                                                                         c(H=1),
                                                                         emptyFormula()),
                                            adductsNr = ifelseProper(chargeStatesX$positive,
                                                                     chargeStatesX$chargeStates[1]:chargeStatesX$chargeStates[2],
                                                                     rev(-chargeStatesX$chargeStates[2]:-chargeStatesX$chargeStates[1])),
                                            exact = chargeStatesX$monoisotopic,
                                            adductCharge = ifelse(chargeStatesX$positive, +1, -1),
                                            useMarkdown = F)[,-c(2:4)]
      chargeStatesX[,2] <- map_chr(chargeStatesX[,2], ~formatDigits(acc)(.x))
    } else {
      chargeStatesX <- NA
    }
    if ((formulaX |> formulaString() != "") & !identical(adductsX, NA)){
      adductsInfoX$adductFormula <- map_chr(adductsX$adductFormula, ~removeAllbutLettersDigits(.x))
      adductsX <- generateAdductTable(aFormula = formulaX,
                                      createTable = adductsX,
                                      exact = input$adductsMonoisotopic,
                                      useMarkdown = T)[-c(2:6)]
      adductsX[,2] <- map_chr(adductsX[,2], ~formatDigits(input$adductsAccuracy)(.x))
    } else {
      adductsX <- NA
    }
    
    sh1 <-RsheetMulti$new()
    sh1$addTable(tableData = generalInfoX, startRow = 1, startCol = 1)
    addRow <- 3
    if (!identical(isotopesX, NA)){
      sh1$addTable(tableData = isotopesX, startRow = nrow(generalInfoX) + 3, startCol = 1)
      addRow <- max(addRow, nrow(isotopesX) + 2)
    }
    if (!identical(chargeStatesX, NA)){
      sh1$addTable(tableData = chargeStatesX, startRow = nrow(generalInfoX) + 3, startCol = 3)
      addRow <- max(addRow, nrow(chargeStatesX) + 2)
    }
    if (!identical(adductsX, NA)){
      sh1$addTable(tableData = adductsX, startRow = nrow(generalInfoX) + 3, startCol = 5)
      addRow <- max(addRow, nrow(adductsX) + 2)
    }
    addRow <- addRow + 3
    if (!identical(additionalInfoX, NA)){
      for (counter in 1:length(additionalInfoX)){
        if (ncol(additionalInfoX[[counter]]) > 0){
          additionalInfoX[[counter]] <- additionalInfoX[[counter]][, "Result"]
          colnames(additionalInfoX[[counter]]) <- additionalInfoTitles[counter]
          sh1$addTable(tableData = additionalInfoX[[counter]], startRow = nrow(generalInfoX) + addRow, startCol = 1)
          addRow <- addRow + nrow(additionalInfoX[[counter]]) + 2
        }
      }
    }
    
    wb1 <- Rxcel$new(nameFile = temp_file)
    wb1$addSheet(sheetData = sh1)
    wb1$writeExcel(overwrite = T)
    file.copy(paste0(temp_file,".xlsx"), file)
    
  }
)
