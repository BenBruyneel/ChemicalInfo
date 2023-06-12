
library(enviPat)
library(stringr)
library(massSpectrometryR)
library(gt)
library(BBPersonalR)
library(dplyr)
library(purrr)

str_replace_allMulti <- function(string, patterns, replacements = ""){
  if (length(replacements) == 1){
    replacements <- rep(replacements, length(patterns))
  }
  for (counter in 1:length(patterns)){
    string <- str_replace_all(string, pattern = patterns[counter], replacement = replacements[counter])
  }
  return(string)
}
  
getAdditionalInfo <- function(pubChemID, additionalInfo = NA, verbose = TRUE){
  result <- list()
  if (!identical(additionalInfo, NA)){
    for (counter in 1:length(additionalInfo)){
      result[[counter]] <- pc_sect(id = pubChemID,
                                   section = additionalInfo[counter],
                                   verbose = TRUE)
    }
    names(result) <- additionalInfo
    return(result)
  } else {
    return(NA)
  }
}

createAdditionalInfoTables <- function(additionalInfoResult){
  if (length(additionalInfoResult) > 0){
    addResult <- additionalInfoResult[map_lgl(additionalInfoResult, ~ncol(.x)>0)]
    if (length(addResult) > 0){
      for (counter in 1:length(addResult)){
        addResult[[counter]] <- addResult[[counter]] %>%
          dplyr::select(Result) %>%
          gt() %>%
          tab_options(column_labels.hidden = TRUE,
                      table.align = "left",
                      table.width = pct(100),
                      table.font.size = "1.25") %>%
          tab_header(title = names(addResult)[counter]) %>%
          opt_align_table_header("left")
      }
    } else {
      addResult <- list(
        data.frame(Result = "No additional data exists or could be retrieved") %>%
          gt() %>%
          tab_options(column_labels.hidden = TRUE,
                      table.align = "left",
                      table.width = pct(100),
                      table.font.size = "1.25")
      )
    }
  } else {
    addResult <- list(
      data.frame(Result = "No additional data requested") %>%
        gt() %>%
        tab_options(column_labels.hidden = TRUE,
                    table.align = "left",
                    table.width = pct(100),
                    table.font.size = "1.25")
    )
  }
  return(addResult)
}

# formulas is character vector
generateIsotopePattern <- function(chemicalFormula,
                                   charge = 1,
                                   adduct = NA,
                                   addAdduct = TRUE,
                                   resolution = 10000,
                                   detect = "centroid",
                                   threshold = 0.1){
  data(isotopes, envir = environment(), package = "enviPat")
  if (!is.na(adduct)){
    chemicalFormula <- BBPersonalR::ifelseProper(addAdduct,
                                                 stringToFormula(chemicalFormula) %f+% (charge*stringToFormula(adduct)),
                                                 stringToFormula(chemicalFormula) %f-% (charge*stringToFormula(adduct)))
  } else {
    chemicalFormula <- stringToFormula(chemicalFormula)
  }
  if (!length(chemicalFormula[chemicalFormula != 0]) == 0){
    chemicalFormula <- formulaString(chemicalFormula[chemicalFormula != 0]) # remove elements of zero
  } else {
    return(NA)
  }
  checkFormulas <- check_chemform(isotopes = isotopes,
                                  chemforms = chemicalFormula)
  if (sum(checkFormulas$warning) > 0){
    warning("Error in formula(s)")
    return(NA)
  }
  suppressMessages(
    result <- enviPat::isowrap(isotopes = isotopes,
                               checked = checkFormulas,
                               charge = charge,
                               resmass = FALSE,
                               resolution = resolution,
                               detect = detect,
                               threshold = threshold)
  )
  if (identical(result,"error")){
    return(NA)
  } else {
    return(result)
  }
}


# can take more formulas but output is only the first
generateIsotopePatternGT <- function(chemicalFormula,
                                     charge = 1,
                                     adduct = NA,
                                     addAdduct = TRUE,
                                     resolution = 10000,
                                     detect = "centroid",
                                     threshold = 0.1,
                                     mzAccuracy = 4, abundanceAccuracy = 2){
  result <- generateIsotopePattern(chemicalFormula = chemicalFormula,
                                   charge = charge,
                                   adduct = adduct,
                                   addAdduct = addAdduct,
                                   resolution = resolution,
                                   detect = detect,
                                   threshold = threshold)
  if (identical(result, NA)){
    return(NULL)
  } else {
    result <- as.data.frame(result[[1]])
    return(result %>%
             dplyr::rename(mz = `m/z`, Abundance = "abundance") %>%
             dplyr::mutate(mz = scales::comma(mz, accuracy = 10^-mzAccuracy),
                           Abundance = scales::comma(Abundance, accuracy = 10^-abundanceAccuracy)) %>%
             gt() %>%
             cols_width(mz ~ pct(50), Abundance ~ pct(50)) %>%
             cols_label(mz = "m/z", Abundance = "%") %>%
             cols_align(align = "center", columns = "mz") %>%
             tab_options(table.align = "left",
                         table.width = pct(100),
                         table.font.size = "1.25") %>%
             tab_header(title = "Isotope Pattern") %>%
             opt_align_table_header("left"))
  }
}

# code for m/z tables
mzTable <- function(aFormula, createTable, exact = TRUE, useMarkdown = FALSE, createNames = FALSE){
  createTable$mz <- 0
  for (rowCounter in 1:nrow(createTable)){
    createTable$mz[rowCounter] <- massToMz(formulaToMass(aFormula, enviPat = T, exact = exact),
                                           adducts = as.numeric(createTable$adductsNr[rowCounter]),
                                           adductFormula = ifelseProper(createTable$adductFormula[rowCounter] == "",
                                                                        ifelseProper(createTable$adductCharge < 0,
                                                                                     electronFormula(),
                                                                                     emptyFormula()),
                                                                        stringToFormula(createTable$adductFormula[rowCounter] |> str_replace_allMulti(patterns = "<[/,[:alnum:]]+>"))),
                                           adductCharge = as.numeric(createTable$adductCharge[rowCounter]))
    if (createNames){
      createTable$name[rowCounter] <- paste(c("[M", ifelse(createTable$adductCharge[rowCounter]>0,"+","-"),
                                              ifelse(createTable$adductsNr[rowCounter] == 1,"",
                                                     toString(createTable$adductsNr[rowCounter])),
                                              formulaString(stringToFormula(createTable$adductFormula[rowCounter]), removeSingle = T, useMarkdown = useMarkdown),"]",
                                              ifelse(createTable$adductCharge[rowCounter]>0,
                                                     ifelse(useMarkdown,
                                                            paste(c("<sup>",ifelse(createTable$adductsNr[rowCounter] == 1,"",
                                                                                   toString(createTable$adductsNr[rowCounter])) ,"+</sup>"), collapse = ""),
                                                            paste(c(ifelse(createTable$adductsNr[rowCounter] == 1,"",
                                                                           toString(createTable$adductsNr[rowCounter])),"+"), collapse = "")),
                                                     ifelse(useMarkdown,
                                                            paste(c("<sup>",ifelse(createTable$adductsNr[rowCounter] == 1,"",
                                                                                   toString(createTable$adductsNr[rowCounter])) ,"-</sup>"), collapse = ""),
                                                            paste(c(ifelse(createTable$adductsNr[rowCounter] == 1,"",
                                                                           toString(createTable$adductsNr[rowCounter])),"-"), collapse = "")))),
                                            collapse = "")
    }
  }
  colnames(createTable)[ncol(createTable)] <- "m/z"  
  return(createTable)
}


# code for charge tables
generateChargeTables <- function(aFormula, adductFormula = c(H=1),
                                 exact = TRUE,
                                 adductsNr = 1:4, adductCharge = 1,
                                 addMzs = TRUE,
                                 useMarkdown = FALSE){
  if (adductCharge == 0){
    stop("Adduct charge should not be 0")
  }
  result <- data.frame(name = as.character(),
                       adductFormula = as.character(),
                       adductsNr = as.numeric(),
                       adductCharge = as.numeric())
  for (counter in 1:length(adductsNr)){
    result <- rbind(result, data.frame(
      name = paste(c(ifelse(length(adductFormula |> removeZeros()) != 0,
                            "[M",
                            "M"),
                     ifelse(length(adductFormula |> removeZeros()) != 0,
                            paste(c(
                              ifelse(adductCharge>0,"+","-"),
                              ifelse(abs(adductsNr[counter]) == 1,"",
                                     toString(adductsNr[counter])),
                              formulaString(adductFormula, removeSingle = T, useMarkdown = useMarkdown),
                              "]"), collapse = ""),
                            ""),
                     ifelse(adductCharge>0,
                            ifelse(useMarkdown,
                                   paste(c("<sup>",ifelse(adductsNr[counter] == 1,"",
                                                          toString(adductsNr[counter])) ,"+</sup>"), collapse = ""),
                                   paste(c(ifelse(adductsNr[counter] == 1,"",
                                                  toString(adductsNr[counter])),"+"), collapse = "")),
                            ifelse(useMarkdown,
                                   paste(c("<sup>",ifelse(adductsNr[counter] == -1,"",
                                                          toString(abs(adductsNr[counter]))),"-</sup>"), collapse = ""),
                                   paste(c(ifelse(adductsNr[counter] == -1,"-",
                                                  toString(adductsNr[counter])),""), collapse = "")))),
                   collapse = ""),
      adductFormula = formulaString(adductFormula, removeSingle = T, useMarkdown = useMarkdown),
      adductsNr = adductsNr[counter],
      adductCharge = adductCharge)
    )
  }
  if (addMzs){
    result <- mzTable(aFormula = aFormula, createTable = result, exact = exact, useMarkdown = useMarkdown, createNames = FALSE)
  }
  return(result)
}

generateChargeTablesGT <- function(aFormula, adductFormula = c(H=1),
                                   exact = TRUE,
                                   adductsNr = 1:4, adductCharge = 1,
                                   addMzs = TRUE,
                                   useMarkdown = FALSE,
                                   mzAccuracy = 4){
  generateChargeTables(aFormula = aFormula, adductFormula = adductFormula,
                       exact = exact,
                       adductCharge = adductCharge, adductsNr = adductsNr,
                       useMarkdown = useMarkdown, addMzs = addMzs) %>%
    dplyr::rename(mz = `m/z`, Ion = name) %>%
    dplyr::select(Ion, mz) %>%
    dplyr::mutate(mz = scales::comma(mz, accuracy = 10^-mzAccuracy)) %>%
    gt() %>%
    fmt_markdown(columns = "Ion") %>%
    cols_width(Ion ~ pct(50), mz ~ pct(50)) %>%
    cols_label(mz = "m/z") %>%
    cols_align(align = "right", columns = "mz") %>%
    tab_options(table.align = "left",
                table.width = pct(100),
                table.font.size = "1.25") %>%
    tab_header(title = "Charge states") %>%
    opt_align_table_header("left")
}


generateAdductTable <- function(aFormula, createTable, exact = TRUE, useMarkdown = FALSE){
  createTable$adductCharge <- as.integer(createTable$adductCharge)
  createTable$adductCharge <- map2_int(createTable$adductCharge, createTable$adductsPosNeg, ~ifelse(.y == "-", -(.x), .x))
  createTable$formula <- map(map_chr(createTable$adductFormula, ~formulaString(aFormula %f+% stringToFormula(.x))), ~stringToFormula(.x))
  createTable$adductFormula = ""
  result <- data.frame()
  for (counter in 1:nrow(createTable)){
    result <- bind_rows(result, mzTable(aFormula = createTable$formula[[counter]], exact = exact, createTable = createTable[counter,], useMarkdown = useMarkdown))
  }
  return(result)
}

generateAdductTableGT <- function(aFormula, createTable, exact = TRUE, mzAccuracy = 4, useMarkdown = FALSE){
  generateAdductTable(aFormula = aFormula,
                      createTable = createTable,
                      exact = exact,
                      useMarkdown = useMarkdown) %>%
    dplyr::rename(mz = `m/z`, Ion = name) %>%
    dplyr::select(Ion, mz) %>%
    dplyr::mutate(mz = scales::comma(mz, accuracy = 10^-mzAccuracy)) %>%
    gt() %>%
    fmt_markdown(columns = "Ion") %>%
    cols_width(Ion ~ pct(50), mz ~ pct(50)) %>%
    cols_label(mz = "m/z") %>%
    cols_align(align = "right", columns = "mz") %>%
    tab_options(table.align = "left",
                table.width = pct(100),
                table.font.size = "1.25") %>%
    tab_header(title = "Adducts") %>%
    opt_align_table_header("left")
}

getAdditionalInfo <- function(id, additionalInfo, verbose = TRUE){
  if (!identical(additionalInfo, NA)){
    if (nrow(additionalInfo)>=1){
      additionalInfoResult <- list()
      for (counter in 1:nrow(additionalInfo)){
        additionalInfoResult[[counter]] <- pc_sect(id = id,
                                                   section = additionalInfo$additionalData[counter],
                                                   verbose = verbose)
      }
      names(additionalInfoResult) <- additionalInfo$additionalData
      return(additionalInfoResult)
    }
  }
  return(NA)
}

getAdditionalInfoTable <- function(additionalResult){
  if ((length(additionalResult) > 0) & (!identical(additionalResult, NA))){
    addResult <- additionalResult[map_lgl(additionalResult, ~ncol(.x)>0)]
    if (length(addResult) > 0){
      theResult <- data.frame()
      for (counter in 1:length(addResult)){
        theResult <- bind_rows(theResult, data.frame(info = names(addResult)[counter], Result = addResult[[counter]]$Result))
      }
      theResult <- theResult %>%
        group_by(info) %>%
        gt() %>%
        tab_options(column_labels.hidden = TRUE,
                    table.align = "left",
                    table.width = pct(100),
                    table.font.size = "1.25") %>%
        tab_header(title = "Other Data") %>%
        opt_align_table_header("left") %>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),
          locations = cells_row_groups()
        )
      return(theResult)
    } else {
      return(
        data.frame(Result = "No additional data exists or could be retrieved") %>%
          gt() %>%
          tab_options(column_labels.hidden = TRUE,
                      table.align = "left",
                      table.width = pct(100),
                      table.font.size = "1.25")
      )
    }
  } else {
    return(
      data.frame(Result = "No additional data requested or found") %>%
        gt() %>%
        tab_options(column_labels.hidden = TRUE,
                    table.align = "left",
                    table.width = pct(100),
                    table.font.size = "1.25")
    )
  }
}

removeMarkdown <- function(string){
  return(str_replace_allMulti(string = string,
                              patterns = c("<sub>","</sub>","<sup>","</sup>"),
                              replacements = c("","","[","]")))
}

removeAllbutLettersDigits <- function(string){
  return(str_replace_allMulti(string, patterns = c("(?!\\[)(?!\\])[:punct:]","[:space:]"), replace = ""))
}