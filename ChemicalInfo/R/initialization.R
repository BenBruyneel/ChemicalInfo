
startAdducts <- data.frame(
  name = c("[M+H]+","[M+Na]+","[M+K]+","[M+NH4]+", "H2O"),
  adductFormula = c("H", "Na", "K", "NH4", "H2O"),
  adductsNr = rep("1",5),
  adductsPosNeg = rep("+", 5),
  adductCharge = rep("1",5))

startAdditionalInfo <- data.frame(additionalData = c("Solubility","Other Experimental Properties"))