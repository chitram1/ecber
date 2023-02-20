#this function will take in an excel file that represents a matrix and we will calculate
#kappa values

#add individual kappa values
if ("vcd" %in% rownames(installed.packages()) == FALSE) {
  install.packages("cvd")
}
library(writexl)
library("vcd")
wd = getwd()

createPracticeExcel <- function(dimr=5, dimc=5){
  df <- data.frame(A = c(19, 2, 1, 4, 1),
                   B = c(0, 20, 5, 2, 3),
                   C = c(1, 2, 11, 3, 0),
                   D = c(2, 1, 2, 18, 2),
                   E = c(0, 3, 2, 1, 15))
  rownames(df) = c("A", "B", "C", "D", "E")
  write_xlsx(df, paste0(wd, '/', 'practicekappa.xlsx'))
}

createPracticeExcel2 <- function(dimr=4, dimc=4){
  df <- data.frame(A = c(56, 115, 0, 0),
                   B = c(4, 121, 0, 0),
                   C = c(0, 4, 0, 0),
                   D = c(0, 0, 0, 0))
  rownames(df) = c("A", "B", "C", "D")
  write_xlsx(df, paste0(wd, '/', 'practicekappa2.xlsx'))
}

calcKappaValues <- function(excelfilepath){
  matrixVals <- read_excel(excelfilepath)
  matrixVals <- data.matrix(matrixVals) #matrix of vals

  matrixVals[is.na(matrixVals)] <- 0
  totalColVals <- colSums(matrixVals)
  matrixValsFull <- rbind(matrixVals, totalColVals) #adding row
  totalRowVals <- rowSums(matrixValsFull)
  matrixValsFull <- cbind(matrixValsFull, totalRowVals)
  total = matrixValsFull[nrow(matrixValsFull), nrow(matrixValsFull)]
  #Calculating Cohen's kappa
  kappaVals <- calcCohensKappa(matrixVals, total)
  row.names(kappaVals)[1] = "name"
  weightedKappa = Kappa(matrixVals)$Weighted['value'] #grab the weighted Kappa values
  kappaWeightedStdError = Kappa(matrixVals)$Weighted[2]
  names(kappaWeightedStdError) = "kappaWeightedStdError"
  kappaUnweightedStdError = Kappa(matrixVals)$Unweighted[2]
  names(kappaUnweightedStdError) = "kappaUnweightedStdError"
  kappaVals = cbind(kappaVals, weightedKappa, kappaUnweightedStdError, kappaWeightedStdError)
  return(kappaVals)
}

calcCohensKappa <- function(mat, total){
  prob_mat <- mat / total
  nr <- nrow(mat)
  nc <- ncol(mat)
  Po <- 0
  for (i in 1:nr) {
    Po <- Po + mat[i,i]
  }
  Po <- Po / total
  Pc <- 0
  allRowSums = rowSums(mat)
  allColSums = colSums(mat)
  for (i in 1:nr) {
    Pc <- Pc + (allRowSums[i] / total)*(allColSums[i] / total)
  }
  k <- (Po - Pc) / (1-Pc)
  resultsdf <- data.frame('CohensOmnibusKappa'=k,
                            'PercentAgreement'=Po,
                            'PercentByChance'=Pc,
                          stringsAsFactors = F)
  return(resultsdf)
}
                                                                                                               #this function will take in the K events which were coded for
individualCodeKappas <- function(K_events) {

}
