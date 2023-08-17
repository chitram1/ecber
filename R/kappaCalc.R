#this function will take in an excel file that represents a matrix and we will calculate
#kappa values

#add individual kappa values
if ("vcd" %in% rownames(installed.packages()) == FALSE) {
  install.packages("vcd")
}

if ("writexl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("writexl")
}

if ("readxl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readxl")
}

library(writexl)
library("vcd")
library(readxl)

#' This function creates a practice confusion matrix to calculate kappa values with.
#' @param dimr This value represents the fixed number of rows for the confusion matrix.
#' @param dimc This value represents the fixed number of columns for the confusion matrix.
#' @param wd The directory in which to write the practice excel file into
#' @return This function doesn't return anything, but saves out the practice confusion matrix to the current directory.
#' @examples
#' createPracticeExcel()
createPracticeExcel <- function(dimr=5, dimc=5, wd = getwd()){
  df <- data.frame(A = c(19, 2, 1, 4, 1),
                   B = c(0, 20, 5, 2, 3),
                   C = c(1, 2, 11, 3, 0),
                   D = c(2, 1, 2, 18, 2),
                   E = c(0, 3, 2, 1, 15))
  rownames(df) = c("A", "B", "C", "D", "E")
  write_xlsx(df, paste0(wd, '/', 'practicekappa.xlsx'))
}

#' This function creates another practice confusion matrix to calculate kappa values with.
#' @param dimr This value represents the fixed number of rows for the confusion matrix.
#' @param dimc This value represents the fixed number of columns for the confusion matrix.
#' @param wd The directory in which to write the practice excel file into
#' @return This function doesn't return anything, but saves out the practice confusion matrix to the current directory.
#' @examples
#' createPracticeExcel2()
createPracticeExcel2 <- function(dimr=4, dimc=4, wd = getwd()){
  df <- data.frame(A = c(56, 115, 0, 0),
                   B = c(4, 121, 0, 0),
                   C = c(0, 4, 0, 0),
                   D = c(0, 0, 0, 0))
  rownames(df) = c("A", "B", "C", "D")
  write_xlsx(df, paste0(wd, '/', 'practicekappa2.xlsx'))
}

#' Calculating Kappa Metrics from a Confusion/Agreement Matrix
#'
#' This function returns all of the individual kappa values for each state/event along with Cohen's kappa, percent agreement, percent by chance, and weighted/unweighted kappa metrics. It invocates the calcCohensKappa function.
#' @param excelfilepath This is the filepath to the excel sheet containing the matrix of agreement values (excluding the row and column totals)
#' @return This function returns a list of all calculations.
#' @examples
#' calcKappaValues(excelfilpath)
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
  weightedKappa = vcd::Kappa(matrixVals)$Weighted['value'] #grab the weighted Kappa values
  kappaWeightedStdError = vcd::Kappa(matrixVals)$Weighted[2]
  names(kappaWeightedStdError) = "kappaWeightedStdError"
  kappaUnweightedStdError = vcd::Kappa(matrixVals)$Unweighted[2]
  names(kappaUnweightedStdError) = "kappaUnweightedStdError"
  kappaVals = cbind(kappaVals, weightedKappa, kappaUnweightedStdError, kappaWeightedStdError)
  individKappas = individualCodeKappas(matrixVals, total)
  return(list(kappaVals, individKappas))
}

#' Calculating Cohen's Kappa, percent agreement, and percent by chance from a confusion matrix
#'
#' This function returns Cohen's kappa, percent agreement, and percent by chance.
#' @param mat The first input is the confusion matrix without row or column sum values
#' @param total The sum of all values in the confusion matrix
#' @return This function returns a data frame of all calculations.
#' @examples
#' calcCohensKappa(confusion_matrix, total)
calcCohensKappa <- function(mat, total = 0){
  if (total == 0){
    total = sum(rowSums(mat))
  }
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


#' Calculating Individual Kappa Values for the K events which were coded for
#'
#' This function takes in a confusion matrix as well as the total of all values in the matrix and calculates the kappa for each of the individual events.
#' @param mat The confusion matrix from both coders with the row and sum totals removed.
#' @param total The sum of all values in the matrix.
#' @return A data frame with the individual code kappas for each event.
individualCodeKappas <- function(mat, total) {
  #the matrix will have the totals removed from it
  cS <- colSums(mat)
  rS <- rowSums(mat)
  events <- colnames(mat)
  ind_vals <- rep(0, length(events))
  for (i in 1:length(events)) {
    shared_val <- mat[i, i]
    mini_mat <- matrix(c(shared_val,
                         rS[i] - shared_val,
                         cS[i] - shared_val,
                         total - (shared_val +
                                    rS[i] - shared_val +
                                    cS[i] - shared_val)),
                       nrow = 2, ncol = 2, byrow = TRUE)
    ind_val <- calcCohensKappa(mini_mat, total)$CohensOmnibusKappa
    ind_vals[i] <- ind_val
  }
  final_res <- t(data.frame(ind_vals))
  colnames(final_res) <- events
  rownames(final_res) <- c("Ind. Kappa Values")
  final_res[is.nan(final_res)] <- 0
  return(final_res)
}
