#Notes
#tolerance of 0 means that the seconds and behaviors need to match up exactly
#tolerance of 1 means that we accept similarities within a 1 second boundary

rm(list=ls(all=TRUE))		#clear workspace of all variables
graphics.off() #close any open grapics window

if ("arsenal" %in% rownames(installed.packages()) == FALSE) {
  install.packages("arsenal")
}

if ("compareDF" %in% rownames(installed.packages()) == FALSE) {
  install.packages("compareDF")
}

if ("writexl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("writexl")
}

if ("devtools" %in% rownames(installed.packages()) == FALSE) {
  install.packages("devtools", dependencies = TRUE)
}

if ("ecber" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github('chitram1/ecber')
}

if ("vcd" %in% rownames(installed.packages()) == FALSE) {
  install.packages("vcd")
}

# the following code requires the package(s):
library(openxlsx)
library(readxl)    #opening xlsx
library(writexl)
library(gridExtra) #for creating graphics of tables
library(tidyverse)
library(DescTools) #for using Overlap()
library(arsenal) #new packages useful for comparing data
library(compareDF)
library(dplyr)
library(ecber)
library(vcd)

#will have to change based on structure of user directory
#source('./reliabilityRcode/functionsSS&A&B.R')
#source('../ecber/R/KappaCalc.R')
setwd('/Users/chitramac/Desktop/HerdLab/testfiles/')
#directory from which to get user input files from
wd <- getwd()
cur_dir <- "6008"
directory <- paste0(wd,"/", cur_dir)
#both csv files to read from
filename1 <- "6008BECNR.csv"
filename2 <- "6008BECEU.csv"


#we will select a collection of this list to look into
eventlist <- list(
  #c("HoldingBaby","NotHoldingBaby","CantTellHolding"),
  #c('ManipulatingObject','NoObjectInHand','ActivityNotVisible'),
  #c("LookAtMomActivity","NotLookAtMomActivity","CantTellLooking"),
  #c("positive","neutral","negative","CantTellAffect"),
  c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")
)
tolerance <- 0 #how many seconds we are comparing for equality
#tolerance is 0 for perfect equality
file_seconds <- 300 #total number of seconds in video

#STEP 1: convert csv files to excel format
filepath1 <- file.path(paste0(directory,'/',filename1))
filepath2 <- file.path(paste0(directory, '/', filename2))
#file extension of both files
ext1 <- strsplit(filepath1, split=".", fixed=T)[[1]][2]
ext2 <- strsplit(filepath2, split=".", fixed=T)[[1]][2]
#converting the csv file to excels
if(ext1 == "csv" | ext2 == "csv"){
  if(ext1 == "csv"){
    outfile <- paste0(strsplit(filepath1,"[.]")[[1]][1],"_t.xlsx")
    x <- Boris2NoldusFileReShape(rel=read.csv(filepath1, header = T, stringsAsFactors = T))
    write.xlsx(x, outfile)
    filepath1 <- outfile
  }
  
  if(ext2 == "csv"){
    outfile <- paste0(strsplit(filepath2,"[.]")[[1]][1],"_t.xlsx")
    x <- Boris2NoldusFileReShape(rel=read.csv(filepath2, header = T, stringsAsFactors = T))
    write.xlsx(x, outfile)
    filepath2 <- outfile
  }
}

#STEP 2: Calculate Agreement on Duration Events

#technically tolerance is 0 since it requires full equality
#set tolerance to 1, 2, etc. to see the change
output <- buildMatrix(filepath1, filepath2, eventlist, file_seconds)
#tolerance is something other than 1 so we have to use the other function
tolerance = 1
toleranceMatrix <- createToleranceMatrix(filepath1, filepath2, eventlist, tolerance, file_seconds = 300)
View(output$confusion_matrix)
View(toleranceMatrix$confusion_matrix)

resultsdf <- data.frame('Perc_Agree_No_Tolerance'=output$percent_agreement_raw,
                        'Perc_Agree_With_Tolerance'=toleranceMatrix$percent_agreement_raw,
                        stringsAsFactors = F)

kappas <- createToleranceAndKappaValsFromMatrix(output$confusion_matrix)
rownames(kappas) <- ("Without Tolerance")
kappas2 <- createToleranceAndKappaValsFromMatrix(toleranceMatrix$confusion_matrix)
rownames(kappas2) <- ("With Tolerance")
kappas = rbind(kappas, kappas2)
View(resultsdf)
View(kappas)
input_mat <- toleranceMatrix$confusion_matrix[1:nrow(toleranceMatrix$confusion_matrix)-1,1:ncol(toleranceMatrix$confusion_matrix)-1]
tolerance_mat_total <- toleranceMatrix$confusion_matrix[nrow(toleranceMatrix$confusion_matrix), ncol(toleranceMatrix$confusion_matrix)]
indiv_df <- individualCodeKappas(input_mat, tolerance_mat_total)
View(indiv_df)
