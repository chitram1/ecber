### ========== user-defined functions ===================
formatData <- function(filepath,events,file_seconds=600){

  #Reformat into stop/start
  ff <- read_excel(filepath)
  ff <- comparisonDfcreator(ff)
  ff <- dfSorter(ff)

  #Keep only rows in events
  i <- which(ff$Behavior %in% events)
  f1 <- ff[i,]

  ##Round to nearest 100th of a sec
  f1$Start_Time <- round(f1$Start_Time,2)
  f1$Stop_Time <- round(f1$Stop_Time,2)

  ##Convert to long format (100ths of a second)
  f1$Start_Time <- f1$Start_Time * 100 + 1
  f1$Stop_Time <- f1$Stop_Time * 100
  file_seconds <- file_seconds * 100

  ####File length adjustments
  ##Truncate file to 60000 if needed
  i <- which(f1$Stop_Time > file_seconds)
  if(length(i) >= 1) f1$Stop_Time[i[1]] <- file_seconds	#Truncate first row that goes over to 60000 (600sec)

  i <- which(f1$Stop_Time > file_seconds)
  if(length(i) > 1) f1 <- f1[-i,] #Drop extra rows

  #Create empty output file
  out <- NULL


  #Populate output file with data
  for(n in 1:nrow(f1)){
    row1 <- f1[n,]

    ##Check for missing rows, add as NA
    if(n > 1){
      if(!row1$Start_Time-1 == f1[n-1,]$Stop_Time){
        # print(n)
        # print(f1[n-1,]$Stop_Time)
        # print(row1$Start_Time-1)
        # flush.console()

        start1 <- f1[n-1,]$Stop_Time+1
        stop1 <- f1[n,]$Start_Time-1

        out <- rbind(out, cbind(start1:stop1,NA))
      }
    }
    start_time = row1$Start_Time
    stop_time = row1$Stop_Time
    beh = row1$Behavior
    if (is.null(out)) {
      out = cbind(start_time:stop_time,beh)
    }
    else {
      out <- rbind(out, cbind(start_time:stop_time,beh))
    }
  }

  #Clean up output
  out <- as.data.frame(out)
  names(out) <- c("time","behavior")
  out$observer <- f1$Observer[1]
  out$time <- as.numeric(out$time) / 100

  #Gaps in data
  i <- which(is.na(out$behavior))
  if(length(i) > 0) out$behavior[i] <- "data_missing"

  ##Data checks
  #1. Check that they are the correct length. If not, output error on where the issue is
  if(!nrow(out) == file_seconds){

    #Overlapping times
    nbad <- nrow(out) - file_seconds
    if(nbad > 0){
      bad <- which(!row.names(out) == (out$time * 100))
      bad <- bad[1:nbad] #only count the ones that are over time
      times <- sort(unique(out$time[bad]))

      ##Prepare error message
      mintime <- min(times)
      maxtime <- max(times)

      rows <- which(ff$Start_Time <= maxtime & ff$Stop_Time >= mintime & ff$Behavior %in% events)

      err <- paste0("Overlapping rows in file ",filepath,":\n")
      badrows <- paste(apply(ff[rows,],1,paste,collapse = " "),collapse="\n")
      output <- c(err,badrows)

      stop(paste(output))

      #Too short
    }else if(nbad < 0){
      output <- paste("Total behaviors in file",filepath,"\nfor:", paste(events, collapse=","),"add up to",(file_seconds+nbad)/100,"seconds instead of",file_seconds/100,"seconds.")
      stop(paste(output))
    }
  }

  return(out)
}

#' Creating Agreement/Confusion Matrix between 2 coders with a tolerance of 0
#'
#' This function takes in the 2 filepaths and the events that we are looking at for coder similarity. We create an agreement/confusion matrix comparing matches between codes in both files.
#' @param filepath1 Filepath to first excel file.
#' @param filepath2 Filepath to second excel file.
#' @param eventlist List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.
#' @param file_seconds The total number of seconds coded for in each file.
#' @return We return a list of two entries: the first being the confusion matrix and the second being the percent agreement metric.
#' @examples
#' buildMatrix(filepath1 = './6008BECEU.xlsx', filepath2 = './6008BECNR.xlsx', eventlist = list(c("positive","neutral","negative","CantTellAffect"), c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")), 300)
buildMatrix <- function(filepath1,filepath2,eventlist,file_seconds=600){
  library(dplyr)
  #Set up matrix of all events in eventlist for confusion matrix
  matvals <- c(unlist(eventlist),"data_missing")
  mat <- matrix(data=NA, nrow=length(matvals), ncol=length(matvals))
  colnames(mat) <- matvals
  rownames(mat) <- matvals

  #setup for agree() later
  obs1total <- NULL
  obs2total <- NULL

  for (events in eventlist){
    #1. read and format data using formatData
    obs1 <- formatData(filepath1, events,file_seconds=file_seconds)
    obs2 <- formatData(filepath2, events,file_seconds=file_seconds)

    obs1total <- rbind(obs1total, obs1)
    obs2total <- rbind(obs2total, obs2)

    #2. Add counts to matrix
    tbl <- table(obs1$behavior, obs2$behavior)
    dtf <- as.data.frame(tbl)
    dtf$Var1 <- as.character(dtf$Var1)
    dtf$Var2 <- as.character(dtf$Var2)

    for(i in 1:nrow(dtf)){
      d <- dtf[i,]
      rownum <- which(rownames(mat) == d$Var1)
      colnum <- which(colnames(mat) == d$Var2)
      mat[rownum,colnum] <- d$Freq
    }

  }

  #drop any names where all rows and columns are NA
  for(val in matvals){
    rownum <- which(rownames(mat) == val)
    colnum <- which(colnames(mat) == val)

    if(all(is.na(mat[rownum,])) & all(is.na(mat[,colnum]))){
      mat <- mat[-rownum,]
      mat <- mat[,-colnum]
    }
  }

  mat <- mat / 100 #back into seconds

  #Data checks:
  #Sum of rows should = (# rows / 100) * number of events in eventlist
  #r1 <- sum(rowSums(mat, na.rm=T))

  ##Temp fix: Floating point issue with this sometimes, add a tiny numer and set to integer
  r1 <- as.integer(sum(rowSums(mat, na.rm=T)) + .000001)
  r1back <- as.integer(sum(rowSums(mat, na.rm=T)) + 1)
  r2 <- nrow(obs1)/100 * length(eventlist)

  if(!r1 == r2){
    if (!r1back == r2) {
      stop(paste("Number of observations for obs1 are",r1,"but should be",r2))
    }
  }

  #Sum of cols should also = (# rows / 100) * number of events in eventlist
  r1 <- as.integer(sum(colSums(mat, na.rm=T)) + .000001)
  r1back <- as.integer(sum(rowSums(mat, na.rm=T)) + 1)
  r2 <- nrow(obs2)/100 * length(eventlist)

  if(!r1 == r2){
    if (!r1back == r2) {
      stop(paste("Number of observations for obs1 are",r1,"but should be",r2))
    }
  }


  ##Step 3: Finish up matrix calculations
  #Calculate % agreement
  agree <- sum(diag(mat), na.rm=T)
  tot <- r1
  perc_agree <- agree/tot * 100

  # #Calculate % agreement w/ tolerance

  # #Turn values into numeric for irr::agree
  # b1 <- as.numeric(factor(obs1total$behavior, levels=matvals))
  # b2 <- as.numeric(factor(obs2total$behavior, levels=matvals))

  # perc_agree_tol <- Agree(cbind(b1,b2), tolerance=tolerance)$value

  #Calculate totals
  rowtotals <- apply(mat,2,sum, na.rm=T)
  coltotals <- apply(mat,1,sum, na.rm=T)

  mat <- rbind(mat, rowtotals)
  mat <- cbind(mat, c(coltotals,r1))

  rowname <- paste0(obs2$observer[1],"_totals")
  colname <- paste0(obs1$observer[1],"_totals")

  rownames(mat)[nrow(mat)] <- rowname
  colnames(mat)[ncol(mat)] <- colname

  return(list(confusion_matrix=mat,percent_agreement_raw=perc_agree))
}

checkData <- function(dta){

  ##Data quality checks
  err_msg <- NULL

  # ##Missing data check:
  # i <- which(is.na(dta$behavior))
  # if(length(i) > 0){
  # err_msg <- c(err_msg,paste("Missing data for observer",dta$observer[1],"for category",events[1],"\nfrom time",dta[i[1],"time"],"to",dta[i[length(i)],"time"]))
  # }

  ##Overlapping times check:
  if(length(which(duplicated(dta$time))) > 0){
    dupes <- which(duplicated(dta$time) | duplicated(dta$time, fromLast=T))

    time1 <- min(dta[dupes,"time"])
    time2 <- max(dta[dupes,"time"])
    user <- dta$observer[1]
    b <- paste(unique(dta[dupes,"behavior"]),collapse=",")

    err_msg <- c(err_msg,paste0("Duplicate data found for user ",user,":\n",b," present from time ",time1," to time ",time2))
  }



  err_msg
}

createEventLog <- function(comparisonDf_file1,comparisonDf_file2,tol,filepath){

  comparisonDf_file1_sorted <- dfSorter(comparisonDf_file1)
  comparisonDf_file2_sorted <- dfSorter(comparisonDf_file2)

  #initialize large empty dataframe for the comparison log to be filled
  #later withing a for loop.  Crude, but works ok for small data sets
  comparisonLog <- as.data.frame(matrix(vector(),0,8)) %>% setNames(c("Start_Time", "Stop_Time", "Behavior", "Observer", "Start_Time", "Stop_Time", "Behavior", "Observer") )
  y <- data.frame(comparisonDf_file1$Start_Time-tol, comparisonDf_file1$Stop_Time+tol)
  y_actual <- data.frame(comparisonDf_file1$Start_Time, comparisonDf_file1$Stop_Time)

  #next, the other data file (that is not sorted) will then be checked
  #against the sorted data file above (comparisonDf_file2_sorted, to check
  #for overlaps in start/stop times then match sequence of events based on a calculated percent agreement
  for (i in 1:nrow(comparisonDf_file2_sorted)){

    x <- c(comparisonDf_file2_sorted$Start_Time[i]-tol, comparisonDf_file2_sorted$Stop_Time[i]+tol)
    x_actual <- c(comparisonDf_file2_sorted$Start_Time[i], comparisonDf_file2_sorted$Stop_Time[i])

    #calculating overlaps (or agreements) between interval x, with intervals in y
    tagree_wtol <- Overlap(x,y)
    overlapIndices_wtol <- which(tagree_wtol > 0)

    #check if "Vocal" behavior is in y among those overlapping intervals AND not in x, then remove
    #these because we don't want a potential match. Noldus seems to only match "Vocal" behavior in x
    #with "Vocal" behaviors in y as seen in the comparison logs, so we do not want "Vocal" behavior in
    #x matching with other behaviors that are not "Vocal"
    if ("Vocal" %in% comparisonDf_file1$Behavior[overlapIndices_wtol ] & comparisonDf_file2_sorted$Behavior[i] != "Vocal"){
      #if there are "Vocal" behaviors in the overlaps in y, and x is not a "Vocal" behavior,
      #then filter out "Vocal" behaviors in the overlaps in y
      indexRemoved <- overlapIndices_wtol[which(comparisonDf_file1$Behavior[overlapIndices_wtol ] == "Vocal")]
      overlapIndices_wtol <- setdiff(overlapIndices_wtol,indexRemoved)
    }

    #calculate disagreements between interval x, with intervals in y where there are
    #overlapping intervals using x and y values with overlap tolereance window added
    tdisagree_wtol <- rep(NA,nrow(y))
    for(j in overlapIndices_wtol){
      a1 <- x[1]
      b1 <- x[2]
      a2 <- y[j,1]
      b2 <- y[j,2]
      tdisagree_wtol[j] <- calculateTdisagree(a1,b1,a2,b2,tagree_wtol[j]) #calculate disagreement
    }
    #calculate percent agreement between interval x, and all those other intervals in y
    #for which there is overlap
    percentAgree_wtol <- tagree_wtol[overlapIndices_wtol]/(tdisagree_wtol[overlapIndices_wtol]+tagree_wtol[overlapIndices_wtol])

    #find indices of largest percent agreement
    largestPAwtol_index <- which(percentAgree_wtol == max(percentAgree_wtol))

    #lets match sequences based on percentaAgree_wtol (with tolerance window added)
    if(length(largestPAwtol_index)>1 & comparisonDf_file2_sorted$Behavior[i] != "Vocal"){ #if there are multiple matches with largest % agreement

      nameMatchIndex <- which(comparisonDf_file2_sorted$Behavior[i] == comparisonDf_file1$Behavior[overlapIndices_wtol ] )
      block <- cbind(comparisonDf_file2_sorted[i,], comparisonDf_file1[overlapIndices_wtol[nameMatchIndex], ])
      comparisonLog <- rbind(comparisonLog,block)

    }else if(comparisonDf_file2_sorted$Behavior[i] == "Vocal"){
      #Inserting special rules on synchronizing events that are type "Vocal":
      #if Behavior is "Vocal" in x and there is ALSO "Vocal" in y in those intervals with overlaps
      nameMatchIndex <- which(comparisonDf_file2_sorted$Behavior[i] == comparisonDf_file1$Behavior[overlapIndices_wtol])
      if(length(nameMatchIndex) > 0){
        index <- which(percentAgree_wtol[nameMatchIndex] == max(percentAgree_wtol[nameMatchIndex])) #choose the larger of the %pa in case there are multiple "Voice" behaviors in y
        nameMatchIndex <- nameMatchIndex[index]
        if(length(nameMatchIndex)>1){
          nameMatchIndex <- nameMatchIndex[1] #just pick the first if there are repeats
        }
        block <- cbind(comparisonDf_file2_sorted[i,], comparisonDf_file1[overlapIndices_wtol[nameMatchIndex], ])
        comparisonLog <- rbind(comparisonLog,block)
      }else{ #if there is no behavior name "Vocal" in y, search for closest "Vocal" in y and force a match
        #this seems to be what Noldus is doing from looking at the comparison log screencaptures,
        #although there really is no way to verify if this is correct without looking at it's source code
        #lets crudely search for nearest "Vocal" in all of y based on nearest Start_Time
        yindexWvocalBehaviors <- which(comparisonDf_file1$Behavior=="Vocal")
        timediff <- abs(comparisonDf_file2_sorted$Start_Time[i]-comparisonDf_file1$Start_Time[yindexWvocalBehaviors] )
        index <- yindexWvocalBehaviors[which(timediff==min(timediff))]

        block <- cbind(comparisonDf_file2_sorted[i,], comparisonDf_file1[index, ])
        comparisonLog <- rbind(comparisonLog,block)
      }#if

    }else{
      block <- cbind(comparisonDf_file2_sorted[i,], comparisonDf_file1[overlapIndices_wtol[largestPAwtol_index], ])
      comparisonLog <- rbind(comparisonLog,block)
    }

  }#for i

  # go through comparison log row by row and mark "Y" next to those events that
  # are overlapping with the same Behavior name, or mark "N" next to those events
  # that are overlapping but with DIFFERENT Behavior names
  agreement_actual <- numeric(nrow(comparisonLog))
  agreement_wtol <- numeric(nrow(comparisonLog))
  disagreement_actual <- numeric(nrow(comparisonLog))
  disagreement_wtol <- numeric(nrow(comparisonLog))
  pa_actual <- numeric(nrow(comparisonLog))
  Results <- rep("NA",nrow(comparisonLog))
  pa_wtol <- numeric(nrow(comparisonLog))
  for(i in 1:nrow(comparisonLog)){
    name1 <- comparisonLog[i,3]
    name2 <- comparisonLog[i,7]
    x1_wtol <- c(comparisonLog[i,1]-tol, comparisonLog[i,2]+tol)
    x2_wtol <- c(comparisonLog[i,5]-tol, comparisonLog[i,6]+tol)
    x1_actual <- c(comparisonLog[i,1], comparisonLog[i,2])
    x2_actual <- c(comparisonLog[i,5], comparisonLog[i,6])
    testOverlap_wtol <- Overlap(x1_wtol,x2_wtol) #check overlap with tolerance added
    if(testOverlap_wtol > 0){  #if events overalp
      if(name1 == name2){    #further, if events also have the same names
        Results[i] <- "Y"
        if(name1 != "Vocal"){
          agreement_actual[i] <- Overlap(x1_actual, x2_actual)
          if(agreement_actual[i] == 0){ #Overlap(x1_wtol,x2_wtol) may show overlap, while Overlap(x1_actual, x2_actual) may not
            # do nothing
          }else{
            disagreement_actual[i] <- calculateTdisagree(x1_actual[1],x1_actual[2],
                                                         x2_actual[1],x2_actual[2],
                                                         agreement_actual[i])
          }#if agreement_actual

          pa_actual[i] <- agreement_actual[i]/(agreement_actual[i]+disagreement_actual[i])

          agreement_wtol[i] <- testOverlap_wtol
          disagreement_wtol[i] <- calculateTdisagree(x1_wtol[1],x1_wtol[2],
                                                     x2_wtol[1],x2_wtol[2],
                                                     agreement_wtol[i])
          pa_wtol[i] <- agreement_wtol[i]/(agreement_wtol[i]+disagreement_wtol[i])
        } #if name1!="Vocal"
      }else{
        Results[i] <- "N"
      }
      #might have to add special case for "Vocal"
    }else{
      Results[i] <- "N"
    }
  }#for i

  comparisonLog <- cbind(comparisonLog,Results,agreement_actual, disagreement_actual)
  return(comparisonLog)
  #write.csv(comparisonLog,paste0(filepath,"comparisonLog_",file1Coder,"_",file2Coder,".csv"))

}

#' Calculating percent agreement and kappa values between 2 files
#'
#' This function takes in the 2 filepaths, an event list, a tolerance value, and the total number of file seconds. An intermediate step of the function is the calculation of the confusion matrix, and this is used to calculate the percent agreement values along with the kappa values for comparing coder report similarity.
#' @param filepath1 Filepath to first excel file.
#' @param filepath2 Filepath to second excel file.
#' @param eventlist List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.
#' @param tolerance Tolerance value for the amount of seconds that we buffer for comparing coder similarity, a tolerance of 0 requiring the same exact coding for each second
#' @param file_seconds The total number of seconds coded for in each file.
#' @return This function returns a data frame with percent agreement, percent by chance, and various kappa values.
#' @examples
#' createToleranceAndKappaValsFromFiles(filepath1, filepath2, eventlist, tolerance, file_seconds)
createToleranceAndKappaValsFromFiles <- function(filepath1, filepath2, eventlist, tolerance, file_seconds = 300) {
  output <- createToleranceMatrix(filepath1, filepath2, eventlist, tolerance, file_seconds)
  output <- output$confusion_matrix
  return(calcKappaValuesFromMatrix(output))
}

#' Calculating percent agreement and kappa values from a confusion matrix
#'
#' This function takes in the confusion matrix already calculated from 2 excel files, and returns the kappa values, percent by chance, and percent agreement values.
#' @param output The confusion matrix already calculated between two files
#' @return This function returns a data frame with percent agreement, percent by chance, and various kappa values.
#' @examples
#' createToleranceAndKappaValsFromMatrix(confusion_matrix)
createToleranceAndKappaValsFromMatrix <- function(output) {
  return(calcKappaValuesFromMatrix(output))
}

comparisonDfcreator <- function(datafile){
  # input:
  # 	datafile : coded data table imported from excel
  # ouput:
  #	comparisonDf : dataframe showing start and stop times of behavior categories
  N <- nrow( datafile %>% filter(Event_Type == "State start" | Event_Type == "State point") )
  Start_Time <- numeric(N)
  Stop_Time <- numeric(N)
  Observer <- rep(NA,N)
  Behavior <- rep(NA,N)
  # Event_type <- rep(NA,N)
  k <- 0
  for (i in 1:nrow(datafile)){
    if( (datafile$Event_Type[i]=='State start') ){ #find start/stop of state events
      k <- k + 1
      Start_Time[k] <- datafile$Time_Relative_sf[i]
      Behavior[k] <- datafile$Behavior[i]
      Observer[k] <- datafile$Observation[i]
      # Event_type
      for(j in (i+1):nrow(datafile)){
        if( (datafile$Behavior[j]==datafile$Behavior[i]) & (datafile$Event_Type[j]=='State stop') ){
          Stop_Time[k] <- datafile$Time_Relative_sf[j]
          break
        }# if
      }#for
    }#if
    if( (datafile$Event_Type[i]=='State point') ){  #start/stop of pont events
      k <- k + 1
      Behavior[k] <- datafile$Behavior[i]
      Observer[k] <- datafile$Observation[i]
      Start_Time[k] <- datafile$Time_Relative_sf[i]
      Stop_Time[k] <- Start_Time[k]
    }#if
  }#for
  comparisonDf <- data.frame(Start_Time, Stop_Time, Behavior, Observer)
}#function

dfSorter <- function(datafile){
  #input:
  #	datafile : coded data table imported from excel
  #output:
  #	comparisonDf_sorted : dataframe sorted by start_time in ascending order and alphabelized by behavior
  startTimes_file <- sort(unique(datafile$Start_Time),decreasing=F)
  for(i in 1:length(startTimes_file)){
    #mutually exclusive events can start at the same time, so first we group
    #data by ascending start times
    dfgroupedByStartTime <- datafile %>% filter(Start_Time==startTimes_file[i])
    #next, group Behaviors alphabetically
    dfgroupedByBehavior_indices <- order(dfgroupedByStartTime$Behavior)
    dfOrderedByBehavior <- dfgroupedByStartTime[dfgroupedByBehavior_indices, ]

    #building new grouped and ordered dataframe
    if(i == 1){
      comparisonDf_sorted <- dfOrderedByBehavior
    }else{
      comparisonDf_sorted <- rbind(comparisonDf_sorted, dfOrderedByBehavior)
    }#if

  }#for i
  return(comparisonDf_sorted)
}#dfSorter

calculateTdisagree <- function(a1,b1,a2,b2,toverlap){
  #input:
  #	limits of range 1: (a1, b1)
  #	limits of range 2: (a2, b2)
  #	toverlap: the range of overlap between range 1 and range 2
  #	NOTE: This function only works of range 1 and range 2 are overlapping
  #output:
  #	tdisagree: disagreement between range 1 and range 2
  if( ((a1>=a2)&(b1<=b2)) | ((a1<=a2)&(b1>=b2))){ #if one interval is completey within another
    tdisagree <- abs((b1-a1)-(b2-a2))
  }else{
    tdisagree <- abs((b1-a1)+(b2-a2)-2*toverlap) #if two intervals slightly overlap
  }#if
}#calculateTdisagree

#' Calculating Kappa metrics and Percent Agreement From a Confusion Matrix
#'
#' This function takes in as input a confusion matrix without the row or column sums, and calculates Cohen's kappa, unweighted and weighted kappa standard error, weighted kappa, percent agreement, and percent by chance values
#' @param matrixVals Confusion matrix without row and column sums
#' @return We return a data frame of all the metrics we are calculating.
#' @examples
#' calcKappaValuesFromMatrix(agreement_matrix)
calcKappaValuesFromMatrix <- function(matrixVals){
  matrixVals <- data.matrix(matrixVals) #matrix of vals
  nr <- nrow(matrixVals)
  nc <- ncol(matrixVals)

  matrixVals <- matrixVals[1:nr-1, 1:nc-1]
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

add_tolerance_per_event <- function(valsindffnotdff2, dff2, timechange_event, cur_confusion_matrix, tolerance) {
  if (nrow(timechange_event) == 0){
    return(cur_confusion_matrix)
  }
  this_event = timechange_event$behavior[1] #name of event
  dff_coder_behavior = ""
  cur_start_time = 0
  if (nrow(valsindffnotdff2) >= 1 && nrow(timechange_event) >= 1) {
    dff_prev_coder_behavior <- this_event
    cur_start_time = valsindffnotdff2[which(valsindffnotdff2$behavior == this_event)[1],]$time
    dff2_coder_behavior <- dff2[which(dff2$time == cur_start_time), ]$behavior
    cur_start_time <- cur_start_time * 100 #convert to long format, we already multiplied by 100 before as well
  }
  for(i in 1:nrow(timechange_event)) {
    #iterates through every row of time changes
    cur_row <- timechange_event[i,] #first point where the time data changes
    cur_time <- cur_row$time #time at which data changes between coders
    if (i == nrow(timechange_event)) {
      #cur_end_time = valsindffnotdff2[length(which(valsindffnotdff2$behavior == this_event)), ]$time
      cur_end_time = valsindffnotdff2[which(valsindffnotdff2$behavior == this_event)[length(which(valsindffnotdff2$behavior == this_event))], ]$time
    } else {
      get_ind <- which(valsindffnotdff2 == cur_time) - 1
      cur_end_time <- valsindffnotdff2[get_ind, ]$time
    }
    cur_end_time <- cur_end_time * 100 #convert to long format
    time_diff <- cur_end_time - cur_start_time
    time_diff <- time_diff / 100 #return back to regular time formatting
    time_diff <- round(time_diff, 2) #round to 2 decimal points
    mat_dff_ind = which(rownames(cur_confusion_matrix) == dff_prev_coder_behavior) #index of behavior for accessing matrix
    mat_dff2_ind = which(rownames(cur_confusion_matrix) == dff2_coder_behavior)
    if(time_diff > tolerance){
      time_diff = tolerance
    }
    if (time_diff < 0){
      time_diff = 0
    }
    cur_confusion_matrix[mat_dff_ind, mat_dff_ind] = cur_confusion_matrix[mat_dff_ind, mat_dff_ind] + time_diff
    cur_confusion_matrix[mat_dff_ind, mat_dff2_ind] = cur_confusion_matrix[mat_dff_ind, mat_dff2_ind] - time_diff

    get_ind <- which(valsindffnotdff2 == cur_time)
    cur_start_time = valsindffnotdff2[get_ind, ]$time * 100
    dff_prev_coder_behavior = cur_row$behavior
    dff2_coder_behavior = dff2[which(dff2$time == cur_time), ]$behavior
  }
  cur_confusion_matrix = pmax(cur_confusion_matrix, 0)
  return(cur_confusion_matrix)
}

#' Creating Agreement/Confusion Matrix between 2 coders with tolerance of n
#'
#' This function takes in the 2 filepaths and the events that we are looking at for coder similarity. We create an agreement/confusion matrix comparing matches between codes in both files. This is the same as the buildMatrix function except that we can custommize the tolerance values.
#' @param filepath1 Filepath to first excel file.
#' @param filepath2 Filepath to second excel file.
#' @param eventlist List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.
#' @param tolerance The tolerance value for increasing the buffer of comparison (0 means that the codes need to be exactly the same at each given second with no leniency, a tolerance of 1 means that we will count the codes as equal as long as they were recorded within 1 second of each other)
#' @param file_seconds The total number of seconds coded for in each file.
#' @return We return a list of two entries: the first being the confusion matrix and the second being the percent agreement metric.
#' @examples
#' createToleranceMatrix(filepath1 = './6008BECEU.xlsx', filepath2 = './6008BECNR.xlsx', eventlist = list(c("positive","neutral","negative","CantTellAffect"), c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")), 300)
createToleranceMatrix <- function(filepath1,filepath2,eventlist,tolerance=0,file_seconds=300) {
  cur_confusion_mat = buildMatrix(filepath1, filepath2, eventlist, file_seconds)
  cur_confusion_matrix = cur_confusion_mat$confusion_matrix #original matrix which we will change
  if (tolerance == 0){
    return(cur_confusion_mat)
  }
  row_names = rownames(cur_confusion_matrix)
  col_names = colnames(cur_confusion_matrix)
  for (events in eventlist){
    #print(events)
    dff <- formatData(filepath1, events, file_seconds) #observer 1
    dff2 <- formatData(filepath2, events, file_seconds) #observer 2

    #get rid of the observer category
    dffdropped <- dff[, 1:2]
    dff2dropped <- dff2[, 1:2]
    #before we make a table with the frequencies, we will convert the values within seconds to the same value
    #by utilizing set difference
    valsindffnotdff2 <- dplyr::setdiff(dffdropped, dff2dropped) #all of the values in dff1 which are not in dff2
    #order alphabetically by behavior
    inds <- order(valsindffnotdff2$behavior)
    if (identical(inds, integer(0))){
      #print("no updates needed to be made")
      next
    }
    #print(length(inds))
    #print(nrow(valsindffnotdff2))
    valsindffnotdff2 <- valsindffnotdff2[inds,]
    valsindffnotdff2 <- valsindffnotdff2[which(valsindffnotdff2$behavior != 'data_missing'),]
    diffs <- data.frame(diff(valsindffnotdff2$time)) #length will be - 1 since we are finding difference
    colnames(diffs) = "diff"
    diffs = rbind(c(diff = 0), diffs) #length is same as valsindffnotdff2
    diffs$diff <- round(diffs$diff,2)
    diffs$diff <- diffs$diff * 100
    valsindffnotdff2 = cbind(valsindffnotdff2, diffs)
    rownames(valsindffnotdff2) = c(1:nrow(valsindffnotdff2))

    time_change = data.frame(valsindffnotdff2[which(valsindffnotdff2$diff > 1), ])

    list_of_timechange_dataframes = list()
    for (i in 1:length(events)) {
      inds = which(time_change$behavior == events[i])
      mini_df <- data.frame(time_change[inds,])
      list_of_timechange_dataframes[[i]] = mini_df
    }

    for (i in 1:length(list_of_timechange_dataframes)){
      dflist = list_of_timechange_dataframes[[i]]
      cur_confusion_matrix = add_tolerance_per_event(valsindffnotdff2, dff2, dflist, cur_confusion_matrix, tolerance)
    }
  }
  cur_confusion_matrix = cur_confusion_matrix[1:nrow(cur_confusion_matrix)-1, 1:ncol(cur_confusion_matrix)-1]

  sr <- as.integer(sum(rowSums(cur_confusion_matrix, na.rm=T)) + .000001)
  sc <- as.integer(sum(colSums(cur_confusion_matrix, na.rm=T)) + .000001)

  if (sr != sc) {
    print("sums not equal")
  }
  agree <- sum(diag(cur_confusion_matrix), na.rm=T)
  tot <- sr
  perc_agree <- agree/tot * 100

  cur_confusion_matrix = rbind(cur_confusion_matrix, colSums(cur_confusion_matrix))
  cur_confusion_matrix = cbind(cur_confusion_matrix, rowSums(cur_confusion_matrix))
  rowname <- paste0(dff2$observer[1],"_totals")
  colname <- paste0(dff$observer[1],"_totals")

  rownames(cur_confusion_matrix)[nrow(cur_confusion_matrix)] <- rowname
  colnames(cur_confusion_matrix)[ncol(cur_confusion_matrix)] <- colname
  return(list(confusion_matrix=cur_confusion_matrix,percent_agreement_raw=perc_agree))
}

#' Creating A Reliability Matrix Output File
#'
#' This function takes in the 2 filepaths and a directory for the output excel file to be saved into. The reliability values calculated are all for fixed tolerance values of 0 and 1 for a direct comparison.
#' @param save_dir Directory path to save the excel file into
#' @param filepath1 Filepath to first excel file.
#' @param filepath2 Filepath to second excel file.
#' @param eventlist List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.
#' @param file_seconds The total number of seconds coded for in each file.
#' @return Nothing is explicitly returned from this function, but the excel file is written and saved out into the output directory. The excel file contains all percent agreement and percent by chance values as well as kappa values.
#' @examples
#' createReliabilityExcel(save_dir, filepath1 = './6008BECEU.xlsx', filepath2 = './6008BECNR.xlsx', eventlist = list(c("positive","neutral","negative","CantTellAffect"), c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")), file_seconds = 300)
createReliabilityExcel <- function(save_dir, filename1, filename2, eventlist, file_seconds = 300){
  cur_dir = setwd(save_dir)
  output_excel_name = paste(cur_dir, "all_reliability_output.xlsx", sep = "/")
  wb = createWorkbook()
  tol_mat0 = createToleranceMatrix(filename1, filename2, eventlist, 0, file_seconds = file_seconds)
  tol_mat1 = createToleranceMatrix(filename1, filename2, eventlist, 1, file_seconds = file_seconds)

  kappas <- createToleranceAndKappaValsFromMatrix(tol_mat0$confusion_matrix)
  rownames(kappas) <- ("Without Tolerance")
  kappas2 <- createToleranceAndKappaValsFromMatrix(tol_mat1$confusion_matrix)
  rownames(kappas2) <- (paste("With Tolerance ", 1, sep=""))
  kappas = rbind(kappas, kappas2)
  #write it to an excel file
  sheetNameDir = output_excel_name
  xlsx::write.xlsx(tol_mat0$confusion_matrix, sheetNameDir,
                   sheetName="Tolerance0",
                   col.names = TRUE,showNA=TRUE,append=TRUE)
  xlsx::write.xlsx(tol_mat1$confusion_matrix, sheetNameDir,
                   sheetName="Tolerance1",
                   col.names = TRUE,showNA=TRUE,append=TRUE)
  xlsx::write.xlsx(kappas, sheetNameDir,
                   sheetName="Kappas",
                   col.names = TRUE,showNA=TRUE,append=TRUE)
  #now f_names contains all of the pairwise names that we care about
}

#' Creating A Reliability Matrix Output File
#'
#' This function is the same as the createReliabilityExcel function except that we can now customize the tolerance values with a list of multiple values we initialize as the user. The tolerance values default to 0 and 1 if we don't change the input parameter.
#' @param save_dir Directory path to save the excel file into
#' @param filepath1 Filepath to first excel file.
#' @param filepath2 Filepath to second excel file.
#' @param eventlist List of all states for each sensory signal, each list entry being a vector of the states per sensory signal.
#' @param tolerance List of tolerance values to use for calculation
#' @param file_seconds The total number of seconds coded for in each file.
#' @return Nothing is explicitly returned from this function, but the excel file is written and saved out into the output directory. The excel file contains all percent agreement and percent by chance values as well as kappa values for each of the tolerance values.
#' @examples
#' createReliabilityExcel(save_dir, filepath1 = './6008BECEU.xlsx', filepath2 = './6008BECNR.xlsx', eventlist = list(c("positive","neutral","negative","CantTellAffect"), c("AutonomySupport","Neither","Intrusiveness","CantTellBehavior")), file_seconds = 300)
createReliabilityExcel2 <- function(save_dir, save_as_fname, filename1, filename2, eventlist, tolerance = c(0,1), file_seconds = 300){
  cur_dir = setwd(save_dir)
  output_excel_name = paste(cur_dir, save_as_fname, sep = "/")
  wb = createWorkbook()

  all_tolerance_matrices = vector("list", length(tolerance))
  all_kappas = vector("list", length(tolerance))

  for(i in 1:length(tolerance)){
    tol_mat = createToleranceMatrix(filename1, filename2, eventlist, tolerance[i], file_seconds = file_seconds)
    kappas = createToleranceAndKappaValsFromMatrix(tol_mat$confusion_matrix)
    all_tolerance_matrices[[i]] = tol_mat$confusion_matrix
    all_kappas[[i]] = kappas
  }

  kappas = data.frame(matrix(unlist(all_kappas), nrow = length(tolerance), byrow = TRUE))
  rownames(kappas) = paste("With Tolerance", tolerance)
  colnames(kappas) = c("CohensOmnibusKappa", "PercentAgreement",
                       "PercentByChance", "weightedKappa",
                       "kappaUnweightedStdError", "kappaWeightedStdError")

  #write it to an excel file
  sheetNameDir = output_excel_name
  for (i in 1:length(tolerance)){
    sheetName = paste("Tolerance", tolerance[i], sep = "")
    xlsx::write.xlsx(all_tolerance_matrices[[i]], sheetNameDir,
                     sheetName = sheetName,
                     col.names = TRUE,showNA=TRUE,append=TRUE)
  }
  xlsx::write.xlsx(kappas, sheetNameDir,
                   sheetName="Kappas",
                   col.names = TRUE,showNA=TRUE,append=TRUE)
  #now f_names contains all of the pairwise names that we care about
}
