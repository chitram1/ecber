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

formatDataToleranceNotWorking <- function(filepath, events, tolerance, file_seconds = 600) {
  ff <- read_excel(filepath)
  ff <- comparisonDfcreator(ff)
  ff <- dfSorter(ff)
  #add the additional bounds of tolerance
  ff$Start_Time <- ff$Start_Time - tolerance
  ff$Stop_Time <- ff$Stop_Time + tolerance

  #truncate negative start times to 0
  i <- which(ff$Start_Time < 0)
  ff[i,]$Start_Time <- 0

  #Keep only rows in events
  i <- which(ff$Behavior %in% events)
  f1 <- ff[i,]

  #round the time to decimal places
  f1$Start_Time <- round(f1$Start_Time,2)
  f1$Stop_Time <- round(f1$Stop_Time,2)

  ##Convert to long format (100ths of a second)
  f1$Start_Time <- f1$Start_Time * 100 + 1
  f1$Stop_Time <- f1$Stop_Time * 100
  file_seconds <- file_seconds * 100

  ####File length adjustments
  ##Truncate file if neededc
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
      fixed_out <- out[-bad,] #get rid of those columns, we will work with them later
      return(fixed_out)
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

buildMatrix <- function(filepath1,filepath2,eventlist,file_seconds=600){

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

createToleranceMatrixNotWorking <- function(filepath1, filepath2, eventlist, tolerance, file_seconds = 300) {
  #if (tolerance == 1) {
    #output <- buildMatrix(filepath1, filepath2, eventlist, tolerance, file_seconds)
    #return(output[1])
  #}
  matvals <- c(unlist(eventlist),"data_missing")
  mat <- matrix(data=NA, nrow=length(matvals), ncol=length(matvals))
  colnames(mat) <- matvals
  rownames(mat) <- matvals

  #setup for agree() later
  obs1total <- NULL
  obs2total <- NULL

  for (events in eventlist){
    #1. read and format data using formatData
    obs1 <- formatDataTolerance(filepath1, events, tolerance, file_seconds=file_seconds)
    obs2 <- formatDataTolerance(filepath2, events, tolerance, file_seconds=file_seconds)

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
  #Sum of cols should also = (# rows / 100) * number of events in eventlist
  r1 <- as.integer(sum(colSums(mat, na.rm=T)) + .000001)
  r1back <- as.integer(sum(rowSums(mat, na.rm=T)) + 1)
  r2 <- nrow(obs2)/100 * length(eventlist)

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

createToleranceAndKappaValsFromFiles <- function(filepath1, filepath2, tolerance, file_seconds = 300) {
  output <- createToleranceMatrix(filepath1, filepath2, tolerance, file_seconds)
  output <- output$confusion_matrix
  return(calcKappaValuesFromMatrix(output))
}

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

Boris2NoldusFileReShape <- function(rel,xlsxFileName){
	#input:
	#	rel = BORIS .csv file read in through read.csv()
	#	xlsxFileName = name that BORIS file is saved under after conversion to Noldus
	#				   in .xlsx format
	#output:
	#	converted BORIS to Noldus xlsx file (saved under the current working directory)

	#str(rel)

	#### Tidy the Boris data file  #################################################

	# Noldus names
	# Time_Relative_sf,	Duration_sf,	Observation,	Behavior,	Event_Type
	names(rel)


	rel.2 <- rel %>% select(Observation = Observation.id,
	                        start = Start..s.,
	                        stop = Stop..s.,
	                        Duration_sf = Duration..s.,
	                        Behavioral.category,
	                        Behavior,
	                        Behavior.type)

	#levels(primary$Event_Type) #  "State point" "State start" "State stop"

	# Make the boris (rel) long so that you have state start and state stop rows
	names(rel.2)

	rel.3 <- rel.2 %>% pivot_longer(
	  cols = c("start", "stop"),
	  names_to = "Event_Type_tempName",
	  values_to = "Time_Relative_sf",
	  values_drop_na = FALSE)

	## Clean up the columns

	# These are the names of the factors form the Noldus file that we want
	# "State point" "State start" "State stop"

	# First, change the variable to be a factor
	rel.3$Event_Type_tempName2 <- as.factor(paste0(rel.3$Behavior.type, " ", rel.3$Event_Type_tempName))

	# Remove the "point stop" level because this is redundant with the point start
	rel.4 <- rel.3 %>% filter(Event_Type_tempName2 != "POINT stop")


	# Relabel the factors so that they match the Noldus exactly
	levels(rel.4$Event_Type_tempName2)
	# "POINT start" "STATE start" "STATE stop"
	rel.4$Event_Type <- ifelse(rel.4$Event_Type_tempName2 == "POINT start", "State point",
	                           ifelse(rel.4$Event_Type_tempName2 == "STATE start", "State start",
	                                  ifelse(rel.4$Event_Type_tempName2 == "STATE stop", "State stop", NA)))

	# Remove our temp columns to tidy up the data and re-order the columns so they match
	# Time_Relative_sf	Duration_sf	Observation	Event_Log	Behavior	Event_Type
	rel.5 <- rel.4 %>% select(-Behavior.type, Event_Type_tempName, Event_Type_tempName2) %>%
	  select(Time_Relative_sf, Duration_sf, Observation,	Behavior,	Event_Type,Behavioral.category) %>%
	  mutate(Duration_sf = ifelse(is.na(Duration_sf),0, Duration_sf)) %>%
	  mutate(Duration_sf = ifelse(Event_Type == "State stop",0, Duration_sf))

	##Time relative to 0

	sessionStartTime <- rel.5 %>% filter(Behavioral.category== "Test behavior")
	timeVariable <- min(sessionStartTime$Time_Relative_sf)
	rel.5$Time_Relative_sf <- rel.5$Time_Relative_sf - timeVariable
	rel.5 <- rel.5 %>% select(-Behavioral.category)
	rel.5 <- rel.5[order(rel.5$Time_Relative_sf),]
	rel.6 <- rel.5 %>%  mutate(Time_Relative_sf = ifelse(Time_Relative_sf < 0,0, Time_Relative_sf)) %>%
	  mutate(Time_Relative_sf = ifelse(Time_Relative_sf >= 600,599.999, Time_Relative_sf))

	# print the data to the processed folder

	# setwd(processed_data)
	write.xlsx(rel.6, xlsxFileName, overwrite=T)
}#Boris2NoldusFileReShape

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

#NEW FUNCTIONS I MADE MOST RECENTLY
add_tolerance_per_event <- function(valsindffnotdff2, dff2, timechange_event, cur_confusion_matrix) {
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
      print(i)
      print(cur_end_time)
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
    print(time_diff)
    if (time_diff < 0){
      print("negative here")
      print(cur_row)
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

createToleranceMatrix <- function(filepath1,filepath2,eventlist,tolerance,file_seconds=300) {
  cur_confusion_mat = buildMatrix(filepath1, filepath2, eventlist, file_seconds)
  cur_confusion_matrix = cur_confusion_mat$confusion_matrix #original matrix which we will change
  if (tolerance == 0){
    return(cur_confusion_mat)
  }
  row_names = rownames(cur_confusion_matrix)
  col_names = colnames(cur_confusion_matrix)
  for (events in eventlist){
    dff <- formatData(filepath1, events, file_seconds) #observer 1
    dff2 <- formatData(filepath2, events, file_seconds) #observer 2

    #get rid of the observer category
    dffdropped <- dff[, 1:2]
    dff2dropped <- dff2[, 1:2]
    #before we make a table with the frequencies, we will convert the values within seconds to the same value
    #by utilizing set difference
    valsindffnotdff2 <- setdiff(dffdropped, dff2dropped) #all of the values in dff1 which are not in dff2
    #order alphabetically by behavior
    indices <- order(valsindffnotdff2$behavior)
    print(length(indices))
    print(nrow(valsindffnotdff2))
    valsindffnotdff2 <- valsindffnotdff2[indices,]
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
      cur_confusion_matrix = add_tolerance_per_event(valsindffnotdff2, dff2, dflist, cur_confusion_matrix)
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
