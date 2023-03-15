# the following code requires the package(s):
#these packages may need to be installed depending on whether or not the user has them already
library(openxlsx)
library(readxl)    #opening xlsx
library(gridExtra) #for creating graphics of tables
library(tidyverse)
library(DescTools) #for using Overlap()
library(dplyr)
library(tidyr)
library(devtools)
library(roxygen2)


#' Combined entropy output
#'
#' @param file_name excel file to read in
#' @param tactile_padding default tactile padding
#' @param auditory_padding default auditory padding
#' @param TAV_behavior_types list of all the behavior types
#' @param affect_behavior_types list of affect behavior types
#' @param autonomy_behavior_types list of autonomy behavior types
#' @param missing_threshold default missing threshold value
#' @return dataframe of all of the entropy outputs for different subsets of the data
combined_entropy_output <- function(file_name,
                                    tactile_padding = 1.0,
                                    auditory_padding = 1.0,
                                    TAV_behavior_types = list("mom_auditory_types" = c('Vocal'),
                                                              "mom_tactile_types" = c('TouchBaby',
                                                                                      'HoldingBaby'),
                                                              "mom_visual_types" = c('ManipulatingObject'),
                                                              "baby_visual_types" = c('LookAtMomActivity'),
                                                              "missing_types" = c('CantTellHolding',
                                                                                  'ActivityNotVisible',
                                                                                  'CantTellLooking')),
                                    affect_behavior_types = list("mom_auditory_types" = c('Vocal'),
                                                                 "mom_tactile_types" = c('TouchBaby',
                                                                                         'HoldingBaby'),
                                                                 "mom_visual_types" = c('ManipulatingObject'),
                                                                 "baby_visual_types" = c('LookAtMomActivity'),
                                                                 "positive" = c('positive'),
                                                                 "negative" = c('negative'),
                                                                 "neutral" = c('neutral'),
                                                                 "missing_types" = c('CantTellHolding',
                                                                                     'ActivityNotVisible',
                                                                                     'CantTellLooking',
                                                                                     'CantTellAffect')),
                                    autonomy_behavior_types = list("positive" = c('positive'),
                                                                   "negative" = c('negative'),
                                                                   "neutral" = c('neutral'),
                                                                   "autonomy_support" = c('AutonomySupport'),
                                                                   "intrusiveness" = c('Intrusiveness'),
                                                                   "neither" = c('Neither'),
                                                                   "missing_types" = c('CantTellAffect', 'CantTellBehavior')),
                                    missing_threshold = 0.1){
  fcheck_fail = FALSE
  list_TAV = ber_analyze_file(file_name, missing_threshold = missing_threshold)
  list_affect = ber_analyze_file_affect(file_name, missing_threshold = missing_threshold)
  list_affect_autonomy = ber_analyze_file_affect_and_autonomy(file_name, missing_threshold = missing_threshold)
  fchecks_TAV = list_TAV$file_checks
  fchecks_affect = list_affect$file_checks
  fchecks_aff_aut = list_affect_autonomy$file_checks
  sub_ID = list_TAV$estimates$SubjectID
  max_percent_missing = c(list_TAV$estimates$PercentMissing, list_affect$estimates$PercentMissing, list_affect_autonomy$estimates$PercentMissing)
  
  if ((sum(unlist(fchecks_TAV)) != 6) || (sum(unlist(fchecks_affect)) != 6)
      || (sum(unlist(fchecks_aff_aut)) != 6)) {
    fcheck_fail = TRUE
  }
  
  resultsInDF <- data.frame('SubjectID'=sub_ID,
                            'EntropyRateTAV'=list_TAV$estimates$EntropyRate,
                            'EntropyRateTAV_Affect'=list_affect$estimates$EntropyRate,
                            'EntropyRateAutonomy_Affect'=list_affect_autonomy$estimates$EntropyRate,
                            'TotalNumberOfTransitionsTAV'=list_TAV$estimates$TotalNumberOfTransitions,
                            'TotalNumberOfTransitionsTAV_Affect'=list_affect$estimates$TotalNumberOfTransitions,
                            'TotalNumberOfTransitionsAutonomy_Affect'=list_affect_autonomy$estimates$TotalNumberOfTransitions,
                            'MaxPercentMissing'= max(max_percent_missing),
                            stringsAsFactors = F)
  return(list(estimates = resultsInDF, file_checks = fcheck_fail))
}

combined_entropy_output_more_detail <- function(file_name,
                                    tactile_padding = 1.0,
                                    auditory_padding = 1.0,
                                    TAV_behavior_types = list("mom_auditory_types" = c('Vocal'),
                                                              "mom_tactile_types" = c('TouchBaby',
                                                                                      'HoldingBaby'),
                                                              "mom_visual_types" = c('ManipulatingObject'),
                                                              "baby_visual_types" = c('LookAtMomActivity'),
                                                              "missing_types" = c('CantTellHolding',
                                                                                  'ActivityNotVisible',
                                                                                  'CantTellLooking')),
                                    affect_behavior_types = list("mom_auditory_types" = c('Vocal'),
                                                                 "mom_tactile_types" = c('TouchBaby',
                                                                                         'HoldingBaby'),
                                                                 "mom_visual_types" = c('ManipulatingObject'),
                                                                 "baby_visual_types" = c('LookAtMomActivity'),
                                                                 "positive" = c('positive'),
                                                                 "negative" = c('negative'),
                                                                 "neutral" = c('neutral'),
                                                                 "missing_types" = c('CantTellHolding',
                                                                                     'ActivityNotVisible',
                                                                                     'CantTellLooking',
                                                                                     'CantTellAffect')),
                                    autonomy_behavior_types = list("positive" = c('positive'),
                                                                   "negative" = c('negative'),
                                                                   "neutral" = c('neutral'),
                                                                   "autonomy_support" = c('AutonomySupport'),
                                                                   "intrusiveness" = c('Intrusiveness'),
                                                                   "neither" = c('Neither'),
                                                                   "missing_types" = c('CantTellAffect', 'CantTellBehavior')),
                                    missing_threshold = 0.1){
  fcheck_fail = FALSE
  list_TAV = ber_analyze_file(file_name, missing_threshold = missing_threshold)
  list_affect = ber_analyze_file_affect(file_name, missing_threshold = missing_threshold)
  list_affect_autonomy = ber_analyze_file_affect_and_autonomy(file_name, missing_threshold = missing_threshold)
  fchecks_TAV = list_TAV$file_checks
  fchecks_affect = list_affect$file_checks
  fchecks_aff_aut = list_affect_autonomy$file_checks
  sub_ID = list_TAV$estimates$SubjectID
  max_percent_missing = c(list_TAV$estimates$PercentMissing, list_affect$estimates$PercentMissing, list_affect_autonomy$estimates$PercentMissing)
  
  if ((sum(unlist(fchecks_TAV)) != 6) || (sum(unlist(fchecks_affect)) != 6)
      || (sum(unlist(fchecks_aff_aut)) != 6)) {
    fcheck_fail = TRUE
  }
  
  resultsInDF <- data.frame('SubjectID'=sub_ID,
                            'EntropyRateTAV'=list_TAV$estimates$EntropyRate,
                            'TAVPercentMissing'=list_TAV$estimates$PercentMissing,
                            'EntropyRateTAV_Affect'=list_affect$estimates$EntropyRate,
                            'TAV_AffectPercentMissing'=list_affect$estimates$PercentMissing,
                            'EntropyRateAutonomy_Affect'=list_affect_autonomy$estimates$EntropyRate,
                            'Autonomy_AffectPercentMissing'=list_affect_autonomy$estimates$PercentMissing,
                            'TotalNumberOfTransitionsTAV'=list_TAV$estimates$TotalNumberOfTransitions,
                            'TotalNumberOfTransitionsTAV_Affect'=list_affect$estimates$TotalNumberOfTransitions,
                            'TotalNumberOfTransitionsAutonomy_Affect'=list_affect_autonomy$estimates$TotalNumberOfTransitions,
                            'MaxPercentMissing'= max(max_percent_missing),
                            stringsAsFactors = F)
  return(list(estimates = resultsInDF, file_checks = fcheck_fail))
}

combined_entropy_output_more_detail_dir <- function(dir_loc, log_file = paste(Sys.Date(), '-ber-logfile.txt', sep=''), missing_threshold = 0.1){
  old_dir <- getwd()
  setwd(path.expand(dir_loc))
  
  # Reading in data
  all_files = list.files('.', pattern="*.xlsx")
  n_files = length(all_files)
  
  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'= character(),
                          'EntropyRateTAV'= double(),
                          'TAVPercentMissing'=double(),
                          'EntropyRateTAV_Affect'=double(),
                          'TAV_AffectPercentMissing'=double(),
                          'EntropyRateAutonomy_Affect'=double(),
                          'Autonomy_AffectPercentMissing'=double(),
                          'TotalNumberOfTransitionsTAV'=double(),
                          'TotalNumberOfTransitionsTAV_Affect'=double(),
                          'TotalNumberOfTransitionsAutonomy_Affect'=double(),
                          'MaxPercentMissing'= double(),
                          stringsAsFactors = F)
  
  run_start <- Sys.time()
  run_count <- 1
  files_to_check <- c()
  for (i in 1:n_files){
    individual_file <- all_files[i]
    capture.output(file_results <- combined_entropy_output_more_detail(individual_file, missing_threshold = missing_threshold), file=NULL)
    if(file_results$file_checks){
      files_to_check <- c(files_to_check, tail(strsplit(individual_file, '/')[[1]], 1))
      capture.output(file_results <- combined_entropy_output_more_detail(individual_file, missing_threshold = missing_threshold),
                     file = log_file, append = T)
      resultsDF[i,] <- file_results$estimates
      message(paste('Warning - See log for file : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    } else{
      resultsDF[i,] <- file_results$estimates
      message(paste('Completed without issue    : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    }
    run_count = run_count + 1
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))
  
  message(paste(c(rep('-',25), ' Check the log for files below ', rep('-',25)), sep=''))
  
  for(i in 1:length(files_to_check)){
    message(files_to_check[i])
  }
  # Resetting the old directory pointer
  setwd(old_dir)
  
  return(resultsDF)
}

#'This function goes through an entire directory and finds the combined entropy output for each file
combined_entropy_output_dir <- function(dir_loc, log_file = paste(Sys.Date(), '-ber-logfile.txt', sep=''), missing_threshold = 0.1){
  old_dir <- getwd()
  setwd(path.expand(dir_loc))
  
  # Reading in data
  all_files = list.files('.', pattern="*.xlsx")
  n_files = length(all_files)
  
  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'=character(),
                          'EntropyRateTAV'=double(),
                          'EntropyRateTAV_Affect'=double(),
                          'EntropyRateAutonomy_Affect'=double(),
                          'TotalNumberOfTransitionsTAV'=double(),
                          'TotalNumberOfTransitionsTAV_Affect'=double(),
                          'TotalNumberOfTransitionsAutonomy_Affect'=double(),
                          'MaxPercentMissing'=double(),
                          stringsAsFactors = F)
  
  run_start <- Sys.time()
  run_count <- 1
  files_to_check <- c()
  for (i in 1:n_files){
    individual_file <- all_files[i]
    capture.output(file_results <- combined_entropy_output(individual_file, missing_threshold = missing_threshold), file=NULL)
    if(file_results$file_checks){
      files_to_check <- c(files_to_check, tail(strsplit(individual_file, '/')[[1]], 1))
      capture.output(file_results <- combined_entropy_output(individual_file, missing_threshold = missing_threshold),
                     file = log_file, append = T)
      resultsDF[i,] <- file_results$estimates
      message(paste('Warning - See log for file : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    } else{
      resultsDF[i,] <- file_results$estimates
      message(paste('Completed without issue    : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    }
    run_count = run_count + 1
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))
  
  message(paste(c(rep('-',25), ' Check the log for files below ', rep('-',25)), sep=''))
  
  for(i in 1:length(files_to_check)){
    message(files_to_check[i])
  }
  # Resetting the old directory pointer
  setwd(old_dir)
  
  return(resultsDF)
}



# ------ ORIGINAL FUNCTION WITHOUT ANY ADDED CHANGE (only based on auditory, tactile, visual) ----------

#' Estimate Behavioral Entropy Rate based upon Video Data Files for a Directory
#'
#' @param dir_loc directory of files
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_dir <- function(dir_loc,
                            tactile_padding = 1.0,
                            auditory_padding = 1.0,
                            behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                "mom_tactile_types" = c('TouchBaby',
                                                                        'HoldingBaby'),
                                                "mom_visual_types" = c('ManipulatingObject'),
                                                "baby_visual_types" = c('LookAtMomActivity'),
                                                "missing_types" = c('CantTellHolding',
                                                                    'ActivityNotVisible',
                                                                    'CantTellLooking')),
                            missing_threshold = 0.1,
                            log_file = paste(Sys.Date(), '-ber-logfile.txt', sep='')){
  
  old_dir <- getwd()
  setwd(path.expand(dir_loc))
  
  # Reading in data
  all_files = list.files('.', pattern="*.xlsx")
  n_files = length(all_files)
  
  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'=character(),
                          'CanEstimateEntropy'=logical(),
                          'EntropyRate'=double(),
                          'TotalNumberOfTransitions'=double(),
                          'CombinedVideoDuration'=double(),
                          'PercentMissing'=double(),
                          'AuditoryCounts'=double(),
                          'AuditoryTotalTime'=double(),
                          'AuditoryAverageTime'=double(),
                          'VisualCounts'=double(),
                          'VisualTotalTime'=double(),
                          'VisualAverageTime'=double(),
                          'TactileCounts'=double(),
                          'TactileTotalTime'=double(),
                          'TactileAverageTime'=double(),
                          stringsAsFactors = F)
  
  all_vars_named = c('AllStates', 'VisualTactile', 'AuditoryVisual', 'AuditoryTactile',
                     'AuditoryOnly', 'TactileOnly', 'VisualOnly', 'NoState')
  for (index in 1:8) {
    cur_var_name = all_vars_named[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    resultsDF[cur_var_name_c] = double()
    resultsDF[cur_var_name_t] = double()
    resultsDF[cur_var_name_a] = double()
  }
  
  
  # Analysis Section -----------------------------------------------------------
  
  run_start <- Sys.time()
  run_count <- 1
  files_to_check <- c()
  for (i in 1:n_files){
    individual_file <- all_files[i]
    capture.output(file_results <- ber_analyze_file(individual_file,
                                                    tactile_padding=tactile_padding,
                                                    auditory_padding=auditory_padding,
                                                    behavior_types=behavior_types,
                                                    missing_threshold=missing_threshold), file=NULL)
    
    if(sum(unlist(file_results$file_checks)) != 6){
      files_to_check <- c(files_to_check, tail(strsplit(individual_file, '/')[[1]], 1))
      capture.output(file_results <- ber_analyze_file(individual_file,
                                                      tactile_padding=tactile_padding,
                                                      auditory_padding=auditory_padding,
                                                      behavior_types=behavior_types,
                                                      missing_threshold=missing_threshold),
                     file = log_file, append = T)
      resultsDF[i,] <- file_results$estimates
      message(paste('Warning - See log for file : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    } else{
      resultsDF[i,] <- file_results$estimates
      message(paste('Completed without issue    : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    }
    run_count = run_count + 1
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))
  
  message(paste(c(rep('-',25), ' Check the log for files below ', rep('-',25)), sep=''))
  
  for(i in 1:length(files_to_check)){
    message(files_to_check[i])
  }
  
  # Resetting the old directory pointer
  setwd(old_dir)
  
  return(resultsDF)
}

#' Estimate Behavioral Entropy Rate based upon Video Data
#'
#' @param f_loc file location
#' @param plot_all logical: Plot the data to observe the sequence of behaviors
#' @param plots_to_file logical: send all plots to a file
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_file <- function(f_loc,
                             plot_all=F,
                             plots_to_file=F,
                             tactile_padding = 1.0,
                             auditory_padding = 1.0,
                             behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                 "mom_tactile_types" = c('TouchBaby',
                                                                         'HoldingBaby'),
                                                 "mom_visual_types" = c('ManipulatingObject'),
                                                 "baby_visual_types" = c('LookAtMomActivity'),
                                                 "missing_types" = c('CantTellHolding',
                                                                     'ActivityNotVisible',
                                                                     'CantTellLooking')),
                             missing_threshold = 0.1){
  
  file_checks <- list(header_pass = T,
                      subjid_pass = T,
                      misdat_pass = T,
                      blabel_pass = T,
                      elabel_pass = T,
                      misnes_pass = T)
  
  
  # Unpacking input behavior types ---------------------------------------------
  mom_auditory_types <- behavior_types$mom_auditory_types
  mom_tactile_types <- behavior_types$mom_tactile_types
  mom_visual_types <- behavior_types$mom_visual_types
  baby_visual_types <- behavior_types$baby_visual_types
  missing_types <- behavior_types$missing_types
  
  # extracting data from file using the readxl package
  behavior_data <- data.frame(readxl::read_xlsx(f_loc))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste('Filename:        ', tail(strsplit(f_loc, '/')[[1]], 1), '\n'))
  cat(paste('Time of Analysis:', Sys.time(), '\n'))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  
  cat(paste(c(rep('*', 27), ' Performing File Check ',rep('*', 27), '\n'), collapse = ''))
  cat(paste('- Checking for required Columns:\n'))
  
  cat(paste('\tObservation      : First cell used to set "SubjectID"\n'))
  cat(paste('\tBehavior         : Set of used behavior labels\n'))
  cat(paste('\tTime_Relative_sf : Sets the start point for each action\n'))
  cat(paste('\tDuration_sf      : Time_Relative_sf + Duration_sf sets end points\n'))
  cat(paste('\tEvent_Type       : Defines point events and states\n'))
  
  columns_needed <- c('Observation', 'Behavior', 'Time_Relative_sf', 'Duration_sf', 'Event_Type')
  columns_found <- colnames(behavior_data)
  
  all_vars_named = c('AllStates', 'VisualTactile', 'AuditoryVisual', 'AuditoryTactile',
                     'AuditoryOnly', 'TactileOnly', 'VisualOnly', 'NoState')
  
  
  if(sum(!(columns_needed %in% columns_found)) > 0){
    file_checks$header_pass <- F
    cat(paste('--- FAILED : Missing Column Headers:\n'))
    cat('\t')
    cat(paste(columns_needed[!(columns_needed %in% columns_found)], collapse = '\n\t'))
    cat('\n')
    cat(paste(c(rep('*', 9), ' IF FILE FORMATS HAVE CHANGED, THIS SCRIPT MUST BE UPDATED ',rep('*', 9), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    data2return <- data.frame('SubjectID'=NA,
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:8) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Found all Required Column Headers\n'))
  }
  
  # Mother ID should be in first cell in the observation column
  
  cat(paste('- Checking "Observation" Column For Subject ID\n'))
  id_number <- behavior_data$Observation[1]
  if(is.na(id_number) == T){
    file_checks$subjid_pass <- F
    cat(paste('--- FAILED: Data is "NA" in Column J, Cell 1\n'))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(NULL)
  } else{
    cat(paste('--- PASSED: Using Subject ID from Column J, Cell 1:', id_number, '\n'))
  }
  
  
  cat(paste('- Checking for Missing Data in Columns\n'))
  if(sum(is.na(behavior_data$Behavior)) > 0 |
     sum(is.na(behavior_data$Time_Relative_sf)) > 0 |
     sum(is.na(behavior_data$Duration_sf)) > 0 |
     sum(is.na(behavior_data$Event_Type)) > 0){
    
    file_checks$header_pass <- F
    
    if(sum(is.na(behavior_data$Behavior)) > 0){
      cat(paste('--- "Behavior"         : FAILED - Check Cells:', paste(which(is.na(behavior_data$Behavior))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Behavior"         : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Time_Relative_sf)) > 0){
      cat(paste('--- "Time_Relative_sf" : FAILED - Check Cells:', paste(which(is.na(behavior_data$Time_Relative_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Duration_sf)) > 0){
      cat(paste('--- "Duration_sf"      : FAILED - Check Cells:', paste(which(is.na(behavior_data$Duration_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Duration_sf"      : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Event_Type)) > 0){
      cat(paste('--- "Event_Type"       : FAILED - Check Cells:', paste(which(is.na(behavior_data$Event_Type))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Event_Type"       : PASSED\n'))
    }
    
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:8) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- "Behavior"         : PASSED\n'))
    cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    cat(paste('--- "Duration_sf"      : PASSED\n'))
    cat(paste('--- "Event_Type"       : PASSED\n'))
  }
  
  cat(paste('- Checking "Behavior" Column For Unused Labels:\n'))
  labels_used <- unique(behavior_data$Behavior)
  if(sum(!(labels_used %in% as.vector(unlist(behavior_types)))) > 0){
    cat(paste('--- WARNING : Unused Labels in "Behavior" Column, See Below:\n'))
    unused_labels <- labels_used[!(labels_used %in% as.vector(unlist(behavior_types)))]
    for(ul in unused_labels){
      if(ul %in% c('NotHoldingBaby', 'NotLookAtMomActivity', 'NoObjectInHand')){
        cat(paste('\tExpected Label  : "', ul, '", not used in analysis', '\n', sep=''))
      } else {
        file_checks$blabel_pass <- F
        cat(paste('\tUnexpected Label: "', ul, '", Cells: ', paste(which(behavior_data$Behavior == ul)+1, collapse = ','), '\n', sep=''))
      }
    }
    cat(paste('--- NOTE: Investigate this if these do not look familar\n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Behavior" Column\n'))
  }
  
  cat(paste('- Checking "Event_Type" Column For Labels:\n'))
  e_type_labels <- unique(behavior_data$Event_Type)
  e_type_expected <- c('State start', 'State point', 'Point', 'State stop', 'State Stop')
  if(sum(!(e_type_labels %in% e_type_expected)) > 0){
    cat(paste('--- WARNING : Unused Labels in "Event_Type" Column, See Below:\n'))
    unused_labels <- e_type_labels[!(e_type_labels %in% e_type_expected)]
    for(ul in unused_labels){
      file_checks$elabel_pass <- F
      cat(paste('\tLabel: "', ul, '", Cells: ', paste(which(behavior_data$Event_Type == ul)+1, collapse = ','), '\n', sep=''))
    }
    cat(paste('--- NOTE: This may require a fix to the function ".subset_by_types()" \n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Event_Type" Column\n'))
  }
  
  # Identifying the last time between both the mother and baby files
  lasttime = max(behavior_data$Time_Relative_sf)
  lastduration = max(subset(behavior_data,
                            behavior_data$Time_Relative_sf==max(behavior_data$Time_Relative_sf))$Duration_sf)
  endtime = lasttime + lastduration
  ##############################################################################
  # Finding the total amount of missing time
  cat(paste('- Checking Missingness based on "missing_types"\n'))
  missing <- .subset_by_types(behavior_data,
                              missing_types)
  
  if (nrow(missing) <= 0){
    message(paste('Warning'))
  }
  
  #all of the missing types
  #we need to get rid of overlaps here
  #1. first we will order in increasing order
  missing <- missing[order(missing$Time_Relative_sf,decreasing = FALSE),]
  missing$Duration_sf = missing$Time_Relative_sf + missing$Duration_sf #convert to get relative end time
  #2. we will start at the top of the missing list
  total_sum_missing = 0
  cur_start_time = 0
  if (nrow(missing) > 0) {
    cur_start_time = missing[1,1]
    for (i in 1:(nrow(missing)-1)) {
      cur_end_time = missing[i,2]
      dur = cur_end_time - cur_start_time
      total_sum_missing = total_sum_missing + dur
      next_start_time = missing[i+1,1]
      next_end_time = missing[i+1,2]
      if (next_start_time > cur_end_time){
        cur_start_time = next_start_time
      } else if(cur_end_time < next_end_time) {
        cur_start_time = cur_end_time
      } else {
        cur_start_time = cur_end_time
        next
      }
    }
    total_sum_missing = total_sum_missing + (missing[i+1,2] - cur_start_time)
  } 
  percent_missing <- total_sum_missing/endtime
  cat(paste('--- Percent Missingness:', round(percent_missing,3), '\n'))
  if(percent_missing>=missing_threshold){
    file_checks$misnes_pass <- F
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=endtime,
                              'PercentMissing'=percent_missing,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:8) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste('--- FAILED: Missingness greater than threshold', missing_threshold, '\n'))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Percent missing less than threshold\n'))
  }
  # ##############################################################################
  #
  # ##############################################################################
  # Finding all of the unique events for a given state type
  baby_vis <- .subset_by_types(behavior_data, baby_visual_types)
  mom_vis <- .subset_by_types(behavior_data, mom_visual_types)
  mom_tac <- .subset_by_types(behavior_data, mom_tactile_types, tactile_padding)
  mom_aud <- .subset_by_types(behavior_data, mom_auditory_types, auditory_padding)
  #
  # Reducing these events, either by intersection or union of events
  whole_interval <- intervals::Intervals(c(0,endtime))
  tac_states <- .find_unions(mom_tac, "TACTILE")
  aud_states <- .find_unions(mom_aud, "AUDITORY")
  vis_states <- .compare_intersection(mom_vis, baby_vis, "VISUAL")
  nottac_states <- .state_complement(whole_interval, tac_states, "NOT TACTILE")
  notaud_states <- .state_complement(whole_interval, aud_states, "NOT AUDITORY")
  notvis_states <- .state_complement(whole_interval, vis_states, "NOT VISUAL")
  #
  # Finding each state category i.e. 'L-T-!V' means Looking, touching, but not speaking
  vis_tac_aud <- .find_macrostate(vis_states, tac_states, aud_states, "V-T-A", 8)
  vis_tac_notaud <- .find_macrostate(vis_states, tac_states, notaud_states, "V-T-!A",7)
  vis_nottac_aud <- .find_macrostate(vis_states, nottac_states, aud_states, "V-!T-A",6)
  notvis_tac_aud <- .find_macrostate(notvis_states, tac_states, aud_states, "!V-T-A",5)
  notvis_nottac_aud <- .find_macrostate(notvis_states, nottac_states, aud_states, "!V-!T-A",4)
  notvis_tac_notaud <- .find_macrostate(notvis_states, tac_states, notaud_states, "!V-T-!A",3)
  vis_nottac_notaud <- .find_macrostate(vis_states, nottac_states, notaud_states, "V-!T-!A",2)
  notvis_nottac_notaud <- .find_macrostate(notvis_states, nottac_states, notaud_states, "!V-!T-!A",1)
  alltypes <- rbind(vis_tac_aud,
                    vis_tac_notaud,
                    vis_nottac_aud,
                    notvis_tac_aud,
                    notvis_nottac_aud,
                    notvis_tac_notaud,
                    vis_nottac_notaud,
                    notvis_nottac_notaud)
  state_sequence <- alltypes[with(alltypes, order(Start)), ]
  # ##############################################################################
  #
  # ##############################################################################
  # # Entropy Calculations
  transition_counts <- CalcTransitionCounts(state_sequence[,5], 8)
  transition_matrix <- CalcTransitionMatrix(transition_counts)
  stationary_matrix <- CalcEmpiricalStationary(state_sequence[,5], 1:8)
  entropy_rate <- CalcMarkovEntropyRate(transition_matrix, stationary_matrix)
  
  numbered_states <- c()
  for(i in 1:nrow(state_sequence)){
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$Start, state_sequence[i,]$point))
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$End, state_sequence[i,]$point))
  }
  
  if(plot_all == T){
    if (plots_to_file == T){
      mainDir <- path.expand(getwd())
      subDir <- "TAVPlots"
      if (file.exists(subDir)){
        setwd(file.path(mainDir, subDir))
      } else {
        dir.create(file.path(mainDir, subDir))
        setwd(file.path(mainDir, subDir))
      }
      pdf(paste(as.character(id_number), "_all_plots_orig.pdf", sep=""))
      plot_file(endtime=endtime,
                aud_states=aud_states,
                tac_states=tac_states,
                vis_states=vis_states,
                numbered_states=numbered_states,
                id_number=id_number)
      
      plot_orig(endtime=endtime,
                aud_states=aud_states,
                tac_states=tac_states,
                vis_states=vis_states,
                numbered_states=numbered_states,
                id_number=id_number)
      
      plot_transformed(endtime=endtime,
                       alltypes=alltypes,
                       id_number=id_number)
      
      plot_sequence(state_sequence=state_sequence,
                    id_number=id_number)
      
      plot_counts(transition_counts, id_number)
      
      plot_transitions(transition_matrix, id_number)
      dev.off()
      setwd('../')
    }
    if (plot_all == T) {
      plot_file(endtime=endtime,
                aud_states=aud_states,
                tac_states=tac_states,
                vis_states=vis_states,
                numbered_states=numbered_states,
                id_number=id_number)
      
      plot_orig(endtime=endtime,
                aud_states=aud_states,
                tac_states=tac_states,
                vis_states=vis_states,
                numbered_states=numbered_states,
                id_number=id_number)
      
      plot_transformed(endtime=endtime,
                       alltypes=alltypes,
                       id_number=id_number)
      
      plot_sequence(state_sequence=state_sequence,
                    id_number=id_number)
      
      plot_counts(transition_counts, id_number)
      
      plot_transitions(transition_matrix, id_number)
    }
  }
  
  
  
  data2return <- data.frame('SubjectID'=as.character(id_number),
                            'CanEstimateEntropy'=TRUE,
                            'EntropyRate'=entropy_rate,
                            'TotalNumberOfTransitions'=sum(transition_counts),
                            'CombinedVideoDuration'=endtime,
                            'PercentMissing'=percent_missing,
                            'AuditoryCounts'=nrow(aud_states),
                            'AuditoryTotalTime'=sum(aud_states[,3]),
                            'AuditoryAverageTime'=mean(aud_states[,3]),
                            'VisualCounts'=nrow(vis_states),
                            'VisualTotalTime'=sum(vis_states[,3]),
                            'VisualAverageTime'=mean(vis_states[,3]),
                            'TactileCounts'=nrow(tac_states),
                            'TactileTotalTime'=sum(tac_states[,3]),
                            'TactileAverageTime'=mean(tac_states[,3]),
                            stringsAsFactors = F)
  
  all_vars_lengths <- c(nrow(vis_tac_aud), nrow(vis_tac_notaud), nrow(vis_nottac_aud), nrow(notvis_tac_aud),
                        nrow(notvis_nottac_aud), nrow(notvis_tac_notaud), nrow(vis_nottac_notaud), nrow(notvis_nottac_notaud))
  
  all_vars_sums <- c(sum(vis_tac_aud$Duration), sum(vis_tac_notaud$Duration), sum(vis_nottac_aud$Duration), sum(notvis_tac_aud$Duration),
                     sum(notvis_nottac_aud$Duration), sum(notvis_tac_notaud$Duration), sum(vis_nottac_notaud$Duration), sum(notvis_nottac_notaud$Duration))
  
  for (index in 1:8) {
    cur_var_name = all_vars_named[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    len = all_vars_lengths[index]
    sum_all = all_vars_sums[index]
    data2return[cur_var_name_c] = len
    data2return[cur_var_name_t] = sum_all
    if (len == 0) {
      data2return[cur_var_name_a] = 0
    } else {
      data2return[cur_var_name_a] = sum_all / len
    }
  }
  
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste(c(rep('*', 24), ' File Completed Successfully ',rep('*', 24), '\n'), collapse = ''))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  return(list(estimates=data2return, file_checks=file_checks, entropy=entropy_rate))
}


# PLOTTING Functions for ORIGINAL unchanged function -------------------------------

plot_file <- function(endtime=endtime,
                      aud_states=aud_states,
                      tac_states=tac_states,
                      vis_states=vis_states,
                      numbered_states=numbered_states,
                      id_number=id_number){
  par(mar=c(3,8,3,8))
  plot(0, type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,6), ylab="", xlab="")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,1,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,1,0,.5))
  }
  lines(numbered_states[,1], 3*(numbered_states[,2]-1)/7)
  axis(1, at=seq(0,endtime,50), las=2)
  axis(2,at=c(1.5, 3.5, 4.5, 5.5), labels=c("State Transition","Visual", "Tactile", "Auditory"), las=2)
  axis(4, at = c(0,3*seq(1,7)/7), labels=c('No State', 'Visual Only', 'Tactile Only', 'Auditory Only',
                                           'Tactile-Auditory', 'Visual-Auditory', 'Visual-Tactile', 'All Three'),
       las=2)
  abline(h=c(0,3,4,5,6))
  abline(h=3*seq(1,7)/7, lty=3, col=rgb(0,0,0,0.2))
  abline(h=3*c(0.5, 3.5, 6.5)/7, lty=2, col=rgb(0,0,0.75,0.6))
  title(paste('File Identifier: ', id_number)) #, ', Entropy: ', round(entropy_rate,4)
}


plot_orig <- function(endtime=endtime,
                      aud_states=aud_states,
                      tac_states=tac_states,
                      vis_states=vis_states,
                      numbered_states=numbered_states,
                      id_number=id_number){
  par(mar=c(4,5,4,2)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,3), ylab="", xlab="Time")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 2
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 0
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=c(0.5, 1.5, 2.5), labels=c("Visual", "Tactile", "Auditory"), las=2)
  abline(h=c(0,1,2, 3))
  title(paste('File Identifier: ', id_number))
}

plot_transformed <- function(endtime=endtime,
                             alltypes=alltypes,
                             id_number=id_number){
  par(mar=c(4,8,4,2)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,8), ylab="", xlab="Time")
  for(i in 1:nrow(alltypes)){
    starttime <- alltypes[i,]$Start
    duration <- alltypes[i,]$Duration
    vert <- alltypes[i,]$point - 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert),
            col = rgb(0,0,0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=seq(0.5, 7.5, 1), labels=c("No State",
                                       "Visual",
                                       "Tactile",
                                       "Auditory",
                                       "Auditory/Tactile",
                                       "Auditory/Visual",
                                       "Visual/Tactile",
                                       'All States'), las=2)
  abline(h=0:8)
  title(paste('File Identifier: ', id_number))
}

plot_sequence <- function(state_sequence=state_sequence,
                          id_number=id_number){
  par(mar=c(4,8,4,2)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,nrow(state_sequence)), ylim=c(0,8), ylab="", xlab="Time Index")
  lines(0:(nrow(state_sequence)-1), state_sequence$point-0.5, lty=3, col=rgb(0,0,0,0.5))
  points(0:(nrow(state_sequence)-1), state_sequence$point-0.5, pch=19, col=rgb(0,0,0,0.5))
  abline(h=0:8)
  axis(1, at=seq(0,nrow(state_sequence),50), las=1)
  axis(2,at=seq(0.5, 7.5, 1), labels=c("No State",
                                       "Visual",
                                       "Tactile",
                                       "Auditory",
                                       "Auditory/Tactile",
                                       "Auditory/Visual",
                                       "Visual/Tactile",
                                       'All States'), las=2)
  title(paste('File Identifier: ', id_number))
}

plot_counts <- function(transition_counts, id_number){
  type_labs <- c('NS', 'V', 'T', 'A', 'A/T', 'A/V', 'V/T', 'A/T/V')
  par(mar =c(4,5,4,3)+0.1)
  plot(0, xlim=c(0, 8), ylim=c(0,8),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Counts, ID:', id_number))
  abline(h=0:8)
  axis(2, at=seq(0.5, 7.5, 1), labels=type_labs[8:1], las=1)
  axis(1, at=seq(0.5, 7.5, 1), labels=type_labs[1:8], las=1)
  axis(4, at=seq(0.5, 7.5, 1)[8:1], labels=rowSums(transition_counts), las=1)
  abline(v=0:8)
  exes <- seq(0.5, 7.5, 1)
  whys <- seq(7.5, 0.5, -1)
  for(j in 1:8){
    for(i in 1:8){
      text(exes[i], whys[j], labels = transition_counts[j, i])
    }
  }
}

plot_transitions <- function(transition_matrix, id_number){
  type_labs <- c('NS', 'V', 'T', 'A', 'A/T', 'A/V', 'V/T', 'A/T/V')
  par(mar =c(4,5,4,2)+0.1)
  plot(0, xlim=c(0, 8), ylim=c(0,8),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Probabilities, ID:', id_number))
  abline(h=0:8)
  axis(2, at=seq(0.5, 7.5, 1), labels=type_labs[8:1], las=1)
  axis(1, at=seq(0.5, 7.5, 1), labels=type_labs[1:8], las=1)
  abline(v=0:8)
  exes <- seq(0.5, 7.5, 1)
  whys <- seq(7.5, 0.5, -1)
  for(j in 1:8){
    for(i in 1:8){
      polygon(c(exes[i]-0.5, exes[i]-0.5, exes[i]+0.5, exes[i]+0.5),
              c(whys[j]-0.5, whys[j]+0.5, whys[j]+0.5, whys[j]-0.5),
              col=rgb(0,0,0.75, round(transition_matrix[j, i],2)))
      text(exes[i], whys[j], labels = round(transition_matrix[j, i],2))
    }
  }
}


# PLOTTING FOR CHANGED FUNCTION OF AFFECT W TAV -----------------------------------------

plot_file_w_affect <- function(endtime=endtime,
                               aud_states=aud_states,
                               tac_states=tac_states,
                               vis_states=vis_states,
                               pos_states=pos_states,
                               neutral_states=neutral_states,
                               neg_states=neg_states,
                               numbered_states=numbered_states,
                               id_number=id_number){
  par(mar=c(3,8,3,8))
  plot(0, type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,9), ylab="", xlab="")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 8
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 7
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,1,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 6
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,1,0,.5))
  }
  for(i in 1:nrow(pos_states)){
    starttime <- pos_states[i,]$Start
    duration <- pos_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0.2, 0.3, 0.6,.5))
  }
  for(i in 1:nrow(neutral_states)){
    starttime <- neutral_states[i,]$Start
    duration <- neutral_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0.7, 1, 0.8,.5))
  }
  for(i in 1:nrow(neg_states)){
    starttime <- neg_states[i,]$Start
    duration <- neg_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1, 0.8, 0.2,.5))
  }
  lines(numbered_states[,1], 3*(numbered_states[,2]-1)/23)
  axis(1, at=seq(0,endtime,50), las=2)
  axis(2,at=c(1.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), labels=c("State Transition","Negative", "Neutral", "Positive", "Visual", "Tactile", "Auditory"), las=2)
  axis(4, at = c(0,3*seq(1,23)/23), labels=c('Neg-Aud-Tac-Vis', 'Neg-Tac-Vis', 'Neg-Aud-Vis', 'Neg-Tac-Aud',
                                             'Neg-Vis', 'Neg-Tac', 'Neg-Aud', 'Negative Only',
                                             'Pos-Aud-Tac-Vis', 'Pos-Tac-Vis', 'Pos-Aud-Vis', 'Pos-Tac-Aud',
                                             'Pos-Vis', 'Pos-Tac', 'Pos-Aud', 'Positive Only',
                                             'Neu-Aud-Tac-Vis', 'Neu-Tac-Vis', 'Neu-Aud-Vis', 'Neu-Tac-Aud',
                                             'Neu-Vis', 'Neu-Tac', 'Neu-Aud', 'Neutral Only'),
       las=2)
  
  abline(h=c(0,3,4,5,6, 7, 8))
  abline(h=3*seq(1,23)/23, lty=3, col=rgb(0,0,0,0.2))
  title(paste('File Identifier: ', id_number)) #, ', Entropy: ', round(entropy_rate,4)
}

plot_orig_w_affect <- function(endtime=endtime,
                               aud_states=aud_states,
                               tac_states=tac_states,
                               vis_states=vis_states,
                               pos_states=pos_states,
                               neutral_states=neutral_states,
                               neg_states=neg_states,
                               numbered_states=numbered_states,
                               id_number=id_number){
  par(mar=c(4,5,3,2)+0.1)
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,6), ylab="", xlab="Time")
  for(i in 1:nrow(aud_states)){
    starttime <- aud_states[i,]$Start
    duration <- aud_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(tac_states)){
    starttime <- tac_states[i,]$Start
    duration <- tac_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(vis_states)){
    starttime <- vis_states[i,]$Start
    duration <- vis_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(pos_states)){
    starttime <- pos_states[i,]$Start
    duration <- pos_states[i,]$Duration
    vert <- 2
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  for(i in 1:nrow(neutral_states)){
    starttime <- neutral_states[i,]$Start
    duration <- neutral_states[i,]$Duration
    vert <- 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  for(i in 1:nrow(neg_states)){
    starttime <- neg_states[i,]$Start
    duration <- neg_states[i,]$Duration
    vert <- 0
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels=c("Negative", "Neutral", "Positive", "Visual", "Tactile", "Auditory"), las=2)
  abline(h=c(0, 1, 2, 3, 4, 5))
  title(paste('File Identifier: ', id_number))
}

plot_transformed_w_affect <- function(endtime=endtime,
                                      alltypes=alltypes,
                                      id_number=id_number){
  par(mar=c(4,8,3,2)+0.1, mgp = c(3, 1, 0))
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,24), ylab="", xlab="Time")
  for(i in 1:nrow(alltypes)){
    starttime <- alltypes[i,]$Start
    duration <- alltypes[i,]$Duration
    vert <- alltypes[i,]$point - 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert),
            col = rgb(0,0,0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=seq(0.5, 23.5, 1), labels=c('Neg-Aud-Tac-Vis', 'Neg-Tac-Vis', 'Neg-Aud-Vis', 'Neg-Tac-Aud',
                                        'Neg-Vis', 'Neg-Tac', 'Neg-Aud', 'Negative Only',
                                        'Pos-Aud-Tac-Vis', 'Pos-Tac-Vis', 'Pos-Aud-Vis', 'Pos-Tac-Aud',
                                        'Pos-Vis', 'Pos-Tac', 'Pos-Aud', 'Positive Only',
                                        'Neu-Aud-Tac-Vis', 'Neu-Tac-Vis', 'Neu-Aud-Vis', 'Neu-Tac-Aud',
                                        'Neu-Vis', 'Neu-Tac', 'Neu-Aud', 'Neutral Only'), las=2)
  abline(h=0:24)
  title(paste('File Identifier: ', id_number))
}

plot_sequence_w_affect <- function(state_sequence=state_sequence,
                                   id_number=id_number){
  par(mar=c(4,8,3,1)+0.1, mgp = c(3, 1, 0))
  plot(0,type='n', axes=FALSE, xlim=c(0,nrow(state_sequence)), ylim=c(0,24), ylab="", xlab="Time Index")
  lines(0:(nrow(state_sequence)-1), state_sequence$point-0.5, lty=3, col=rgb(0,0,0,0.5))
  points(0:(nrow(state_sequence)-1), state_sequence$point-0.5, pch=19, col=rgb(0,0,0,0.5))
  abline(h=0:24)
  axis(1, at=seq(0,nrow(state_sequence),50), las=1)
  axis(2,at=seq(0.5, 23.5, 1), labels=c('Neg-Aud-Tac-Vis', 'Neg-Tac-Vis', 'Neg-Aud-Vis', 'Neg-Tac-Aud',
                                        'Neg-Vis', 'Neg-Tac', 'Neg-Aud', 'Negative Only',
                                        'Pos-Aud-Tac-Vis', 'Pos-Tac-Vis', 'Pos-Aud-Vis', 'Pos-Tac-Aud',
                                        'Pos-Vis', 'Pos-Tac', 'Pos-Aud', 'Positive Only',
                                        'Neu-Aud-Tac-Vis', 'Neu-Tac-Vis', 'Neu-Aud-Vis', 'Neu-Tac-Aud',
                                        'Neu-Vis', 'Neu-Tac', 'Neu-Aud', 'Neutral Only'), las=2)
  title(paste('File Identifier: ', id_number))
}

plot_counts_w_affect <- function(transition_counts, id_number){
  type_labs <- c('Neg/A/T/V', 'Neg/T/V', 'Neg/A/V', 'Neg/T/A',
                 'Neg/V', 'Neg/T', 'Neg/A', 'Neg',
                 'Pos/A/T/V', 'Pos/T/V', 'Pos/A/V', 'Pos/T/A',
                 'Pos/V', 'Pos/T', 'Pos/A', 'Pos',
                 'Neu/A/T/V', 'Neu/T/V', 'Neu/A/V', 'Neu/T/A',
                 'Neu/V', 'Neu/T', 'Neu/A', 'Neu')
  par(mar =c(5,7,3,3)+0.1, mgp = c(5, 1, 0))
  plot(0, xlim=c(0, 24), ylim=c(0,24),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Counts, ID:', id_number))
  abline(h=0:24)
  axis(2, at=seq(0.5, 23.5, 1), labels=type_labs[24:1], las=1)
  axis(1, at=seq(0.5, 23.5, 1), labels=type_labs[1:24], las=1)
  axis(4, at=seq(0.5, 23.5, 1)[24:1], labels=rowSums(transition_counts), las=1)
  abline(v=0:24)
  exes <- seq(0.5, 23.5, 1)
  whys <- seq(23.5, 0.5, -1)
  for(j in 1:24){
    for(i in 1:24){
      text(exes[i], whys[j], labels = transition_counts[j, i])
    }
  }
}

plot_transitions_w_affect <- function(transition_matrix, id_number){
  type_labs <- c('Neg/A/T/V', 'Neg/T/V', 'Neg/A/V', 'Neg/T/A',
                 'Neg/V', 'Neg/T', 'Neg/A', 'Neg',
                 'Pos/A/T/V', 'Pos/T/V', 'Pos/A/V', 'Pos/T/A',
                 'Pos/V', 'Pos/T', 'Pos/A', 'Pos',
                 'Neu/A/T/V', 'Neu/T/V', 'Neu/A/V', 'Neu/T/A',
                 'Neu/V', 'Neu/T', 'Neu/A', 'Neu')
  par(mar =c(6,7,4,1)+0.1, mgp = c(5, 1, 0))
  plot(0, xlim=c(0, 24), ylim=c(0,24),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Probabilities, ID:', id_number))
  abline(h=0:24)
  axis(2, at=seq(0.5, 23.5, 1), labels=type_labs[24:1], las=1)
  axis(1, at=seq(0.5, 23.5, 1), labels=type_labs[1:24], las=1)
  abline(v=0:24)
  exes <- seq(0.5, 23.5, 1)
  whys <- seq(23.5, 0.5, -1)
  for(j in 1:24){
    for(i in 1:24){
      polygon(c(exes[i]-0.5, exes[i]-0.5, exes[i]+0.5, exes[i]+0.5),
              c(whys[j]-0.5, whys[j]+0.5, whys[j]+0.5, whys[j]-0.5),
              col=rgb(0,0,0.75, round(transition_matrix[j, i],2)))
      text(exes[i], whys[j], labels = round(transition_matrix[j, i],2))
    }
  }
}

# PLOTTING FOR CHANGED FUNCTION OF AFFECT AND AUTONOMY GRANTING BEHAVIOR -----------------------------

plot_file_w_affect_autonomy <- function(endtime=endtime,
                                        pos_states=pos_states,
                                        neutral_states=neutral_states,
                                        neg_states=neg_states,
                                        autsup_states=autsup_states,
                                        intrusive_states=intrusive_states,
                                        neither_states=neither_states,
                                        numbered_states=numbered_states,
                                        id_number=id_number){
  par(mar=c(3,9,3,6))
  plot(0, type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,9), ylab="", xlab="")
  for(i in 1:nrow(autsup_states)){
    starttime <- autsup_states[i,]$Start
    duration <- autsup_states[i,]$Duration
    vert <- 8
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1,0,0,.5))
  }
  for(i in 1:nrow(intrusive_states)){
    starttime <- intrusive_states[i,]$Start
    duration <- intrusive_states[i,]$Duration
    vert <- 7
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,1,.5))
  }
  for(i in 1:nrow(neither_states)){
    starttime <- neither_states[i,]$Start
    duration <- neither_states[i,]$Duration
    vert <- 6
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,1,0,.5))
  }
  for(i in 1:nrow(pos_states)){
    starttime <- pos_states[i,]$Start
    duration <- pos_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0.2, 0.3, 0.6,.5))
  }
  for(i in 1:nrow(neutral_states)){
    starttime <- neutral_states[i,]$Start
    duration <- neutral_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0.7, 1, 0.8,.5))
  }
  for(i in 1:nrow(neg_states)){
    starttime <- neg_states[i,]$Start
    duration <- neg_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(1, 0.8, 0.2,.5))
  }
  lines(numbered_states[,1], 3*(numbered_states[,2]-1)/8)
  axis(1, at=seq(0,endtime,50), las=2)
  axis(2,at=c(1.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), labels=c("State Transition","Negative", "Neutral", "Positive", "Neither", "Intrusive", "Autonomy Support"), las=2)
  axis(4, at = c(0,3*seq(1,8)/8), labels=c('Neg-Intrusive', 'Neg-Autsup', 'Neg-Neither', 'Pos-Intrusive', 'Pos-Autsup', 'Pos-Neither',
                                           'Neu-Intrusive', 'Neu-Autsup', 'Neu-Neither'),
       las=2)
  
  abline(h=c(0,3,4,5,6,7,8))
  abline(h=3*seq(1,8)/8, lty=3, col=rgb(0,0,0,0.2))
  title(paste('File Identifier: ', id_number)) #, ', Entropy: ', round(entropy_rate,4)
}

plot_orig_w_affect_autonomy <- function(endtime=endtime,
                                        pos_states=pos_states,
                                        neutral_states=neutral_states,
                                        neg_states=neg_states,
                                        autsup_states=autsup_states,
                                        intrusive_states=intrusive_states,
                                        neither_states=neither_states,
                                        numbered_states=numbered_states,
                                        id_number=id_number){
  par(mar=c(4,9,4,2)+0.1, mgp = c(3, 1, 0))
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,6), ylab="", xlab="Time")
  for(i in 1:nrow(autsup_states)){
    starttime <- autsup_states[i,]$Start
    duration <- autsup_states[i,]$Duration
    vert <- 5
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(intrusive_states)){
    starttime <- intrusive_states[i,]$Start
    duration <- intrusive_states[i,]$Duration
    vert <- 4
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(neither_states)){
    starttime <- neither_states[i,]$Start
    duration <- neither_states[i,]$Duration
    vert <- 3
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0,0,0,.5))
  }
  for(i in 1:nrow(pos_states)){
    starttime <- pos_states[i,]$Start
    duration <- pos_states[i,]$Duration
    vert <- 2
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  for(i in 1:nrow(neutral_states)){
    starttime <- neutral_states[i,]$Start
    duration <- neutral_states[i,]$Duration
    vert <- 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  for(i in 1:nrow(neg_states)){
    starttime <- neg_states[i,]$Start
    duration <- neg_states[i,]$Duration
    vert <- 0
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert), col = rgb(0, 0, 0,.5))
  }
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), labels=c("Negative", "Neutral", "Positive", "Neither", "Intrusive", "Autonomy Support"), las=2)
  abline(h=c(0, 1, 2, 3, 4, 5))
  title(paste('File Identifier: ', id_number))
}

plot_transformed_w_affect_autonomy <- function(endtime=endtime,
                                               alltypes=alltypes,
                                               id_number=id_number){
  par(mar=c(4,7,4,3)+0.1, mgp = c(3, 1, 0))
  plot(0,type='n', axes=FALSE, xlim=c(0,endtime), ylim=c(0,9), ylab="", xlab="Time")
  for(i in 1:nrow(alltypes)){
    starttime <- alltypes[i,]$Start
    duration <- alltypes[i,]$Duration
    vert <- alltypes[i,]$point - 1
    polygon(c(starttime, starttime, starttime+duration, starttime+duration),
            c(vert, vert + 1, vert+1, vert),
            col = rgb(0,0,0,.5))
  }
  
  axis(1, at=seq(0,endtime,50), las=1)
  axis(2,at=seq(0.5, 8.5, 1), labels=c('Neg-Intrusive', 'Neg-Autsup', 'Neg-Neither', 'Pos-Intrusive', 'Pos-Autsup', 'Pos-Neither',
                                       'Neu-Intrusive', 'Neu-Autsup', 'Neu-Neither'), las=2)
  abline(h=0:9)
  title(paste('File Identifier: ', id_number))
}

plot_sequence_w_affect_autonomy <- function(state_sequence=state_sequence,
                                            id_number=id_number){
  par(mar=c(4,6,4,2)+0.1, mgp = c(3, 1, 0))
  num_sequences = nrow(state_sequence)
  if (100 - num_sequences < 10){
    num_sequences= 100
  }
  plot(0,type='n', axes=FALSE, xlim=c(0,num_sequences), ylim=c(0,9), ylab="", xlab="Time Index")
  lines(0:(nrow(state_sequence)-1), state_sequence$point-0.5, lty=3, col=rgb(0,0,0,0.5))
  points(0:(nrow(state_sequence)-1), state_sequence$point-0.5, pch=19, col=rgb(0,0,0,0.5))
  abline(h=0:9)
  axis(1, at=seq(0,num_sequences,50), las=1)
  axis(2,at=seq(0.5, 8.5, 1), labels=c('Neg-Intrusive', 'Neg-Autsup', 'Neg-Neither', 'Pos-Intrusive', 'Pos-Autsup', 'Pos-Neither',
                                       'Neu-Intrusive', 'Neu-Autsup', 'Neu-Neither'), las=2)
  title(paste('File Identifier: ', id_number))
}

plot_counts_w_affect_autonomy <- function(transition_counts, id_number){
  type_labs <- c('Neg/Intrusive', 'Neg/Autsup', 'Neg/Neither', 'Pos/Intrusive',
                 'Pos/Autsup', 'Pos/Neither', 'Neu/Intrusive', 'Neu/Autsup', 'Neu/Neither')
  par(mar =c(6,8,3,4)+0.1, mgp = c(6, 1, 0))
  plot(0, xlim=c(0, 9), ylim=c(0,9),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Counts, ID:', id_number))
  abline(h=0:9)
  axis(2, at=seq(0.5, 8.5, 1), labels=type_labs[9:1], las=1)
  axis(1, at=seq(0.5, 8.5, 1), labels=type_labs[1:9], las=1)
  axis(4, at=seq(0.5, 8.5, 1)[9:1], labels=rowSums(transition_counts), las=1)
  abline(v=0:9)
  exes <- seq(0.5, 8.5, 1)
  whys <- seq(8.5, 0.5, -1)
  for(j in 1:9){
    for(i in 1:9){
      text(exes[i], whys[j], labels = transition_counts[j, i])
    }
  }
}

plot_transitions_w_affect_autonomy <- function(transition_matrix, id_number){
  type_labs <- c('Neg/Intrusive', 'Neg/Autsup', 'Neg/Neither', 'Pos/Intrusive',
                 'Pos/Autsup', 'Pos/Neither', 'Neu/Intrusive', 'Neu/Autsup', 'Neu/Neither')
  par(mar =c(6,8,4,2)+0.1, mgp = c(6, 1, 0))
  plot(0, xlim=c(0, 9), ylim=c(0,9),
       pch=19, col=rgb(0,0,0,0), axes=F,
       ylab='From Action', xlab='To Action\n',
       xaxs='i', yaxs='i',
       main=paste('Transition Probabilities, ID:', id_number))
  abline(h=0:9)
  axis(2, at=seq(0.5, 8.5, 1), labels=type_labs[9:1], las=1)
  axis(1, at=seq(0.5, 8.5, 1), labels=type_labs[1:9], las=1)
  abline(v=0:9)
  exes <- seq(0.5, 8.5, 1)
  whys <- seq(8.5, 0.5, -1)
  for(j in 1:9){
    for(i in 1:9){
      polygon(c(exes[i]-0.5, exes[i]-0.5, exes[i]+0.5, exes[i]+0.5),
              c(whys[j]-0.5, whys[j]+0.5, whys[j]+0.5, whys[j]-0.5),
              col=rgb(0,0,0.75, round(transition_matrix[j, i],2)))
      text(exes[i], whys[j], labels = round(transition_matrix[j, i],2))
    }
  }
}


# ------ FUNCTIONS FOR AFFECT ----------

#' Estimate Behavioral Entropy Rate based upon Video Data Files for a Directory
#' @param dir_loc directory of files
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, pos, neg, neutral, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_dir_affect <- function(dir_loc,
                                   tactile_padding = 1.0,
                                   auditory_padding = 1.0,
                                   behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                       "mom_tactile_types" = c('TouchBaby',
                                                                               'HoldingBaby'),
                                                       "mom_visual_types" = c('ManipulatingObject'),
                                                       "baby_visual_types" = c('LookAtMomActivity'),
                                                       "missing_types" = c('CantTellHolding',
                                                                           'ActivityNotVisible',
                                                                           'CantTellLooking',
                                                                           'CantTellAffect'),
                                                       "positive" = c('positive'),
                                                       "negative" = c('negative'),
                                                       "neutral" = c('neutral')),
                                   missing_threshold = 0.1,
                                   log_file = paste(Sys.Date(), '-ber-logfile.txt', sep='')){
  
  old_dir <- getwd()
  setwd(path.expand(dir_loc))
  
  # Reading in data
  all_files = list.files('.', pattern="*.xlsx")
  n_files = length(all_files)
  
  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'=character(),
                          'CanEstimateEntropy'=logical(),
                          'EntropyRate'=double(),
                          'TotalNumberOfTransitions'=double(),
                          'CombinedVideoDuration'=double(),
                          'PercentMissing'=double(),
                          'AuditoryCounts'=double(),
                          'AuditoryTotalTime'=double(),
                          'AuditoryAverageTime'=double(),
                          'VisualCounts'=double(),
                          'VisualTotalTime'=double(),
                          'VisualAverageTime'=double(),
                          'TactileCounts'=double(),
                          'TactileTotalTime'=double(),
                          'TactileAverageTime'=double(),
                          'PositiveCounts'=double(),
                          'PositiveTotalTime'=double(),
                          'PositiveAverageTime'=double(),
                          'NeutralCounts'=double(),
                          'NeutralTotalTime'=double(),
                          'NeutralAverageTime'=double(),
                          'NegativeCounts'=double(),
                          'NegativeTotalTime'=double(),
                          'NegativeAverageTime'=double(),
                          stringsAsFactors = F)
  
  all_vars_named = c('NeutralOnly', 'NeutralAuditory', 'NeutralTactile', 'NeutralVisual',
                     'NeutralAuditoryTactile', 'NeutralAuditoryVisual', 'NeutralVisusalTactile', 'NeutralVisualTactileAuditory',
                     'PositiveOnly', 'PositiveAuditory', 'PositiveTactile', 'PositiveVisual',
                     'PositiveAuditoryTactile', 'PositiveAuditoryViuals', 'PositiveVisualTactile', 'PositiveVisualTactileAuditory',
                     'NegativeOnly', 'NeativegAuditory', 'NegativeTactile', 'NegativeVisual',
                     'NegativeAuditoryTactile', 'NegativeAuditoryVisual', 'NegatievVisualTactile', 'NegativeVisualTactileAuditory')
  
  for (index in 1:24) {
    cur_var_name = all_vars_named[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    resultsDF[cur_var_name_c] = double()
    resultsDF[cur_var_name_t] = double()
    resultsDF[cur_var_name_a] = double()
  }
  
  
  # Analysis Section -----------------------------------------------------------
  
  run_start <- Sys.time()
  run_count <- 1
  files_to_check <- c()
  for (i in 1:n_files){
    individual_file <- all_files[i]
    capture.output(file_results <- ber_analyze_file_affect(individual_file,
                                                           tactile_padding=tactile_padding,
                                                           auditory_padding=auditory_padding,
                                                           behavior_types=behavior_types,
                                                           missing_threshold=missing_threshold), file=NULL)
    
    if(sum(unlist(file_results$file_checks)) != 6){
      files_to_check <- c(files_to_check, tail(strsplit(individual_file, '/')[[1]], 1))
      capture.output(file_results <- ber_analyze_file_affect(individual_file,
                                                             tactile_padding=tactile_padding,
                                                             auditory_padding=auditory_padding,
                                                             behavior_types=behavior_types,
                                                             missing_threshold=missing_threshold),
                     file = log_file, append = T)
      resultsDF[i,] <- file_results$estimates
      message(paste('Warning - See log for file : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    } else{
      resultsDF[i,] <- file_results$estimates
      message(paste('Completed without issue    : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    }
    run_count = run_count + 1
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))
  
  message(paste(c(rep('-',25), ' Check the log for files below ', rep('-',25)), sep=''))
  
  for(i in 1:length(files_to_check)){
    message(files_to_check[i])
  }
  
  # Resetting the old directory pointer
  setwd(old_dir)
  
  return(resultsDF)
}


#' Estimate Behavioral Entropy Rate based upon Video Data
#'
#' @param f_loc file location
#' @param plot_all logical: Plot the data to observe the sequence of behaviors
#' @param plots_to_file logical: send all plots to a file
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.
#' Required sections: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, positive, negative, neutral, missing_types
#' @param missing_threshold proportion of acceptable missing time
#' @return Entropy rate estimates of an individual
ber_analyze_file_affect <- function(f_loc,
                                    plot_all=F,
                                    plots_to_file=F,
                                    tactile_padding = 1.0,
                                    auditory_padding = 1.0,
                                    behavior_types=list("mom_auditory_types" = c('Vocal'),
                                                        "mom_tactile_types" = c('TouchBaby',
                                                                                'HoldingBaby'),
                                                        "mom_visual_types" = c('ManipulatingObject'),
                                                        "baby_visual_types" = c('LookAtMomActivity'),
                                                        "positive" = c('positive'),
                                                        "negative" = c('negative'),
                                                        "neutral" = c('neutral'),
                                                        "missing_types" = c('CantTellHolding',
                                                                            'ActivityNotVisible',
                                                                            'CantTellLooking',
                                                                            'CantTellAffect')),
                                    missing_threshold = 0.1){
  
  file_checks <- list(header_pass = T,
                      subjid_pass = T,
                      misdat_pass = T,
                      blabel_pass = T,
                      elabel_pass = T,
                      misnes_pass = T)
  
  
  # Unpacking input behavior types ---------------------------------------------
  mom_auditory_types <- behavior_types$mom_auditory_types
  mom_tactile_types <- behavior_types$mom_tactile_types
  mom_visual_types <- behavior_types$mom_visual_types
  baby_visual_types <- behavior_types$baby_visual_types
  positive <- behavior_types$positive
  negative <- behavior_types$negative
  neutral <- behavior_types$neutral
  missing_types <- behavior_types$missing_types
  
  # extracting data from file using the readxl package
  behavior_data <- data.frame(readxl::read_xlsx(f_loc))
  
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste('Filename:        ', tail(strsplit(f_loc, '/')[[1]], 1), '\n'))
  cat(paste('Time of Analysis:', Sys.time(), '\n'))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  
  cat(paste(c(rep('*', 27), ' Performing File Check ',rep('*', 27), '\n'), collapse = ''))
  cat(paste('- Checking for required Columns:\n'))
  
  cat(paste('\tObservation      : First cell used to set "SubjectID"\n'))
  cat(paste('\tBehavior         : Set of used behavior labels\n'))
  cat(paste('\tTime_Relative_sf : Sets the start point for each action\n'))
  cat(paste('\tDuration_sf      : Time_Relative_sf + Duration_sf sets end points\n'))
  cat(paste('\tEvent_Type       : Defines point events and states\n'))
  
  columns_needed <- c('Observation', 'Behavior', 'Time_Relative_sf', 'Duration_sf', 'Event_Type')
  columns_found <- colnames(behavior_data)
  
  all_vars_named = c('NeutralOnly', 'NeutralAuditory', 'NeutralTactile', 'NeutralVisual',
                     'NeutralAuditoryTactile', 'NeutralAuditoryVisual', 'NeutralVisusalTactile', 'NeutralVisualTactileAuditory',
                     'PositiveOnly', 'PositiveAuditory', 'PositiveTactile', 'PositiveVisual',
                     'PositiveAuditoryTactile', 'PositiveAuditoryViuals', 'PositiveVisualTactile', 'PositiveVisualTactileAuditory',
                     'NegativeOnly', 'NeativegAuditory', 'NegativeTactile', 'NegativeVisual',
                     'NegativeAuditoryTactile', 'NegativeAuditoryVisual', 'NegatievVisualTactile', 'NegativeVisualTactileAuditory')
  
  
  if(sum(!(columns_needed %in% columns_found)) > 0){
    file_checks$header_pass <- F
    cat(paste('--- FAILED : Missing Column Headers:\n'))
    cat('\t')
    cat(paste(columns_needed[!(columns_needed %in% columns_found)], collapse = '\n\t'))
    cat('\n')
    cat(paste(c(rep('*', 9), ' IF FILE FORMATS HAVE CHANGED, THIS SCRIPT MUST BE UPDATED ',rep('*', 9), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    data2return <- data.frame('SubjectID'=NA,
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:24) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Found all Required Column Headers\n'))
  }
  
  # Mother ID should be in first cell in the observation column
  
  cat(paste('- Checking "Observation" Column For Subject ID\n'))
  id_number <- behavior_data$Observation[1]
  if(is.na(id_number) == T){
    file_checks$subjid_pass <- F
    cat(paste('--- FAILED: Data is "NA" in Column J, Cell 1\n'))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(NULL)
  } else{
    cat(paste('--- PASSED: Using Subject ID from Column J, Cell 1:', id_number, '\n'))
  }
  
  
  cat(paste('- Checking for Missing Data in Columns\n'))
  if(sum(is.na(behavior_data$Behavior)) > 0 |
     sum(is.na(behavior_data$Time_Relative_sf)) > 0 |
     sum(is.na(behavior_data$Duration_sf)) > 0 |
     sum(is.na(behavior_data$Event_Type)) > 0){
    
    file_checks$header_pass <- F
    
    if(sum(is.na(behavior_data$Behavior)) > 0){
      cat(paste('--- "Behavior"         : FAILED - Check Cells:', paste(which(is.na(behavior_data$Behavior))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Behavior"         : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Time_Relative_sf)) > 0){
      cat(paste('--- "Time_Relative_sf" : FAILED - Check Cells:', paste(which(is.na(behavior_data$Time_Relative_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Duration_sf)) > 0){
      cat(paste('--- "Duration_sf"      : FAILED - Check Cells:', paste(which(is.na(behavior_data$Duration_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Duration_sf"      : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Event_Type)) > 0){
      cat(paste('--- "Event_Type"       : FAILED - Check Cells:', paste(which(is.na(behavior_data$Event_Type))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Event_Type"       : PASSED\n'))
    }
    
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              stringsAsFactors = F)
    for (index in 1:24) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- "Behavior"         : PASSED\n'))
    cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    cat(paste('--- "Duration_sf"      : PASSED\n'))
    cat(paste('--- "Event_Type"       : PASSED\n'))
  }
  
  cat(paste('- Checking "Behavior" Column For Unused Labels:\n'))
  labels_used <- unique(behavior_data$Behavior)
  if(sum(!(labels_used %in% as.vector(unlist(behavior_types)))) > 0){
    cat(paste('--- WARNING : Unused Labels in "Behavior" Column, See Below:\n'))
    unused_labels <- labels_used[!(labels_used %in% as.vector(unlist(behavior_types)))]
    for(ul in unused_labels){
      if(ul %in% c('NotHoldingBaby', 'NotLookAtMomActivity', 'NoObjectInHand')){
        cat(paste('\tExpected Label  : "', ul, '", not used in analysis', '\n', sep=''))
      } else {
        file_checks$blabel_pass <- F
        cat(paste('\tUnexpected Label: "', ul, '", Cells: ', paste(which(behavior_data$Behavior == ul)+1, collapse = ','), '\n', sep=''))
      }
    }
    cat(paste('--- NOTE: Investigate this if these do not look familar\n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Behavior" Column\n'))
  }
  
  cat(paste('- Checking "Event_Type" Column For Labels:\n'))
  e_type_labels <- unique(behavior_data$Event_Type)
  e_type_expected <- c('State start', 'State point', 'Point', 'State stop', 'State Stop')
  if(sum(!(e_type_labels %in% e_type_expected)) > 0){
    cat(paste('--- WARNING : Unused Labels in "Event_Type" Column, See Below:\n'))
    unused_labels <- e_type_labels[!(e_type_labels %in% e_type_expected)]
    for(ul in unused_labels){
      file_checks$elabel_pass <- F
      cat(paste('\tLabel: "', ul, '", Cells: ', paste(which(behavior_data$Event_Type == ul)+1, collapse = ','), '\n', sep=''))
    }
    cat(paste('--- NOTE: This may require a fix to the function ".subset_by_types()" \n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Event_Type" Column\n'))
  }
  
  # Identifying the last time between both the mother and baby files
  lasttime = max(behavior_data$Time_Relative_sf)
  lastduration = max(subset(behavior_data,
                            behavior_data$Time_Relative_sf==max(behavior_data$Time_Relative_sf))$Duration_sf)
  endtime = lasttime + lastduration
  ##############################################################################
  # Finding the total amount of missing time
  cat(paste('- Checking Missingness based on "missing_types"\n'))
  missing <- .subset_by_types(behavior_data,
                              missing_types)
  
  missing <- missing[order(missing$Time_Relative_sf,decreasing = FALSE),]
  missing$Duration_sf = missing$Time_Relative_sf + missing$Duration_sf #convert to get relative end time
  #2. we will start at the top of the missing list
  total_sum_missing = 0
  cur_start_time = 0
  if (nrow(missing) > 0) {
    cur_start_time = missing[1,1]
    for (i in 1:(nrow(missing)-1)) {
      cur_end_time = missing[i,2]
      dur = cur_end_time - cur_start_time
      total_sum_missing = total_sum_missing + dur
      next_start_time = missing[i+1,1]
      next_end_time = missing[i+1,2]
      if (next_start_time > cur_end_time){
        cur_start_time = next_start_time
      } else if(cur_end_time < next_end_time) {
        cur_start_time = cur_end_time
      } else {
        cur_start_time = cur_end_time
        next
      }
    }
    total_sum_missing = total_sum_missing + (missing[i+1,2] - cur_start_time)
  } 
  
  percent_missing <- total_sum_missing/endtime
  cat(paste('--- Percent Missingness:', round(percent_missing,3), '\n'))
  if(percent_missing>=missing_threshold){
    file_checks$misnes_pass <- F
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=endtime,
                              'PercentMissing'=percent_missing,
                              'AuditoryCounts'=NA,
                              'AuditoryTotalTime'=NA,
                              'AuditoryAverageTime'=NA,
                              'VisualCounts'=NA,
                              'VisualTotalTime'=NA,
                              'VisualAverageTime'=NA,
                              'TactileCounts'=NA,
                              'TactileTotalTime'=NA,
                              'TactileAverageTime'=NA,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:24) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste('--- FAILED: Missingness greater than threshold', missing_threshold, '\n'))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Percent missing less than threshold\n'))
  }
  # ##############################################################################
  #
  # ##############################################################################
  # Finding all of the unique events for a given state type
  baby_vis <- .subset_by_types(behavior_data, baby_visual_types)
  mom_vis <- .subset_by_types(behavior_data, mom_visual_types)
  mom_tac <- .subset_by_types(behavior_data, mom_tactile_types, tactile_padding)
  mom_aud <- .subset_by_types(behavior_data, mom_auditory_types, auditory_padding)
  pos_affect <- .subset_by_types(behavior_data, positive)
  neg_affect <- .subset_by_types(behavior_data, negative)
  neutral_affect <- .subset_by_types(behavior_data, neutral)
  #
  # Reducing these events, either by intersection or union of events
  whole_interval <- intervals::Intervals(c(0,endtime))
  tac_states <- .find_unions(mom_tac, "TACTILE")
  aud_states <- .find_unions(mom_aud, "AUDITORY")
  vis_states <- .compare_intersection(mom_vis, baby_vis, "VISUAL")
  pos_states <- .find_unions(pos_affect, "POSITIVE")
  neutral_states <- .find_unions(neutral_affect, "NEUTRAL")
  neg_states <- .find_unions(neg_affect, "NEGATIVE")
  nottac_states <- .state_complement(whole_interval, tac_states, "NOT TACTILE")
  notaud_states <- .state_complement(whole_interval, aud_states, "NOT AUDITORY")
  notvis_states <- .state_complement(whole_interval, vis_states, "NOT VISUAL")
  
  notpos_states <- .state_complement(whole_interval, pos_states, "NOT POSITIVE")
  notneutral_states <- .state_complement(whole_interval, neutral_states, "NOT NEUTRAL")
  notneg_states <- .state_complement(whole_interval, neg_states, "NOT NEGATIVE")
  
  #24 states
  neu <- .find_macrostate_new(notvis_states, nottac_states, notaud_states, neutral_states, "!V-!T-!A-Neu", 24)
  aud_neu <- .find_macrostate_new(notvis_states, nottac_states, aud_states, neutral_states, "!V-!T-A-Neu",23)
  tac_neu <- .find_macrostate_new(notvis_states, tac_states, notaud_states, neutral_states, "!V-T-!A-Neu",22)
  vis_neu <- .find_macrostate_new(vis_states, nottac_states, notaud_states, neutral_states, "V-!T-!A-Neu",21)
  aud_tac_neu <- .find_macrostate_new(notvis_states, tac_states, aud_states, neutral_states, "!V-T-A-Neu",20)
  aud_vis_neu <- .find_macrostate_new(vis_states, nottac_states, aud_states, neutral_states, "V-!T-A-Neu",19)
  vis_tac_neu <- .find_macrostate_new(vis_states, tac_states, notaud_states, neutral_states, "V-T-!A-Neu",18)
  vis_aud_tac_neu <- .find_macrostate_new(vis_states, tac_states, aud_states, neutral_states, "V-T-A-Neu",17)
  
  pos <- .find_macrostate_new(notvis_states, nottac_states, notaud_states, pos_states, "!V-!T-!A-Pos", 16)
  aud_pos <- .find_macrostate_new(notvis_states, nottac_states, aud_states, pos_states, "!V-!T-A-Pos",15)
  tac_pos <- .find_macrostate_new(notvis_states, tac_states, notaud_states, pos_states, "!V-T-!A-Pos",14)
  vis_pos <- .find_macrostate_new(vis_states, nottac_states, notaud_states, pos_states, "V-!T-!A-Pos",13)
  aud_tac_pos <- .find_macrostate_new(notvis_states, tac_states, aud_states, pos_states, "!V-T-A-Pos",12)
  aud_vis_pos <- .find_macrostate_new(vis_states, nottac_states, aud_states, pos_states, "V-!T-A-Pos",11)
  vis_tac_pos <- .find_macrostate_new(vis_states, tac_states, notaud_states, pos_states, "V-T-!A-Pos",10)
  vis_aud_tac_pos <- .find_macrostate_new(vis_states, tac_states, aud_states, pos_states, "V-T-A-Pos",9)
  
  neg <- .find_macrostate_new(notvis_states, nottac_states, notaud_states, neg_states, "!V-!T-!A-Neg", 8)
  aud_neg <- .find_macrostate_new(notvis_states, nottac_states, aud_states, neg_states, "!V-!T-A-Neg",7)
  tac_neg <- .find_macrostate_new(notvis_states, tac_states, notaud_states, neg_states, "!V-T-!A-Neg",6)
  vis_neg <- .find_macrostate_new(vis_states, nottac_states, notaud_states, neg_states, "V-!T-!A-Neg",5)
  aud_tac_neg <- .find_macrostate_new(notvis_states, tac_states, aud_states, neg_states, "!V-T-A-Neg",4)
  aud_vis_neg <- .find_macrostate_new(vis_states, nottac_states, aud_states, neg_states, "V-!T-A-Neg",3)
  vis_tac_neg <- .find_macrostate_new(vis_states, tac_states, notaud_states, neg_states, "V-T-!A-Neg",2)
  vis_aud_tac_neg <- .find_macrostate_new(vis_states, tac_states, aud_states, neg_states, "V-T-A-Neg",1)
  
  alltypes <- rbind(neu, aud_neu, tac_neu, vis_neu, aud_tac_neu, aud_vis_neu, vis_tac_neu, vis_aud_tac_neu,
                    pos, aud_pos, tac_pos, vis_pos, aud_tac_pos, aud_vis_pos, vis_tac_pos, vis_aud_tac_pos,
                    neg, aud_neg, tac_neg, vis_neg, aud_tac_neg, aud_vis_neg, vis_tac_neg, vis_aud_tac_neg)
  state_sequence <- alltypes[with(alltypes, order(Start)), ]
  # ##############################################################################
  #
  # ##############################################################################
  # # Entropy Calculations
  transition_counts <- CalcTransitionCounts(state_sequence[,5], 24)
  transition_matrix <- CalcTransitionMatrix(transition_counts)
  stationary_matrix <- CalcEmpiricalStationary(state_sequence[,5], 1:24)
  entropy_rate <- CalcMarkovEntropyRate(transition_matrix, stationary_matrix)
  
  numbered_states <- c()
  for(i in 1:nrow(state_sequence)){
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$Start, state_sequence[i,]$point))
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$End, state_sequence[i,]$point))
  }
  
  if(plot_all == T){
    if (plots_to_file == T){
      mainDir <- path.expand(getwd())
      subDir <- "TAV_AffectPlots"
      if (file.exists(subDir)){
        setwd(file.path(mainDir, subDir))
      } else {
        dir.create(file.path(mainDir, subDir))
        setwd(file.path(mainDir, subDir))
      }
      pdf(paste(as.character(id_number), "_all_plots_w_affect.pdf", sep=""))
      plot_file_w_affect(endtime=endtime,
                         aud_states=aud_states,
                         tac_states=tac_states,
                         vis_states=vis_states,
                         pos_states=pos_states,
                         neutral_states=neutral_states,
                         neg_states=neg_states,
                         numbered_states=numbered_states,
                         id_number=id_number)
      
      plot_orig_w_affect(endtime=endtime,
                         aud_states=aud_states,
                         tac_states=tac_states,
                         vis_states=vis_states,
                         pos_states=pos_states,
                         neutral_states=neutral_states,
                         neg_states=neg_states,
                         numbered_states=numbered_states,
                         id_number=id_number)
      
      plot_transformed_w_affect(endtime=endtime,
                                alltypes=alltypes,
                                id_number=id_number)
      
      plot_sequence_w_affect(state_sequence=state_sequence,
                             id_number=id_number)
      
      plot_counts_w_affect(transition_counts, id_number)
      
      plot_transitions_w_affect(transition_matrix, id_number)
      dev.off()
      setwd('../')
    }
    if (plot_all == T) {
      plot_file_w_affect(endtime=endtime,
                         aud_states=aud_states,
                         tac_states=tac_states,
                         vis_states=vis_states,
                         pos_states=pos_states,
                         neutral_states=neutral_states,
                         neg_states=neg_states,
                         numbered_states=numbered_states,
                         id_number=id_number)
      
      plot_orig_w_affect(endtime=endtime,
                         aud_states=aud_states,
                         tac_states=tac_states,
                         vis_states=vis_states,
                         pos_states=pos_states,
                         neutral_states=neutral_states,
                         neg_states=neg_states,
                         numbered_states=numbered_states,
                         id_number=id_number)
      
      plot_transformed_w_affect(endtime=endtime,
                                alltypes=alltypes,
                                id_number=id_number)
      
      plot_sequence_w_affect(state_sequence=state_sequence,
                             id_number=id_number)
      
      plot_counts_w_affect(transition_counts, id_number)
      
      plot_transitions_w_affect(transition_matrix, id_number)
    }
  }
  
  data2return <- data.frame('SubjectID'=as.character(id_number),
                            'CanEstimateEntropy'=TRUE,
                            'EntropyRate'=entropy_rate,
                            'TotalNumberOfTransitions'=sum(transition_counts),
                            'CombinedVideoDuration'=endtime,
                            'PercentMissing'=percent_missing,
                            'AuditoryCounts'=nrow(aud_states),
                            'AuditoryTotalTime'=sum(aud_states[,3]),
                            'AuditoryAverageTime'=mean(aud_states[,3]),
                            'VisualCounts'=nrow(vis_states),
                            'VisualTotalTime'=sum(vis_states[,3]),
                            'VisualAverageTime'=mean(vis_states[,3]),
                            'TactileCounts'=nrow(tac_states),
                            'TactileTotalTime'=sum(tac_states[,3]),
                            'TactileAverageTime'=mean(tac_states[,3]),
                            'PositiveCounts'=nrow(pos_states),
                            'PositiveTotalTime'=sum(pos_states[,3]),
                            'PositiveAverageTime'=mean(pos_states[,3]),
                            'NeutralCounts'=nrow(neutral_states),
                            'NeutralTotalTime'=sum(neutral_states[,3]),
                            'NeutralAverageTime'=mean(neutral_states[,3]),
                            'NegativeCounts'=nrow(neg_states),
                            'NegativeTotalTime'=sum(neg_states[,3]),
                            'NegativeAverageTime'=mean(neg_states[,3]),
                            stringsAsFactors = F)
  
  #here we are going to be adding the 72 new co-occurrence variables
  affect_states <- c('Neutral', 'Positive', 'Negative')
  all_vars_lengths <- c(nrow(neu), nrow(aud_neu), nrow(tac_neu), nrow(vis_neu), nrow(aud_tac_neu), nrow(aud_vis_neu), nrow(vis_tac_neu), nrow(vis_aud_tac_neu),
                        nrow(pos), nrow(aud_pos), nrow(tac_pos), nrow(vis_pos), nrow(aud_tac_pos), nrow(aud_vis_pos), nrow(vis_tac_pos), nrow(vis_aud_tac_pos),
                        nrow(neg), nrow(aud_neg), nrow(tac_neg), nrow(vis_neg), nrow(aud_tac_neg), nrow(aud_vis_neg), nrow(vis_tac_neg), nrow(vis_aud_tac_neg))
  
  all_vars_sums <- c(sum(neu$Duration), sum(aud_neu$Duration), sum(tac_neu$Duration), sum(vis_neu$Duration), sum(aud_tac_neu$Duration), sum(aud_vis_neu$Duration), sum(vis_tac_neu$Duration), sum(vis_aud_tac_neu$Duration),
                     sum(pos$Duration), sum(aud_pos$Duration), sum(tac_pos$Duration), sum(vis_pos$Duration), sum(aud_tac_pos$Duration), sum(aud_vis_pos$Duration), sum(vis_tac_pos$Duration), sum(vis_aud_tac_pos$Duration),
                     sum(neg$Duration), sum(aud_neg$Duration), sum(tac_neg$Duration), sum(vis_neg$Duration), sum(aud_tac_neg$Duration), sum(aud_vis_neg$Duration), sum(vis_tac_neg$Duration), sum(vis_aud_tac_neg$Duration))
  
  
  all_vars_named_sec <- c('NeuOnly', 'NeuAud', 'NeuTac', 'NeuVis',
                          'NeuAudTac', 'NeuAudVis', 'NeuVisTac', 'NeuVisTacAud',
                          'PosOnly', 'PosAud', 'PosTac', 'PosVis',
                          'PosAudTac', 'PosAudVis', 'PosVisTac', 'PosVisTacAud',
                          'NegOnly', 'NegAud', 'NegTac', 'NegVis',
                          'NegAudTac', 'NegAudVis', 'NegVisTac', 'NegVisTacAud')
  
  for (index in 1:24) {
    cur_var_name = all_vars_named_sec[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    len = all_vars_lengths[index]
    sum_all = all_vars_sums[index]
    data2return[cur_var_name_c] = len
    data2return[cur_var_name_t] = sum_all
    if (len == 0) {
      data2return[cur_var_name_a] = 0
    } else {
      data2return[cur_var_name_a] = sum_all / len
    }
  }
  
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste(c(rep('*', 24), ' File Completed Successfully ',rep('*', 24), '\n'), collapse = ''))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  return(list(estimates=data2return, file_checks=file_checks, entropy=entropy_rate))
}



# ------ FUNCTIONS FOR AFFECT & AUTONOMY GRANTING ONLY ----------

#' Estimate Behavioral Entropy Rate based upon Video Data Files for a Directory
#' @param dir_loc directory of files
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.  Required sections, positive, negative, neutral,
#'                                                      autonomy support, intrusiveness, neither, missing_types
#' @param missing_threshold proportion of acceptable missing time
#'
#' @return Entropy rate estimates of an individual
ber_analyze_dir_affect_and_autonomy <- function(dir_loc,
                                                tactile_padding = 1.0,
                                                auditory_padding = 1.0,
                                                behavior_types=list("missing_types" = c('CantTellAffect', 'CantTellBehavior'),
                                                                    "positive" = c('positive'),
                                                                    "negative" = c('negative'),
                                                                    "neutral" = c('neutral'),
                                                                    "autonomy_support" = c('AutonomySupport'),
                                                                    "intrusiveness" = c('Intrusiveness'),
                                                                    "neither" = c('Neither')),
                                                missing_threshold = 0.1,
                                                log_file = paste(Sys.Date(), '-ber-logfile.txt', sep='')){
  
  old_dir <- getwd()
  setwd(path.expand(dir_loc))
  
  # Reading in data
  all_files = list.files('.', pattern="*.xlsx")
  n_files = length(all_files)
  
  # Allocating DataFrame for returning data
  resultsDF <- data.frame('SubjectID'=character(),
                          'CanEstimateEntropy'=logical(),
                          'EntropyRate'=double(),
                          'TotalNumberOfTransitions'=double(),
                          'CombinedVideoDuration'=double(),
                          'PercentMissing'=double(),
                          'PositiveCounts'=double(),
                          'PositiveTotalTime'=double(),
                          'PositiveAverageTime'=double(),
                          'NeutralCounts'=double(),
                          'NeutralTotalTime'=double(),
                          'NeutralAverageTime'=double(),
                          'NegativeCounts'=double(),
                          'NegativeTotalTime'=double(),
                          'NegativeAverageTime'=double(),
                          'NeitherCounts'=double(),
                          'NeitherTotalTime'=double(),
                          'NeitherAverageTime'=double(),
                          'AutonomyCounts'=double(),
                          'AutonomyTotalTime'=double(),
                          'AutonomyAverageTime'=double(),
                          'IntrusivenessCounts'=double(),
                          'IntrusivenessTotalTime'=double(),
                          'IntrusivenessAverageTime'=double(),
                          stringsAsFactors = F)
  
  all_vars_named <- c('NeutralNeither', 'NeutralAutonomySupport', 'NeutralIntrusive',
                      'PositiveNeither', 'PositiveAutonomySupport', 'PositiveIntrusive',
                      'NegativeNeither', 'NegativeAutonomySupport', 'NegativeIntrusive')
  
  for (index in 1:9) {
    cur_var_name = all_vars_named[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    resultsDF[cur_var_name_c] = double()
    resultsDF[cur_var_name_t] = double()
    resultsDF[cur_var_name_a] = double()
  }
  
  # Analysis Section -----------------------------------------------------------
  
  run_start <- Sys.time()
  run_count <- 1
  files_to_check <- c()
  for (i in 1:n_files){
    individual_file <- all_files[i]
    capture.output(file_results <- ber_analyze_file_affect_and_autonomy(individual_file,
                                                                        behavior_types=behavior_types,
                                                                        missing_threshold=missing_threshold), file=NULL)
    
    if(sum(unlist(file_results$file_checks)) != 6){
      files_to_check <- c(files_to_check, tail(strsplit(individual_file, '/')[[1]], 1))
      capture.output(file_results <- ber_analyze_file_affect_and_autonomy(individual_file,
                                                                          behavior_types=behavior_types,
                                                                          missing_threshold=missing_threshold),
                     file = log_file, append = T)
      resultsDF[i,] <- file_results$estimates
      message(paste('Warning - See log for file : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    } else{
      resultsDF[i,] <- file_results$estimates
      message(paste('Completed without issue    : ', tail(strsplit(individual_file, '/')[[1]], 1)))
    }
    run_count = run_count + 1
  }
  run_dur <- Sys.time() - run_start
  message(paste('Script total run time: ', round(as.numeric(run_dur, units='mins'),3), 'minutes'))
  
  message(paste(c(rep('-',25), ' Check the log for files below ', rep('-',25)), sep=''))
  
  for(i in 1:length(files_to_check)){
    message(files_to_check[i])
  }
  
  # Resetting the old directory pointer
  setwd(old_dir)
  
  return(resultsDF)
}


#' Estimate Behavioral Entropy Rate based upon Video Data
#'
#' @param f_loc file location
#' @param plot_all logical: Plot the data to observe the sequence of behaviors
#' @param plots_to_file logical: send all plots to a file
#' @param tactile_padding right padding adjustment to tactile events
#' @param auditory_padding right padding adjustment to auditory events
#' @param behavior_types dictionary of behavior types.
#' Required sections: mom_auditory_types, mom_tactile_types, mom_visual_types, baby_visual_types, positive, negative, neutral, missing_types
#' @param missing_threshold proportion of acceptable missing time
#' @return Entropy rate estimates of an individual
ber_analyze_file_affect_and_autonomy <- function(f_loc,
                                                 plot_all=F,
                                                 plots_to_file=F,
                                                 behavior_types=list("positive" = c('positive'),
                                                                     "negative" = c('negative'),
                                                                     "neutral" = c('neutral'),
                                                                     "autonomy_support" = c('AutonomySupport'),
                                                                     "intrusiveness" = c('Intrusiveness'),
                                                                     "neither" = c('Neither'),
                                                                     "missing_types" = c('CantTellAffect', 'CantTellBehavior')),
                                                 missing_threshold = 0.1){
  
  file_checks <- list(header_pass = T,
                      subjid_pass = T,
                      misdat_pass = T,
                      blabel_pass = T,
                      elabel_pass = T,
                      misnes_pass = T)
  
  
  # Unpacking input behavior types ---------------------------------------------
  positive <- behavior_types$positive
  negative <- behavior_types$negative
  neutral <- behavior_types$neutral
  autonomy_support <- behavior_types$autonomy_support
  intrusiveness <- behavior_types$intrusiveness
  neither <- behavior_types$neither
  missing_types <- behavior_types$missing_types
  
  # extracting data from file using the readxl package
  behavior_data <- data.frame(readxl::read_xlsx(f_loc))
  
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste('Filename:        ', tail(strsplit(f_loc, '/')[[1]], 1), '\n'))
  cat(paste('Time of Analysis:', Sys.time(), '\n'))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  
  cat(paste(c(rep('*', 27), ' Performing File Check ',rep('*', 27), '\n'), collapse = ''))
  cat(paste('- Checking for required Columns:\n'))
  
  cat(paste('\tObservation      : First cell used to set "SubjectID"\n'))
  cat(paste('\tBehavior         : Set of used behavior labels\n'))
  cat(paste('\tTime_Relative_sf : Sets the start point for each action\n'))
  cat(paste('\tDuration_sf      : Time_Relative_sf + Duration_sf sets end points\n'))
  cat(paste('\tEvent_Type       : Defines point events and states\n'))
  
  columns_needed <- c('Observation', 'Behavior', 'Time_Relative_sf', 'Duration_sf', 'Event_Type')
  columns_found <- colnames(behavior_data)
  
  all_vars_named <- c('NeutralNeither', 'NeutralAutonomySupport', 'NeutralIntrusive',
                      'PositiveNeither', 'PositiveAutonomySupport', 'PositiveIntrusive',
                      'NegativeNeither', 'NegativeAutonomySupport', 'NegativeIntrusive')
  
  if(sum(!(columns_needed %in% columns_found)) > 0){
    file_checks$header_pass <- F
    cat(paste('--- FAILED : Missing Column Headers:\n'))
    cat('\t')
    cat(paste(columns_needed[!(columns_needed %in% columns_found)], collapse = '\n\t'))
    cat('\n')
    cat(paste(c(rep('*', 9), ' IF FILE FORMATS HAVE CHANGED, THIS SCRIPT MUST BE UPDATED ',rep('*', 9), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    data2return <- data.frame('SubjectID'=NA,
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              'NeitherCounts'=NA,
                              'NeitherTotalTime'=NA,
                              'NeitherAverageTime'=NA,
                              'AutonomyCounts'=NA,
                              'AutonomyTotalTime'=NA,
                              'AutonomyAverageTime'=NA,
                              'IntrusivenessCounts'=NA,
                              'IntrusivenessTotalTime'=NA,
                              'IntrusivenessAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:9) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Found all Required Column Headers\n'))
  }
  
  # Mother ID should be in first cell in the observation column
  
  cat(paste('- Checking "Observation" Column For Subject ID\n'))
  id_number <- behavior_data$Observation[1]
  if(is.na(id_number) == T){
    file_checks$subjid_pass <- F
    cat(paste('--- FAILED: Data is "NA" in Column J, Cell 1\n'))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(NULL)
  } else{
    cat(paste('--- PASSED: Using Subject ID from Column J, Cell 1:', id_number, '\n'))
  }
  
  
  cat(paste('- Checking for Missing Data in Columns\n'))
  if(sum(is.na(behavior_data$Behavior)) > 0 |
     sum(is.na(behavior_data$Time_Relative_sf)) > 0 |
     sum(is.na(behavior_data$Duration_sf)) > 0 |
     sum(is.na(behavior_data$Event_Type)) > 0){
    
    file_checks$header_pass <- F
    
    if(sum(is.na(behavior_data$Behavior)) > 0){
      cat(paste('--- "Behavior"         : FAILED - Check Cells:', paste(which(is.na(behavior_data$Behavior))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Behavior"         : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Time_Relative_sf)) > 0){
      cat(paste('--- "Time_Relative_sf" : FAILED - Check Cells:', paste(which(is.na(behavior_data$Time_Relative_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Duration_sf)) > 0){
      cat(paste('--- "Duration_sf"      : FAILED - Check Cells:', paste(which(is.na(behavior_data$Duration_sf))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Duration_sf"      : PASSED\n'))
    }
    
    if(sum(is.na(behavior_data$Event_Type)) > 0){
      cat(paste('--- "Event_Type"       : FAILED - Check Cells:', paste(which(is.na(behavior_data$Event_Type))+1, collapse = ', '), '\n'))
    } else{
      cat(paste('--- "Event_Type"       : PASSED\n'))
    }
    
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=NA,
                              'PercentMissing'=NA,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              'NeitherCounts'=NA,
                              'NeitherTotalTime'=NA,
                              'NeitherAverageTime'=NA,
                              'AutonomyCounts'=NA,
                              'AutonomyTotalTime'=NA,
                              'AutonomyAverageTime'=NA,
                              'IntrusivenessCounts'=NA,
                              'IntrusivenessTotalTime'=NA,
                              'IntrusivenessAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:9) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    cat(paste(c(rep('*', 16), ' File Failed: Unable to Estimate Entropy Rate ',rep('*', 16), '\n'), collapse = ''))
    cat(paste(c(rep('-', 78), '\n'),collapse = ''))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- "Behavior"         : PASSED\n'))
    cat(paste('--- "Time_Relative_sf" : PASSED\n'))
    cat(paste('--- "Duration_sf"      : PASSED\n'))
    cat(paste('--- "Event_Type"       : PASSED\n'))
  }
  
  cat(paste('- Checking "Behavior" Column For Unused Labels:\n'))
  labels_used <- unique(behavior_data$Behavior)
  if(sum(!(labels_used %in% as.vector(unlist(behavior_types)))) > 0){
    cat(paste('--- WARNING : Unused Labels in "Behavior" Column, See Below:\n'))
    unused_labels <- labels_used[!(labels_used %in% as.vector(unlist(behavior_types)))]
    for(ul in unused_labels){
      if(ul %in% c('NotHoldingBaby', 'NotLookAtMomActivity', 'NoObjectInHand')){
        cat(paste('\tExpected Label  : "', ul, '", not used in analysis', '\n', sep=''))
      } else {
        file_checks$blabel_pass <- F
        cat(paste('\tUnexpected Label: "', ul, '", Cells: ', paste(which(behavior_data$Behavior == ul)+1, collapse = ','), '\n', sep=''))
      }
    }
    cat(paste('--- NOTE: Investigate this if these do not look familar\n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Behavior" Column\n'))
  }
  
  cat(paste('- Checking "Event_Type" Column For Labels:\n'))
  e_type_labels <- unique(behavior_data$Event_Type)
  e_type_expected <- c('State start', 'State point', 'Point', 'State stop', 'State Stop')
  if(sum(!(e_type_labels %in% e_type_expected)) > 0){
    cat(paste('--- WARNING : Unused Labels in "Event_Type" Column, See Below:\n'))
    unused_labels <- e_type_labels[!(e_type_labels %in% e_type_expected)]
    for(ul in unused_labels){
      file_checks$elabel_pass <- F
      cat(paste('\tLabel: "', ul, '", Cells: ', paste(which(behavior_data$Event_Type == ul)+1, collapse = ','), '\n', sep=''))
    }
    cat(paste('--- NOTE: This may require a fix to the function ".subset_by_types()" \n'))
  } else {
    cat(paste('--- PASSED : No Unused Labels in "Event_Type" Column\n'))
  }
  
  # Identifying the last time between both the mother and baby files
  lasttime = max(behavior_data$Time_Relative_sf)
  lastduration = max(subset(behavior_data,
                            behavior_data$Time_Relative_sf==max(behavior_data$Time_Relative_sf))$Duration_sf)
  endtime = lasttime + lastduration
  ##############################################################################
  # Finding the total amount of missing time
  cat(paste('- Checking Missingness based on "missing_types"\n'))
  missing <- .subset_by_types(behavior_data,
                              missing_types)
  
  
  missing <- missing[order(missing$Time_Relative_sf,decreasing = FALSE),]
  missing$Duration_sf = missing$Time_Relative_sf + missing$Duration_sf #convert to get relative end time
  #2. we will start at the top of the missing list
  total_sum_missing = 0
  cur_start_time = 0
  if (nrow(missing) > 0) {
    cur_start_time = missing[1,1]
    for (i in 1:(nrow(missing)-1)) {
      cur_end_time = missing[i,2]
      dur = cur_end_time - cur_start_time
      total_sum_missing = total_sum_missing + dur
      next_start_time = missing[i+1,1]
      next_end_time = missing[i+1,2]
      if (next_start_time > cur_end_time){
        cur_start_time = next_start_time
      } else if(cur_end_time < next_end_time) {
        cur_start_time = cur_end_time
      } else {
        cur_start_time = cur_end_time
        next
      }
    }
    total_sum_missing = total_sum_missing + (missing[i+1,2] - cur_start_time)
  } 
  
  percent_missing <- total_sum_missing/endtime
  cat(paste('--- Percent Missingness:', round(percent_missing,3), '\n'))
  if(percent_missing>=missing_threshold){
    file_checks$misnes_pass <- F
    data2return <- data.frame('SubjectID'=as.character(id_number),
                              'CanEstimateEntropy'=FALSE,
                              'EntropyRate'=NA,
                              'TotalNumberOfTransitions'=NA,
                              'CombinedVideoDuration'=endtime,
                              'PercentMissing'=percent_missing,
                              'PositiveCounts'=NA,
                              'PositiveTotalTime'=NA,
                              'PositiveAverageTime'=NA,
                              'NeutralCounts'=NA,
                              'NeutralTotalTime'=NA,
                              'NeutralAverageTime'=NA,
                              'NegativeCounts'=NA,
                              'NegativeTotalTime'=NA,
                              'NegativeAverageTime'=NA,
                              'NeitherCounts'=NA,
                              'NeitherTotalTime'=NA,
                              'NeitherAverageTime'=NA,
                              'AutonomyCounts'=NA,
                              'AutonomyTotalTime'=NA,
                              'AutonomyAverageTime'=NA,
                              'IntrusivenessCounts'=NA,
                              'IntrusivenessTotalTime'=NA,
                              'IntrusivenessAverageTime'=NA,
                              stringsAsFactors = F)
    
    for (index in 1:9) {
      cur_var_name = all_vars_named[index]
      cur_var_name_c = paste(cur_var_name, "Count", sep="")
      cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
      cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
      data2return[cur_var_name_c] = NA
      data2return[cur_var_name_t] = NA
      data2return[cur_var_name_a] = NA
    }
    
    cat(paste('--- FAILED: Missingness greater than threshold', missing_threshold, '\n'))
    return(list(estimates=data2return, file_checks=file_checks))
  } else{
    cat(paste('--- PASSED : Percent missing less than threshold\n'))
  }
  # ##############################################################################
  #
  # ##############################################################################
  # Finding all of the unique events for a given state type
  pos_affect <- .subset_by_types(behavior_data, positive)
  neg_affect <- .subset_by_types(behavior_data, negative)
  neutral_affect <- .subset_by_types(behavior_data, neutral)
  autonomy_sup <- .subset_by_types(behavior_data, autonomy_support)
  intrusive <- .subset_by_types(behavior_data, intrusiveness)
  nei <- .subset_by_types(behavior_data, neither)
  #
  # Reducing these events, either by intersection or union of events
  whole_interval <- intervals::Intervals(c(0,endtime))
  pos_states <- .find_unions(pos_affect, "POSITIVE")
  neutral_states <- .find_unions(neutral_affect, "NEUTRAL")
  neg_states <- .find_unions(neg_affect, "NEGATIVE")
  autsup_states <- .find_unions(autonomy_sup, "AUTONOMY_SUPPORT")
  intrusive_states <- .find_unions(intrusive, "INTRUSIVENESS")
  neither_states <- .find_unions(nei, "NEITHER")
  
  #9 states
  neu_nei <- .find_macrostate_affect_behavior(neutral_states, neither_states, "Neu-Nei", 9)
  neu_autsup <- .find_macrostate_affect_behavior(neutral_states, autsup_states, "Neu-Autsup", 8)
  neu_intrusive <- .find_macrostate_affect_behavior(neutral_states, intrusive_states, "Neu-Intrusive", 7)
  pos_nei <- .find_macrostate_affect_behavior(pos_states, neither_states, "Pos-Nei", 6)
  pos_autsup <- .find_macrostate_affect_behavior(pos_states, autsup_states, "Pos-Autsup", 5)
  pos_intrusive <- .find_macrostate_affect_behavior(pos_states, intrusive_states, "Pos-Intrusive", 4)
  neg_nei <- .find_macrostate_affect_behavior(neg_states, neither_states, "Neg-Nei", 3)
  neg_autsup <- .find_macrostate_affect_behavior(neg_states, autsup_states, "Neg-Autsup", 2)
  neg_intrusive <- .find_macrostate_affect_behavior(neg_states, intrusive_states, "Neg-Intrusive", 1)
  
  alltypes <- rbind(neu_nei, neu_autsup, neu_intrusive, pos_nei, pos_autsup, pos_intrusive,
                    neg_nei, neg_autsup, neg_intrusive)
  state_sequence <- alltypes[with(alltypes, order(Start)), ]
  # ##############################################################################
  #
  # ##############################################################################
  # # Entropy Calculations
  transition_counts <- CalcTransitionCounts(state_sequence[,5], 9)
  transition_matrix <- CalcTransitionMatrix(transition_counts)
  stationary_matrix <- CalcEmpiricalStationary(state_sequence[,5], 1:9)
  entropy_rate <- CalcMarkovEntropyRate(transition_matrix, stationary_matrix)
  
  numbered_states <- c()
  for(i in 1:nrow(state_sequence)){
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$Start, state_sequence[i,]$point))
    numbered_states <- rbind(numbered_states,
                             c(state_sequence[i,]$End, state_sequence[i,]$point))
  }
  
  if(plot_all == T){
    if (plots_to_file == T){
      mainDir <- path.expand(getwd())
      subDir <- "Affect_AutonomyPlots"
      if (file.exists(subDir)){
        setwd(file.path(mainDir, subDir))
      } else {
        dir.create(file.path(mainDir, subDir))
        setwd(file.path(mainDir, subDir))
      }
      pdf(paste(as.character(id_number), "_all_plots_affect_autonomy.pdf", sep=""))
      plot_file_w_affect_autonomy(endtime=endtime,
                                  pos_states=pos_states,
                                  neutral_states=neutral_states,
                                  neg_states=neg_states,
                                  autsup_states=autsup_states,
                                  intrusive_states=intrusive_states,
                                  neither_states=neither_states,
                                  numbered_states=numbered_states,
                                  id_number=id_number)
      
      plot_orig_w_affect_autonomy(endtime=endtime,
                                  pos_states=pos_states,
                                  neutral_states=neutral_states,
                                  neg_states=neg_states,
                                  autsup_states=autsup_states,
                                  intrusive_states=intrusive_states,
                                  neither_states=neither_states,
                                  numbered_states=numbered_states,
                                  id_number=id_number)
      
      plot_transformed_w_affect_autonomy(endtime=endtime,
                                         alltypes=alltypes,
                                         id_number=id_number)
      
      plot_sequence_w_affect_autonomy(state_sequence=state_sequence,
                                      id_number=id_number)
      
      plot_counts_w_affect_autonomy(transition_counts, id_number)
      
      plot_transitions_w_affect_autonomy(transition_matrix, id_number)
      dev.off()
      setwd('../')
    }
    if (plot_all == T) {
      plot_file_w_affect_autonomy(endtime=endtime,
                                  pos_states=pos_states,
                                  neutral_states=neutral_states,
                                  neg_states=neg_states,
                                  autsup_states=autsup_states,
                                  intrusive_states=intrusive_states,
                                  neither_states=neither_states,
                                  numbered_states=numbered_states,
                                  id_number=id_number)
      
      plot_orig_w_affect_autonomy(endtime=endtime,
                                  pos_states=pos_states,
                                  neutral_states=neutral_states,
                                  neg_states=neg_states,
                                  autsup_states=autsup_states,
                                  intrusive_states=intrusive_states,
                                  neither_states=neither_states,
                                  numbered_states=numbered_states,
                                  id_number=id_number)
      
      plot_transformed_w_affect_autonomy(endtime=endtime,
                                         alltypes=alltypes,
                                         id_number=id_number)
      
      plot_sequence_w_affect_autonomy(state_sequence=state_sequence,
                                      id_number=id_number)
      
      plot_counts_w_affect_autonomy(transition_counts, id_number)
      
      plot_transitions_w_affect_autonomy(transition_matrix, id_number)
    }
  }
  
  
  
  data2return <- data.frame('SubjectID'=as.character(id_number),
                            'CanEstimateEntropy'=TRUE,
                            'EntropyRate'=entropy_rate,
                            'TotalNumberOfTransitions'=sum(transition_counts),
                            'CombinedVideoDuration'=endtime,
                            'PercentMissing'=percent_missing,
                            'PositiveCounts'=nrow(pos_states),
                            'PositiveTotalTime'=sum(pos_states[,3]),
                            'PositiveAverageTime'=mean(pos_states[,3]),
                            'NeutralCounts'=nrow(neutral_states),
                            'NeutralTotalTime'=sum(neutral_states[,3]),
                            'NeutralAverageTime'=mean(neutral_states[,3]),
                            'NegativeCounts'=nrow(neg_states),
                            'NegativeTotalTime'=sum(neg_states[,3]),
                            'NegativeAverageTime'=mean(neg_states[,3]),
                            'NeitherCounts'=nrow(neither_states),
                            'NeitherTotalTime'=sum(neither_states[,3]),
                            'NeitherAverageTime'=mean(neither_states[,3]),
                            'AutonomyCounts'=nrow(autsup_states),
                            'AutonomyTotalTime'=sum(autsup_states[,3]),
                            'AutonomyAverageTime'=mean(autsup_states[,3]),
                            'IntrusivenessCounts'=nrow(intrusive_states),
                            'IntrusivenessTotalTime'=sum(intrusive_states[,3]),
                            'IntrusivenessAverageTime'=mean(intrusive_states[,3]),
                            stringsAsFactors = F)
  
  #here we are going to be adding the 27 new co-occurrence variables
  affect_states <- c('Neutral', 'Positive', 'Negative')
  all_vars_lengths <- c(nrow(neu_nei), nrow(neu_autsup), nrow(neu_intrusive),
                        nrow(pos_nei), nrow(pos_autsup), nrow(pos_intrusive),
                        nrow(neg_nei), nrow(neg_autsup), nrow(neg_intrusive))
  
  all_vars_sums <- c(sum(neu_nei$Duration), sum(neu_autsup$Duration), sum(neu_intrusive$Duration),
                     sum(pos_nei$Duration), sum(pos_autsup$Duration), sum(pos_intrusive$Duration),
                     sum(neg_nei$Duration), sum(neg_autsup$Duration), sum(neg_intrusive$Duration))
  
  all_vars_named_sec <- c('NeuNei', 'NeuAutonomySupport', 'NeuIntrusive',
                          'PosNei', 'PosAutonomySupport', 'PosIntrusive',
                          'NegNei', 'NegAutonomySupport', 'NegIntrusive')
  
  for (index in 1:9) {
    cur_var_name = all_vars_named_sec[index]
    cur_var_name_c = paste(cur_var_name, "Count", sep="")
    cur_var_name_t = paste(cur_var_name, "TotalTime", sep="")
    cur_var_name_a = paste(cur_var_name, "AverageTime", sep="")
    len = all_vars_lengths[index]
    sum_all = all_vars_sums[index]
    data2return[cur_var_name_c] = len
    data2return[cur_var_name_t] = sum_all
    if (len == 0) {
      data2return[cur_var_name_a] = 0
    } else {
      data2return[cur_var_name_a] = sum_all / len
    }
  }
  
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  cat(paste(c(rep('*', 24), ' File Completed Successfully ',rep('*', 24), '\n'), collapse = ''))
  cat(paste(c(rep('-', 78), '\n'),collapse = ''))
  return(list(estimates=data2return, file_checks=file_checks, entropy=entropy_rate))
}


#------------------- HELPER FUNCTIONS --------------------------
.subset_by_types <- function(dset, event_str, padding=0.0){
  dat <- c()
  for(i in 1:length(event_str)){
    dat <- rbind(dat, dset[(dset$Behavior == event_str[i]) &
                             ((dset$Event_Type =='State start') | (dset$Event_Type =='State point') | (dset$Event_Type =='Point')) ,
                           c("Time_Relative_sf",'Duration_sf')])
  }
  dat$Duration_sf[dat$Duration_sf == 0] <- padding
  return(dat)
}

.compare_intersection <-  function (mother, baby, statetype=""){
  babyints <- intervals::Intervals(cbind(baby[,1], baby[,1]+baby[,2]))
  momints <- intervals::Intervals(cbind(mother[,1], mother[,1]+mother[,2]))
  mombaby_intersection <- intervals::interval_intersection(babyints, momints)
  if(length(mombaby_intersection)==0){
    mombaby_intersection <- data.frame(c(0),c(0))
  }
  mombaby_intersection <- data.frame(mombaby_intersection)
  names(mombaby_intersection) <- c('Start', 'End')
  mombaby_intersection$Duration <- mombaby_intersection$End - mombaby_intersection$Start
  mombaby_intersection$State <- statetype
  return(mombaby_intersection)
}

.find_unions <- function(times, statetype=""){
  ints <- intervals::Intervals(cbind(times[,1], times[,1]+times[,2]))
  
  allunions <- intervals::interval_union(ints)
  
  if(length(allunions)==0){
    allunions <- data.frame(c(0),c(0))
  }
  allunions <- data.frame(allunions)
  names(allunions) <- c('Start', 'End')
  allunions$Duration <- allunions$End - allunions$Start
  allunions$State <- statetype
  return(allunions)
}

.state_complement<- function(whole_interval, states, statetype=""){
  state_comp <- suppressWarnings(intervals::interval_difference(whole_interval, intervals::Intervals(states[,c(1,2)])))
  if(length(state_comp)==0){
    state_comp <- data.frame(c(0),c(0))
  }
  state_comp <- data.frame(state_comp)
  names(state_comp) <- c('Start', 'End')
  state_comp$Duration <- state_comp$End - state_comp$Start
  state_comp$State <- statetype
  return(state_comp)
}

.find_macrostate <- function(look, touch, vocal, tag="", pt=0){
  look_ints <- intervals::Intervals(look[,c(1,2)])
  touch_ints <- intervals::Intervals(touch[,c(1,2)])
  vocal_ints <- intervals::Intervals(vocal[,c(1,2)])
  step1 <- intervals::interval_intersection(look_ints, touch_ints)
  step2 <- intervals::interval_intersection(vocal_ints, step1)
  if(length(step2)==0){
    triple_intersection <- data.frame(c(0),c(0))
  } else{
    triple_intersection <- data.frame(step2)
  }
  names(triple_intersection) <- c('Start', 'End')
  triple_intersection$Duration <- triple_intersection$End - triple_intersection$Start
  triple_intersection$State <- tag
  triple_intersection$point <- pt
  return(triple_intersection[triple_intersection$Duration!=0.0,])
}

.find_macrostate_new <- function(look, touch, vocal, affect, tag="", pt=0){
  look_ints <- intervals::Intervals(look[,c(1,2)])
  touch_ints <- intervals::Intervals(touch[,c(1,2)])
  vocal_ints <- intervals::Intervals(vocal[,c(1,2)])
  affect_ints <- intervals::Intervals(affect[,c(1,2)])
  step1 <- intervals::interval_intersection(look_ints, touch_ints)
  step2 <- intervals::interval_intersection(vocal_ints, step1)
  step3 <- intervals::interval_intersection(affect_ints, step2)
  if(length(step3)==0){
    triple_intersection <- data.frame(c(0),c(0))
  } else{
    triple_intersection <- data.frame(step3)
  }
  names(triple_intersection) <- c('Start', 'End')
  triple_intersection$Duration <- triple_intersection$End - triple_intersection$Start
  triple_intersection$State <- tag
  triple_intersection$point <- pt
  return(triple_intersection[triple_intersection$Duration!=0.0,])
}

.find_macrostate_affect_behavior <- function(affect, beh, tag="", pt=0){
  affect_ints <- intervals::Intervals(affect[,c(1,2)])
  autonomy_ints <- intervals::Intervals(beh[,c(1,2)])
  step1 <- intervals::interval_intersection(affect_ints, autonomy_ints)
  if(length(step1)==0){
    triple_intersection <- data.frame(c(0),c(0))
  } else{
    triple_intersection <- data.frame(step1)
  }
  names(triple_intersection) <- c('Start', 'End')
  triple_intersection$Duration <- triple_intersection$End - triple_intersection$Start
  triple_intersection$State <- tag
  triple_intersection$point <- pt
  return(triple_intersection[triple_intersection$Duration!=0.0,])
}

#Function to turn Boris file to entropy ready excel
Boris2NoldusFileReShape <- function(rel){
  #input:
  #	rel = BORIS .csv file read in through read.csv(), file that has already been read in
  #output:
  #	converted BORIS to Noldus xlsx file (saved under the current working directory)
  
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
  
}

#Function which reads in csv directory and in the same one creates a new folder to contain the translated excel files
ConvertCSVtoEXCEL <- function(csv_dir) {
  library(tidyr)
  library(tidyverse)
  setwd(path.expand(csv_dir))
  data_frame_names <- list.files(pattern = "*.csv", recursive = T) #accesses all files in subdirectories if recursive = T
  data_frame_list <- lapply(data_frame_names, read.csv) #reads in all csv files
  data_frame_output <- lapply(data_frame_list, Boris2NoldusFileReShape)
  
  #Sub folders have folder name before file name, delete
  data_frame_names <- gsub(".*/","",data_frame_names)
  #Create a character vector with all names to add to new reshaped files, delete the ".csv" part
  namesss <-substr(data_frame_names,1,nchar(data_frame_names)-4)
  data_frame_output <- setNames(data_frame_output, namesss)
  
  library(writexl)
  subDir <- "ConvertedToExcels"
  mainDir <- path.expand(getwd())
  if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create("ConvertedToExcels")
    setwd(file.path(mainDir, subDir))
  }
  #Export reshped files into new folder
  lapply(names(data_frame_output),
         function (x) write_xlsx(data_frame_output[[x]], path=paste(x, "xlsx", sep=".")))
}

################################################################################
#
# Functions related to calculating the Entropy Rate of Finite Markov Processes
#   - Author: Brian Vegetabile, University of California - Irvine
#
################################################################################

# Simulation of Finite Markov Chains -------------------------------------------

#' Simulate a Markov Chain Given a Transition Matrix
#'
#' @param trans_mat A row-stochastic matrix representing a Markov transition matrix
#' @param n_sims Number of observations for simulation length. Default of 100.
#' @return A vector of observations from a Markov chain
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
SimulateMarkovChain <- function(trans_mat, n_sims=100){
  n_states <- nrow(trans_mat)
  states <- seq(1, n_states)
  
  simulations <- matrix(0, nrow = 1, ncol = n_sims)
  
  stat_mat <- CalcEigenStationary(trans_mat = trans_mat)
  init_state <- sample(x = states,
                       size = 1,
                       replace = TRUE,
                       prob = stat_mat)
  simulations[1,1] <- init_state
  
  for (i in 2:n_sims){
    prev_step <- simulations[1,(i-1)]
    next_step <- sample(states,
                        size = 1,
                        replace = TRUE,
                        prob = trans_mat[prev_step,])
    simulations[1, i] <- next_step
  }
  return(as.vector(simulations))
}

# Functions for the Estimation of Entropy Rate----------------------------------

#' Compute a Matrix of Transition Counts from an Observed Sequence (First-Order)
#'
#' @param event_seq Vector of observations.
#' @param n_states Total number of expected unique states.
#' @return Matrix \deqn{n_{states} \times n_{states}} representing the number of transitions from each state to all other states.
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc <- CalcTransitionCounts(sim_mc)
CalcTransitionCounts <- function(event_seq, n_states=length(unique(event_seq))){
  obs_trans <- matrix(nrow = n_states, ncol = n_states, 0)
  for (t in 1:(length(event_seq) - 1)){
    obs_trans[event_seq[t],
              event_seq[t + 1]] <- obs_trans[event_seq[t], event_seq[t + 1]] + 1
  }
  return(obs_trans)
}


#' Compute a Matrix of Transition Counts from an Observed Sequence (Arbitrary Order)
#'
#' @inheritParams CalcTransitionCounts
#' @param state_space vector composed of all of the state space
#' @param mc_order order of the Markov chain.  Defaults to 1.
#'
#' @return Returns a matrix of transition counts of a given order
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, mc_order=2)
CalcTC_Mth_Order <- function(event_seq, state_space, mc_order=1){
  n_obs <- length(event_seq)
  n_states <- length(state_space)
  states <- data.frame(matrix(NA, nrow=n_states, ncol=mc_order))
  for(i in 1:mc_order){
    states[,i] <- state_space
  }
  df_args <- c(expand.grid(states), sep=":")
  vector_states <- do.call(paste, df_args)
  obs_trans <- matrix(0, nrow = length(vector_states), ncol = length(vector_states))
  rownames(obs_trans) <- vector_states
  colnames(obs_trans) <- vector_states
  
  if(mc_order > 1){
    n_new <- n_obs - (mc_order-1)
    mth_order_seq <- rep(NA, n_new)
    for(i in 1:n_new){
      mth_order_seq[i] <- paste(event_seq[i:(i+(mc_order-1))], collapse = ':')
    }
  } else {
    mth_order_seq <- event_seq
  }
  for (t in 1:(length(mth_order_seq) - 1)){
    obs_trans[mth_order_seq[t],
              mth_order_seq[t + 1]] <- obs_trans[mth_order_seq[t],
                                                 mth_order_seq[t + 1]] + 1
  }
  return(obs_trans)
}

#' Calculate the Transition Matrix of a First-Order Markov Chain
#'
#' @param trans_counts Matrix of transition counts
#' @return Row-stochastic transition matrix of a first-order Markov chain.
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' tc <- CalcTransitionCounts(sim_mc)
#' tm <- CalcTransitionMatrix(tc)
CalcTransitionMatrix <- function(trans_counts){
  mat_dim = ncol(trans_counts)
  row_totals <- rowSums(trans_counts)
  row_tot_mat <- matrix(rep(row_totals, ncol(trans_counts)),
                        nrow=mat_dim, ncol=mat_dim)
  trans_mat <- trans_counts / row_tot_mat
  trans_mat[trans_mat=="NaN"] <- 0
  return(trans_mat)
}


#' Calculate the Stationary Distribution of a First-Order Markov Chain from its
#' Transition Matrix
#'
#' @param trans_mat Transition matrix
#' @return vector representing the stationary distribution of the Markov chain
#' @examples
#' tm1 <- matrix(c(0,0,1,1,0,0,0,1,0), 3,3, T)
#' sm1 <- CalcEigenStationary(tm1)
#' tm2 <- matrix(c(.3, 0.7, 0.8, 0.2), 2,2, T)
#' sm2 <- CalcEigenStationary(tm2)
CalcEigenStationary <- function(trans_mat){
  tm_eig <- eigen(t(trans_mat))
  if(any(round(Mod(tm_eig$values),10)==1)){
    lamb1 <- which(abs(tm_eig$values-1) == min(abs(tm_eig$values-1)))
    stat_vec <- tm_eig$vectors[,lamb1] / sum(tm_eig$vectors[,lamb1])
    return(Re(stat_vec))
  } else{
    stat_vec <- rep(0, nrow(trans_mat))
    return(stat_vec)
  }
}


#' Calculation of the Empirical Stationary Distribution from an observed Markov Chain (first-order)
#'
#' @param event_seq Observed sequence of events as a Markov chain
#' @param state_space State space of the observed Markov chain
#'
#' @return Estimate of the stationary distribution
#'
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 500)
#' true_sm <- CalcEigenStationary(t_mat)
#' est_sm <- CalcEmpiricalStationary(sim_mc, 1:2)
CalcEmpiricalStationary <- function(event_seq, state_space){
  emp_stat <- matrix(0, nrow = 1, ncol = length(state_space))
  for(i in 1:length(state_space)){
    emp_stat[1,i] <- length(event_seq[event_seq == state_space[i]]) / length(event_seq)
  }
  return(emp_stat)
}

#' Calculation of the Empirical Stationary Distribution from an observed Markov Chain (Arbitrary-Order)
#'
#' @param event_seq Observed sequence of events as a Markov chain
#' @param state_space State space of the observed Markov chain
#' @param mc_order Order of the Markov Chain
#'
#' @return Estimate of the stationary distribution
#'
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 10000)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, 2)
#' tm2 <- CalcTransitionMatrix(tc2)
#' eig_sm <- CalcEigenStationary(tm2)
#' emp_sm <- CalcEmpStat_Mth_Order(sim_mc, 1:2, 2)
#' print(eig_sm)
#' print(emp_sm)
CalcEmpStat_Mth_Order <- function(event_seq, state_space, mc_order=1){
  n_obs <- length(event_seq)
  n_states <- length(state_space)
  states <- data.frame(matrix(NA, nrow=n_states, ncol=mc_order))
  for(i in 1:mc_order){
    states[,i] <- state_space
  }
  df_args <- c(expand.grid(states), sep=":")
  vector_states <- do.call(paste, df_args)
  
  emp_stat <- matrix(0, nrow = 1, ncol = length(vector_states))
  colnames(emp_stat) <- vector_states
  
  if(mc_order > 1){
    n_new <- n_obs - (mc_order-1)
    mth_order_seq <- rep(NA, n_new)
    for(i in 1:n_new){
      mth_order_seq[i] <- paste(event_seq[i:(i+(mc_order-1))], collapse = ':')
    }
  } else {
    mth_order_seq <- event_seq
  }
  for(i in 1:length(vector_states)){
    emp_stat[1,i] <- length(mth_order_seq[mth_order_seq == vector_states[i]]) / length(mth_order_seq)
  }
  return(emp_stat)
}

#' Calculate an estimate of the Entropy Rate of a finite Markov Chain
#'
#' @param trans_mar Transition Matrix of a finite Markov chain
#' @param stat_dist Vector of the stationary distribution of the Markov chain
#'
#' @return Estimate of the Entropy Rate
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 10000)
#' tc2 <- CalcTC_Mth_Order(sim_mc, 1:2, 2)
#' tm2 <- CalcTransitionMatrix(tc2)
#' eig_sm <- CalcEigenStationary(tm2)
#' emp_sm <- CalcEmpStat_Mth_Order(sim_mc, 1:2, 2)
#' CalcMarkovEntropyRate(t_mat, CalcEigenStationary(t_mat))
#' CalcMarkovEntropyRate(tm2, eig_sm)
#' CalcMarkovEntropyRate(tm2, emp_sm)
CalcMarkovEntropyRate <- function(trans_mat, stat_dist){
  n_dim <- length(stat_dist)
  stat_mat <- matrix(rep(stat_dist, n_dim), n_dim, n_dim, byrow=TRUE)
  ent_rate <- -sum(t(stat_mat) * trans_mat * log2(trans_mat), na.rm = TRUE)
  return(ent_rate)
}

#' Calculate the Entropy Rate of a Finite Markov Chain
#'
#' @param event_seq Observed sequence of events
#' @param state_space State space of the observed process
#' @param mc_order Order of the Markov Chain
#' @param stat_method Method for computing the Stationary distribution
#'
#' @return Estimate of the Entropy Rate
#' @examples
#' t_mat <- matrix(c(0.3, 0.7, 0.6, 0.4), 2,2, T)
#' sim_mc <- SimulateMarkovChain(t_mat, n_sims = 50000)
#' CalcMarkovEntropyRate(t_mat, CalcEigenStationary(t_mat))
#' CalcEntropyRate(sim_mc, 1:2, mc_order = 1, stat_method="Empirical")
#' CalcEntropyRate(sim_mc, 1:2, mc_order = 1, stat_method="Eigen")
CalcEntropyRate <- function(event_seq,
                            state_space,
                            mc_order = 1,
                            stat_method='Empirical'){
  tc <- CalcTC_Mth_Order(event_seq, state_space, mc_order)
  tm <- CalcTransitionMatrix(tc)
  if(stat_method != 'Empirical'){
    sm <- CalcEigenStationary(tm)
  } else{
    sm <- CalcEmpStat_Mth_Order(event_seq, state_space, mc_order)
  }
  ent <- CalcMarkovEntropyRate(tm, sm)
  return(ent)
}
