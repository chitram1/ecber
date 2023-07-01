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
  #Export reshaped files into new folder
  lapply(names(data_frame_output),
         function (x) writexl::write_xlsx(data_frame_output[[x]], path=paste(x, "xlsx", sep=".")))
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
  return(rel.6)
}
