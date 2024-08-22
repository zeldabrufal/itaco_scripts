################################################################################
############# LOADING IN AND PROCESSING ITACO DATA                ##############
############# August 2024.                                        ##############
############# Zelda Brufal                                        ##############
################################################################################


############# Defining some variables for later ################################

Week_in_seconds <- 604800
Day_in_seconds <- 86400



############# Getting to the right folder in the directory #######33############

data_directory <- stringi::stri_join(here::here(), "usable_participant_data", "", sep = "/")


#  Creating a list of all the participant folders and also creating list of ppids

participant_folders <- list.dirs(path = (data_directory), recursive = FALSE, full.names = TRUE)
ppids <- paste0(list.dirs(path = (data_directory), recursive = FALSE, full.names = FALSE), ".") # Adding a dot so the multiple files from the same folder (ppid) are correctly labelled
ppids_for_participant_level_data_frame <- list.dirs(path = (data_directory), recursive = FALSE, full.names = FALSE)
################################################################################
#############   Making the data frame that all the inidivudal  #################
#############   file data frames will be going into            #################
################################################################################


combined_data_frame <- data.frame()

################################################################################
#############   This is the loop that read in all the files as  ################
#############   data frames for each participant, joins them    ################
#############.  togehter and then does some basic processing    ################
################################################################################




for (folder in participant_folders) {
  
  print(paste("Processing folder:", folder))
  
  # List all text files in the current folder
  files <- list.files(path = folder, pattern = '*.txt', recursive = TRUE, full.names = TRUE)
  
  for (file_path in files) {
    print(paste("Processing file:", file_path))
    
    # Read the file
    reading_files <- readLines(file_path, encoding = "UTF-8")
    
    # Read the data into a data frame
    data_frame <- read.table(textConnection(reading_files), header = FALSE, sep = ";", fill = TRUE, col.names = paste(1:6))
    
    # Add ppid column
    data_frame$Source <- basename(folder)
    
    # processing incl. removing non user initiaated events
    data_frame <- data_frame %>%
      dplyr::select(-X1, -X6) %>%
      dplyr::rename(Event = X2,
                    Date_and_Time = X3,
                    Node_info = X4,
                    App = X5) %>%
      dplyr::mutate(ppid = str_extract(Source, "^[^.]+")) %>%
      dplyr::select(-Source) %>%
      dplyr::select(ppid, Event, Date_and_Time, Node_info, App) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!grepl("TYPE_WINDOWS_CHANGED|TYPE_WINDOW_STATE_CHANGED", Event)) %>% # this row removed the non uesr initiated events
      dplyr::mutate(across(everything(), as.character)) %>%
      dplyr::mutate(date_and_time = lubridate::ymd_hms(Date_and_Time, tz = "GMT", quiet = FALSE)) %>% # this row formats the date and time into POSIXct
      dplyr::select(-Date_and_Time)
    
    # Verify date_and_time column
    if (!"POSIXct" %in% class(data_frame$date_and_time)) {
      stop("STOP: The date_and_time column is not of class POSIXct.")
    }
    
    if (!all(sapply(data_frame$date_and_time, inherits, what = "POSIXct"))) {
      stop("STOP: Not all elements in the date_and_time column are of class POSIXct.")
    }
    
 

    
    
    # put into the combined data frames
    combined_data_frame <- dplyr::bind_rows(combined_data_frame, data_frame)
    
    
    # Remove the individual data frame to free up memory (makes a lot quicker)
    rm(data_frame)
  }
}


#############################################################################

#######


embedded_null_lines <- grepl("nul", combined_data_frame, fixed = TRUE)



failed_time_parsing_rows <- combined_data_frame  %>%
  filter(is.na(date_and_time))



combined_data_frame <- combined_data_frame %>%
  filter(!is.na(date_and_time))

################################################################################
###### Arranging by date and time and then only keeping 7 full days of data ####
################################################################################



processed_data_7_days <- combined_data_frame %>%
  dplyr::group_by(ppid) %>%
  dplyr::arrange(date_and_time, .by_group = TRUE) %>%
  dplyr::mutate(last_time = max(date_and_time),
                time_diff_secs = as.numeric(difftime(last_time, date_and_time, units = "secs")),
                day_no = 7 - floor(time_diff_secs / Day_in_seconds)) %>%
  dplyr::filter(date_and_time >= last_time - Week_in_seconds) %>%
  dplyr::select(-last_time, -time_diff_secs)
  





################################################################################
###### CHECKING TO SEE IF PARTICIPANTS HAVE DAY FILES MISSING ##################
################################################################################



ppids_with_real_life_days_missing<- processed_data_7_days %>%
  group_by(ppid) %>%
  mutate(date_diff = difftime(date_and_time, lag(date_and_time, default = first(date_and_time)), units = "days")) %>%
  filter(date_diff > 1) %>%
  ungroup() %>%
  select(ppid) %>%
  distinct()




################################################################################
###### FINDING PARTICIPANTS W ##################
################################################################################





################################################################################
##### Removing the space before some of the apps ###############################
################################################################################

processed_data_7_days$App <- str_trim(processed_data_7_days$App)


################################################################################
####### Assigning session IDs     #############################################
################################################################################


# THis now means that if a session runs over the start of a new day, it is still counted as the same session. This will 
# affect things like the individual daily screen time because it will be counting screen time from a different day. 
# It was decided to do this because only a tiny number of sessions spanned over a day change. Some of these were indications that 
# there was missing data 
# Add this code : | day_no != lag(day_no, default = first(day_no)) to the function below to change the definition of a session
# so that if a session runs over a day it splits it in two (to retain accurate daily screen time)

processed_data_7_days <- processed_data_7_days %>%
  dplyr::ungroup()%>%
  dplyr::mutate(session_id = cumsum(Event == "TYPE_SCREEN_Mode_on" | ppid != lag(ppid, default = first(ppid))))




################################################################################
####### Assigning app-session IDs     ##########################################
################################################################################

######### !!!!!!! the blank app thing is a problem that need to be resolved still

# Sorting out how to handle it when there is nothing in the App column 

# rewriting "blank" in the gaps 

processed_data_7_days <- processed_data_7_days %>%
  dplyr::mutate(App = ifelse(Node_info == "null" & App == "", "blank", App))



## Changing it so that screen on and screen off events are "no app" under the app column



processed_data_7_days <- processed_data_7_days %>%
  dplyr::mutate(App = ifelse(Node_info == "0", "no_app", App))

## Now assigning app-session ids. AT THE MOMENT THIS DOESN'T IGNORE BLANK COLUMNS, SO THERE WILL BE MORE SUB-SESSIONS THAN THEIR SHOULD BE AT THE MOMENT for participants where this was an issue,  

processed_data_7_days <- processed_data_7_days %>%
  dplyr::ungroup() %>%
  dplyr::mutate(app_session_id = cumsum(App != lag(App, default = first(App)) | 
                                          ppid != lag(ppid, default = first(ppid)) | 
                                          session_id != lag(session_id, default = first(session_id)))) 


processed_data_7_days <- processed_data_7_days%>%
  dplyr::ungroup() %>%
  dplyr::mutate(app_session_id = ifelse(App == "no_app", NA, app_session_id))


####### Ranking each app-session in terms of order within a session 



processed_data_7_days <- processed_data_7_days %>%
  dplyr::group_by(session_id) %>%
  dplyr::mutate(app_session_order_rank = dense_rank(app_session_id))

