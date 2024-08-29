################################################################################
################################################################################
############ Creating the different summary data frames    #####################
############ Zelda Brufal                                  #####################
############ JULY 2024                                     #####################
################################################################################
################################################################################



################################################################################
############ Creating the app-session level data frame     #####################
################################################################################



app_session_df <- processed_data_7_days %>%
  dplyr::filter(!is.na(app_session_id)) %>%
  dplyr::group_by(app_session_id) %>%
  dplyr::summarise(ppid = first(ppid),
            day_no = first(day_no),
            App = first(App),
            session_id = first(session_id),
            app_session_start = min(date_and_time),
            app_session_end = max(date_and_time),
            app_session_duration = as.numeric(difftime(app_session_end, app_session_start, units = "secs")),
            order_ranking_in_session = first(app_session_order_rank))%>%
  ungroup()



################################################################################
############ Creating session level data frame     #############################
################################################################################



session_df <- processed_data_7_days %>%
  dplyr::group_by(session_id) %>%
  dplyr::summarise(ppid = first(ppid),
            day_no = first(day_no),
            session_id = first(session_id),
            session_start = min(date_and_time),
            session_end = max(date_and_time),
            session_duration = as.numeric(difftime(session_end, session_start, units = "secs")),
            no_app_sessions = n_distinct(app_session_id),
            first_app = App[which(app_session_order_rank == 1)[1]],
            second_app = App[which(app_session_order_rank == 2)[1]], .groups = 'drop')%>%
  ungroup()




################################################################################
############ Creating participant level data frame     #########################
################################################################################

# getting new list of participant ids

ppids_for_participant_level_data_frame <- unique(processed_data_7_days$ppid)

# actually creating the participant level data frame


participant_level_data_frame <- data.frame(ppids_for_participant_level_data_frame)

# naming the ppid column "ppid" 

participant_level_data_frame <- participant_level_data_frame %>%
  dplyr::rename(ppid = ppids_for_participant_level_data_frame)


### putting all ppid id's into lower case so it is actually possible to merge data frames

participant_level_data_frame$ppid <- tolower(participant_level_data_frame$ppid)
processed_mh_data$ppid <- tolower(processed_mh_data$ppid)

# Creating the participant level data frame 

participant_level_data_frame <- merge(participant_level_data_frame, processed_mh_data, by = 'ppid')



if (nrow(participant_level_data_frame) != n_distinct(participant_folders)){
  stop("STOP: The number of partiicpants put in the participant level data frame does not match the number different
       participant folders / no. participant mh surverys. This is normally because someone forogt to 
       enter their id in the questionnaire. ")}






# Selecting the columns that are useful 

participant_level_data_frame <- participant_level_data_frame %>%
  dplyr::select(ppid,
         gender,
         most_automatic_app_self_reported,
         least_automatic_app_self_reported,
         SRHI_phone_use_score,
         SRHI_social_media_use_score,
         SRHI_most_automatic_app_score,
         SRHI_least_automatic_app_score,
         OCI_R_score,
         GAD_7_score, 
         PHQ_8_score,
         BIS_scale_score,
         BAS_fun_seeking_score,
         BAS_drive_score,
         BAS_reward_responsiveness_score,
         ATQ_short_score
  )


