##################################################################################
##################################################################################
############        Mental health questionnaire coding      ######################
##################################################################################
##################################################################################

############## LOADING IN THE DATA. ###############################################


setwd("/Users/zeldabrufal/iTACO")
mh_data_raw <- read.csv("CICESE- Main - Georgia owned_August 14, 2024_02.57.csv")
list.files()


##################################################################################
############        Renaming columns                        ######################
##################################################################################


mh_data <- mh_data_raw %>%
  dplyr::rename('ppid' = 'Q24')%>% # naming the ppid column
  dplyr::rename('gender' = 'Q25.1')%>% # naming gender column 
  dplyr::rename('most_automatic_app_self_reported' = 'Q23')%>% # naming most automatic app (self report) column
  dplyr::rename('least_automatic_app_self_reported' = 'Q24.1')%>% # naming least automatic app (self report) column
  dplyr::rename_with(~ gsub("Q8_", "PHQ8_", .))%>% # renaming PHQ-8 
  dplyr::rename_with(~ gsub("Q9_", "GAD_7_", .))%>% # renaming GAD_7
  dplyr::rename_with(~ gsub("Q13_", "OCI_R_", .)) %>%# renaming 0CI
  dplyr::rename_with(~ gsub("Q4_11", "attention_check_1", .)) %>% # identifying and labelling attention check q1
  dplyr::rename_with(~ gsub("Q4_13", "attention_check_2", .)) %>% # identifying and labelling attention check q2
  dplyr::rename_with(~ gsub("Q4_", "BIS/BAS_", .)) %>% # renaming BIS/BAS
  dplyr::rename_with(~ gsub("Q23_", "ATQ_short_", .)) %>% # renaming ATQ
  dplyr::rename_with(~ gsub("SRHI_", "SRHI_phone_use_", .)) %>% # renaming SRHI (on phone use)
  dplyr::rename_with(~ gsub("Q19_", "SRHI_social_media_use_", .)) %>% # renaming SRHI (on social media use)
  dplyr::rename_with(~ gsub("Q20_", "SRHI_most_automatic_app_", .)) %>% # renaming SRHI (on their self report most automatically used app)
  dplyr::rename_with(~ gsub("Q22_", "SRHI_least_automatic_app_", .)) # renaming SRHI (on their self report least automatically used app)
 


########################################################################################
########### Removing incompleted questionnaire data ####################################
########################################################################################   
    
mh_data <- mh_data %>%
  filter(Finished != 'False')


   
########################################################################################
###########.  giving the correct names to the BIS/BAS questions  #######################
############(as they got affected by the attention checking questions###################
########################################################################################

mh_data_col_names <- names(mh_data)
BISBAS_columns <- grep("BIS/BAS", mh_data_col_names, value = TRUE)


replace_BISBAS_names <- function(mh_data, BISBAS_columns) {
  current_names <- names(mh_data)
  name_counter <- 1
  new_names <- current_names
  for (i in seq_along(current_names)) {
    if (current_names[i] %in% BISBAS_columns) {
      new_names[i] <- paste0("BIS/BAS_", name_counter)
      name_counter <- name_counter + 1
    }
  }
  
  names(mh_data) <- new_names
  return(mh_data)
}


mh_data <- replace_BISBAS_names(mh_data, BISBAS_columns)
             
# identifying and renaming which bisbas questions need to be reversed 
         
mh_data <- mh_data %>%
  dplyr::rename("BIS/BAS_2_reverse" = "BIS/BAS_2") %>%
  dplyr::rename("BIS/BAS_22_reverse"=  "BIS/BAS_22")





##################################################################################
##################################################################################
############        SCORING THE SCREENERS                        #################
##################################################################################
##################################################################################




##################################################################################
#########################        PHQ-8          ####################################
##################################################################################

recode.rules.PHQ  <- function(x) {
  x <- gsub('Not at all', 0, x)
  x <- gsub('Several days', 1, x)
  x <- gsub('More than half the days', 2, x)
  x <- gsub('Nearly every day', 3, x)
  return(x)
}


PHQ_columns <- grep('PHQ', names(mh_data), value = TRUE)
mh_data[PHQ_columns] <- lapply(mh_data[PHQ_columns], recode.rules.PHQ)




##################################################################################
#########################        GAD-7         ####################################
##################################################################################

recode.rules.GAD  <- function(x) {
  x <- gsub('not at all', 0, x)
  x <- gsub('several days', 1, x)
  x <- gsub('more than half the days', 2, x)
  x <- gsub('nearly every day', 3, x)
  return(x)
}


GAD_columns <- grep('GAD', names(mh_data), value = TRUE)
mh_data[GAD_columns] <- lapply(mh_data[GAD_columns], recode.rules.GAD)



##################################################################################
#########################        OCI-R      ####################################
##################################################################################

 # Link to website used for scoring info : https://greenspacehealth.com/en-us/obsessive-compulsive-oci-r/#:~:text=Scores%20on%20the%20OCI%2DR,the%20likely%20presence%20of%20OCD.


recode.rules.OCI  <- function(x) {
  x <- gsub('Not at all', 0, x)
  x <- gsub('A little', 1, x)
  x <- gsub('Moderately', 2, x)
  x <- gsub('A lot', 3, x)
  x <- gsub('Extremely', 4, x)
  return(x)
}


OCI_columns <- grep('OCI', names(mh_data), value = TRUE)
mh_data[OCI_columns] <- lapply(mh_data[OCI_columns], recode.rules.OCI)




##################################################################################
#########################        BIS/BAS      ####################################
##################################################################################
# Link to website used for scoring info: https://www.psy.miami.edu/faculty/ccarver/bisbas.html

####### BIS/BAS INFO 

##### Q1, Q6, Q11, Q17 ARE FILLERS 
####### Q2 AND Q22 ARE REVERSE SCORED
########## BIS SCALE =  Q 2, 8, 13, 16, 19, 22, 24
###########.BAS DRIVE = 3, 9, 12, 21
############# BAS FUN SEEKING = 5, 10, 15, 20
############# BAS  REWARD RESPONSIVENESS = 4, 7, 14, 18, 23



recode.rules.BISBAS_normal  <- function(x) {
  x <- gsub('Very true for me', 1, x)
  x <- gsub( 'Somewhat true for me', 2, x)
  x <- gsub('Somewhat false for me', 3, x)
  x <- gsub('Very false for me', 4, x)
  return(x)
}


recode.rules.BISBAS_reverse  <- function(x) {
  x <- gsub('Very true for me', 4, x)
  x <- gsub( 'Somewhat true for me', 3, x)
  x <- gsub('Somewhat false for me', 2, x)
  x <- gsub('Very false for me', 1, x)
  return(x)
}




# specifying all bisbas columns

BISBAS_columns <- grep('BIS/BAS', names(mh_data), value = TRUE)

# specifying bis bas reverse columns 
BISBAS_reverse_columns <- grep('reverse', BISBAS_columns, value = TRUE)

# excluding reverse columns to specify the normal columns

BISBAS_normal_columns <- dplyr::setdiff(BISBAS_columns, BISBAS_reverse_columns)


# now recoding BIS BAS 


mh_data[BISBAS_normal_columns] <- lapply(mh_data[BISBAS_normal_columns], recode.rules.BISBAS_normal)
mh_data[BISBAS_reverse_columns] <- lapply(mh_data[BISBAS_reverse_columns], recode.rules.BISBAS_reverse)


# Naming the column names of the different BIS and BAS scale scores 

BIS_scale_columns <- c('BIS/BAS_2_reverse', 'BIS/BAS_8','BIS/BAS_13', 'BIS/BAS_16', 'BIS/BAS_19','BIS/BAS_22_reverse', 'BIS/BAS_24')
BAS_drive_columns <- c('BIS/BAS_3', 'BIS/BAS_9', 'BIS/BAS_12', 'BIS/BAS_21')
BAS_fun_seeking_columns <- c('BIS/BAS_5', 'BIS/BAS_10', 'BIS/BAS_15', 'BIS/BAS_20')
BAS_fun_seeking_columns <- c('BIS/BAS_5', 'BIS/BAS_10', 'BIS/BAS_15', 'BIS/BAS_20')
BAS_reward_responsiveness_columns <- c('BIS/BAS_4', 'BIS/BAS_7', 'BIS/BAS_14', 'BIS/BAS_18', 'BIS/BAS_23')




####################################################################################
############## The ATTC / ATQ SHORT FORM  ##########################################
####################################################################################

# link to website used for info about questions and scoring :
#. https://arc.psych.wisc.edu/self-report/attention-control-scale-attc/
# NOTE: We are using the short form. The only way to establish which questions are
# reverse coded is to cross reference between the short version and the normal version information on this website. 
# From doing this, in our short form version, q 1,2,3,5,6,8 are reverse coded.



#### Labelling the columns that need to be reverse coded



mh_data <- mh_data %>%
  dplyr::rename("ATQ_short_1_reverse" = "ATQ_short_1") %>%
  dplyr::rename("ATQ_short_2_reverse" = "ATQ_short_2") %>%
  dplyr::rename("ATQ_short_3_reverse" = "ATQ_short_3") %>%
  dplyr::rename("ATQ_short_5_reverse" = "ATQ_short_5") %>%
  dplyr::rename("ATQ_short_6_reverse" = "ATQ_short_6") %>%
  dplyr::rename("ATQ_short_8_reverse" = "ATQ_short_8") 

### grouping the ATQ columns into reverse and normal 


ATQ_columns_all  <- grep('ATQ', names(mh_data), value = TRUE)

# specifying ATQ reverse columns 
ATQ_reverse_columns <- grep('reverse', ATQ_columns_all, value = TRUE)


# excluding reverse columns to specify normal

ATQ_normal_columns <- dplyr::setdiff(ATQ_columns_all, ATQ_reverse_columns)
  

############# Specifying how the ATQ columns should be coded ######################

recode.rules.ATQ_short_normal  <- function(x) {
  x <- gsub('Almost never', 1, x)
  x <- gsub( 'Sometimes', 2, x)
  x <- gsub('Often', 3, x)
  x <- gsub('Always', 4, x)
  return(x)
}



recode.rules.ATQ_short_reverse  <- function(x) {
  x <- gsub('Almost never', 4, x)
  x <- gsub( 'Sometimes', 3, x)
  x <- gsub('Often', 2, x)
  x <- gsub('Always', 1, x)
  return(x)
}



########### Recoding THE ATQ_SHORT columns 


mh_data[ATQ_normal_columns] <- lapply(mh_data[ATQ_normal_columns], recode.rules.ATQ_short_normal)
mh_data[ATQ_reverse_columns] <- lapply(mh_data[ATQ_reverse_columns], recode.rules.ATQ_short_reverse)






##################################################################################
#################       THE 4 DIFFERENT SHRI            ##########################
##################################################################################


# link to page used to code responses  (not perfect because it is in spanish and uses never to always, but essentially scoring strongly disagree to strongly agree as 1 -5 )
#  https://scielosp.org/article/ssm/content/raw/?resource_ssm_path=/media/assets/resp/v85n4/05_original4.pdf



## specifying the different SHRI columns 

# all SRHI columns
SRHI_columns <- grep('SRHI', names(mh_data), value = TRUE)

# About the speicfic item

SRHI_phone_use_columns <- grep('SRHI_phone_use', names(mh_data), value = TRUE)
SRHI_social_media_use_columns <- grep('SRHI_social_media_use', names(mh_data), value = TRUE)
SRHI_most_automatic_app_columns <- grep('SRHI_most', names(mh_data), value = TRUE)
SRHI_least_automatic_app_columns <- grep('SRHI_least', names(mh_data), value = TRUE)
# Recode rules 


recode.rules.SRHI  <- function(x) {
  x <- gsub('Strongly disagree', 1, x)
  x <- gsub( 'Somwhat disagree', 2, x) # THERE IS A TYPO IN THE QUESTIONNAIRE SO...
  x <- gsub('Neither agree nor disagree', 3, x)
  x <- gsub('Somewhat agree', 4, x)
  x <- gsub('Strongly agree', 5, x)
  return(x)
}



mh_data[SRHI_columns] <- lapply(mh_data[SRHI_columns], recode.rules.SRHI)



##################################################################################
############     Creating scores for each questionnaire    ######################
##################################################################################






mh_data2 <- mh_data %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>%
  dplyr::slice(3:n()) %>% ## Getting rid of the top two rows that are info about what the question was 
  dplyr::mutate(across(all_of(PHQ_columns), as.numeric)) %>% # making the numbers numeric
  dplyr::mutate(across(all_of(GAD_columns), as.numeric)) %>%
  dplyr::mutate(across(all_of(OCI_columns), as.numeric)) %>%
  dplyr::mutate(across(all_of(BISBAS_columns), as.numeric))%>%
  dplyr::mutate(across(all_of(ATQ_columns_all), as.numeric))%>%
  dplyr::mutate(across(all_of(SRHI_columns), as.numeric))%>%
  dplyr::mutate(PHQ_8_score = rowSums(select(., all_of(PHQ_columns)), na.rm = TRUE)) %>% # Where the actual recoding starts
  dplyr::mutate(GAD_7_score = rowSums(select(., all_of(GAD_columns)), na.rm = TRUE))%>%
  dplyr::mutate(OCI_R_score = rowSums(select(., all_of(OCI_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BIS_scale_score = rowSums(select(., all_of(BIS_scale_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_fun_seeking_score = rowSums(select(., all_of(BAS_fun_seeking_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_drive_score = rowSums(select(., all_of(BAS_drive_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_reward_responsiveness_score = rowSums(select(., all_of(BAS_reward_responsiveness_columns)), na.rm = TRUE))%>%
  dplyr::mutate(ATQ_short_score = rowSums(select(., all_of(ATQ_columns_all)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_phone_use_score = rowSums(select(., all_of(SRHI_phone_use_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_social_media_use_score = rowSums(select(., all_of(SRHI_social_media_use_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_most_automatic_app_score = rowSums(select(., all_of(SRHI_most_automatic_app_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_least_automatic_app_score = rowSums(select(., all_of(SRHI_least_automatic_app_columns)), na.rm = TRUE))
  
  
  


################################################################################
################# MANUALLY ADDING IN Z83 AS THEY ###############################
################# FORGOT TO PUT IN THEIR PPID    ###############################
################################################################################


mh_data2$ppid[mh_data2$RecordedDate == '2024-07-26 08:13:36'] <- 'z83'



################################################################################
########### Only keeping the ppids we also have phone data for #################
########### AND RENAMING MH DATA                               #################
################################################################################


### only keeping ppids that we also have phone data for 

processed_mh_data <- mh_data2 %>%
  dplyr::filter(str_to_lower(ppid) %in% str_to_lower(ppids_for_participant_level_data_frame))




rm(mh_data_raw)


