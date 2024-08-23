################################################################################
################################################################################
##################  MAIN SCRIPT FOR ITACO DATA CLEANING ########################
##################  Zelda Brufal, July 2024             ########################
##################  Zelda@live.co.uk                    ########################
################################################################################
################################################################################

################## SESSION INFORMATION #########################################

# R version 4.3.3 (2024-02-29)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.2.1

################## Setting up environment  #####################################

install.packages('dplyr')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('stringr')
install.packages("here")


library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(here)






################################################################################
############     setting up the directory correctly ############################
################################################################################


#### moving out of folder that is uploaded to git hub to where the data is 

here_dir <- here()
parent_dir <- dirname(here_dir)
setwd(parent_dir)

### creating directory to data 

data_directory <- stringi::stri_join(parent_dir, "usable_participant_data", "", sep = "/")



################################################################################
##################  LOADING IN AND PROCESSING ITACO DATA.          #############
################################################################################


code_script_function <- stringi::stri_join(here::here(), "Code_scripts", "", sep = "/")

source(paste0(here_dir,"/", "final_processing_itaco.R"))



################################################################################
##################  LOADING IN AND PROCESSING MENTAL HEALTH  DATA  #############
################################################################################


#Set working directory for the screener survey data 


project_directory <- here()
setwd(project_directory)


# Read in the file with the mental health data


mh_data_raw <- read.csv("CICESE- Main - Georgia owned_August 14, 2024_02.57.csv")

source("/Users/zeldabrufal/iTACO/Code_scripts/mental_health_code.R")

source(paste0(code_script_function, "Mental health code.R"))

################################################################################
##################  CREATING THE DIFFERENT LEVEL DATA FRAMES        ############
################################################################################



source(paste0(code_script_function, "iTACO_summary_data_frames_code.R"))



