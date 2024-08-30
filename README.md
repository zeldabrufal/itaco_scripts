# ITACO AND QUESTIONNAIRE PROCESSING SCRIPTS

To make these scripts work, your directory needs to be set up as follows:

```{r}
PARENT_FOLDER
├── CICESE- Main - "".csv. # put the most up to date version of post data collection survey
├── itaco_scripts 
      ├── README.md
      ├── final_main_itaco.R
      ├── final_mental_health_code.R
      ├── final_processing_itaco.R
      ├── iTACO_summary_data_frames_code.R
      └── itaco_scripts.Rproj
└── usable_participant_data #containing the folders for each individual participants iTACO data 

```

## final_main_script.R

This script calls the other scripts. If you run it it should load in and process all the iTACO data, then the mental health questionnaire data, and then create three different data frames all reflecting the data on different levels (e.g., the app-session level, the session level, and then the participant level). These data frames can all be added to depending on specific research questions you are looking at.

## final_processing_itaco.R

This script first loops through every participant folder in the usable_participant_data folder, and loads in every txt.file. It then gets rid of non user initiated events and processes all the rows into columns, and loads this processed data into the data frame called "combined_data_frame". This data frame is the most unprocessed version of the data (i.e., only contains the raw variables from the txt files: PPID, event, node info, app, date and time)

It then creates a more processed data frame of all the participant data, called **processed_data_7_days**. Details about this can be found in the data dictionary, but broadly: - It only contains 7 days of data for each participant - Contains all the created variables on the lowest level (i.e., sessions, app-sessions, rank order)

## final_mental_health_code.R

This script loads in the post data questionnaire data. It then:

-   relabels the columns to reflect what question/ questionnaire is actually being asked.

-   Recodes the answers to score all the clinical questionnaires numerically

-   Creates overall scores for each of the questionnaires

-   adds in missing information to the data frame (i.e., for people who forgot to enter their id)

-   removes incomplete rows / participants we don't have data for.

    This script retains data frames from various different stages of this process in case things need to be altered, but the final, 'complete' version of the mental health data is called **processed_mh_data**

## iTACO_summary_data_frames_code.R

This script creates three additional data frames that reflect the different levels you might wish to look at the data at. More information about each of these data frames can be found in the data dictionary
