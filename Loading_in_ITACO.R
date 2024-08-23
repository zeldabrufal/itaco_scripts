################################################################################
################################################################################
############ Loading in the iTACO Data                     #####################
############ Zelda Brufal                                  #####################
############ JULY- AUGUST 2024                             #####################
################################################################################
################################################################################

################################################################################
########   Identifying participant folders (i.e. by ppid) ######################
################################################################################


participant_folders <- list.dirs(path = (data_directory), recursive = FALSE, full.names = TRUE)


ppids <- paste0(list.dirs(path = (data_directory), recursive = FALSE, full.names = FALSE), ".") # Adding a dot so the multiple files from the same folder (ppid) are correctly labelled


## creating the file path list 

file_paths_list <- lapply(participant_folders, function(dir_path) {
  list.files(path = dir_path, pattern = '*.txt', recursive = TRUE, full.names = TRUE)
})


###### naming the files by folder (i.e.their ppid) and creating big list #######

names(file_paths_list) <- ppids

raw_file_paths <- unlist(file_paths_list, recursive = TRUE)



#### creating a function to read the file 

read_file <- function(file_path) {
  readLines(file_path, encoding = "UTF-8")
}

##### using this function to read in all the files

data_list <- lapply(raw_file_paths, read_file)

################################################################################
############## READING THE DATA INTO DATA FRAMES ###############################
################################################################################


###### turning them into data frames with six columns 

make_df <- function(file_path) {
  read.table(textConnection(file_path), header = FALSE, sep = ";", fill = TRUE, col.names = paste(1:6)) ##### forcing 6 columns otherwise it is all out of sync
}

data_frame_list <- lapply(data_list, make_df)

################################################################################
######## Combining data frames and adding an identifier column #################
################################################################################

data_frame_list <- lapply(data_frame_list, function(df) {
  # Convert each column to character
  df[] <- lapply(df, as.character)
  return(df)
})

# Bind rows with .id 

rm(data_list)
Data <- dplyr::bind_rows(data_frame_list, .id = "Source")


# Checking to see if there are any duplicated rows 

Duplicates <- duplicated(Data)

if (any(Duplicates)) {
  print("There are duplicates.")
} else {
  print("There are no duplicates.")
}



Non_duplicated_data <- Data %>% distinct()
duplicate_rows <- dplyr::setdiff(Data, Non_duplicated_data)
duplicate_rows <- Data %>% anti_join(Non_duplicated_data, by = names(Data))

##### removing unnecessary things from environment to free up r

View(data_frame_list)

rm(data_frame_list)
rm(data_list)
rm(file_paths_list)




