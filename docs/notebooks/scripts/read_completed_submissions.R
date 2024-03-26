# ------ Load libraries and paths ------ #
box::use(
  dplyr[...],
  tibble[...],
  data.table[...],
  magrittr[`%>%`],
  pins[...],
  purrr[...],
  lubridate[...],
  readxl[read_xlsx],
  rio[import],
  vroom[vroom],
  fs[...],
  furrr[future_map],
  jsonlite[...],
  janitor[...],
  stringr[...],
  openxlsx[convertToDate],
  tictoc[tic,toc],
  scripts/functions[...]
)

# caution - this will take more than an hour to run
completed_submissions_extract <- function(){
  # ------ Read in all files into a list ------ #

  # find the file paths
  submission_file_paths <- dir_ls(path = file.path(project_folder, "Completed_Submissions"),
                          recurse = TRUE,
                          type = "file")
  
  tic()
  # Read in the files
  submission_file_list <- future_map(submission_file_paths,safer_process_file) %>%
    # attach the file path name to the element in the list
    setNames(submission_file_paths) %>%
    # return the original list as an object in the environment
    {. ->> original_file_list} %>%
    # remove the bad or corrupt files that can't be read in
    keep(~!inherits(.x, 'character')) %>%
    # convert all columns to character so they can bind easily
    rapply(as.character, how = "replace")
  toc()
  
  # Get a list of all the corrupt files
  bad_files <- keep(original_file_list, ~inherits(.x, 'character'))
  
  
  # ------ transform the list of files ------ # 
  
  
  # make a json column in each file
  tic()
  temp_json <- future_map(submission_file_list,
                          ~ purrr::possibly(make_json_column,
                                            otherwise = "error_here")(.x)) %>%
    # return the original list as an object in the environment
    {. ->> original_json_file_list} %>%
    # remove the bad or corrupt files that can't be read in
    keep(~!inherits(.x, 'character')) %>%
    # convert all columns to character so they can bind easily
    rapply(as.character, how = "replace")
  toc()
  
  bad_json_files <- keep(original_json_file_list, ~inherits(.x, 'character'))
  
  if(length(bad_json_files)> 0){
    stop("jsonify function may have failed!!! stop and check script")
  }
  
  # Clean up and coalesce the column names
  # get a list of all the column names in total
  names_temp <- c()
  
  for (i in names(temp_json)) {
    test <- names(temp_json[[i]])
    names_temp <- c(names_temp,test)
  }
  
  names_cs_list <- unique(names_temp)
  
  # Get the GISAID_ID column 
  # Previous versions of this script contain code that programmatically searched for the column names of interest. 
  # Since this script will be retired, I hardcoded this list for GISAID_ID names to make things easier. 
  gisaid_id_names <- c("GISAID_ID",
                       "Seq ID",
                       "SEQUENCE_ACCESSION",
                       "shortname",
                       "gisaid_virus_name",
                       "Alt_CoVID",
                       "altius_sample_identifier",
                       "virus_name",
                       "Virus name",
                       "strain_name",
                       "GISAID",
                       "IDs",
                       "GISAID ID",
                       "Taxon ID")
  
  # Get the Accession ID column
  accession_names <- names_cs_list[which(str_detect(toupper(names_cs_list),
                                                    "ACCESSION|ACC|SPECIMENID|SPECIMEN_ID|SEQUENCE_CLINICAL|ACESSION"))]
  
  # Get date
  date_names <- names_cs_list[which(str_detect(toupper(names_cs_list),"DATE"))]
  
  # Get submitter names
  submitter_names <- names_cs_list[which(str_detect(toupper(names_cs_list),"SUBMITTER|LAB"))]
  
  # combine to get columns I want and exclude the rest
  keep_cols <- c(gisaid_id_names,
                 accession_names,
                 date_names,
                 submitter_names,
                 "SEQUENCE_REASON",
                 "SEQUENCE_STATUS",
                 "PANGO_LINEAGE",
                 "FIRST_NAME",
                 "LAST_NAME",
                 "MIDDLE_NAME",
                 "DOB",
                 "ALTERNATIVE_ID",
                 "raw_inbound_submission")
  
  # Above we created a vector of the specific column names that we want called `keep_cols`. 
  # This vector will be used in the select statement to take only those colnames in the `temp` list. 
  # If you look in your R environment, you will see that the original `files_recent_list` is giant 
  # and now the `all_results_list_clean` list is much smaller and will be easier to convert to a dataframe
  all_results_list_clean <- lapply(temp_json, function(x) subset(x, select = intersect(keep_cols,colnames(x))))
  
  
  # ------ convert to a dataframe ------ #
  
  completed_submissions_raw <- rbindlist(all_results_list_clean,idcol = "file_name",fill = TRUE)
  
  
  # ------ wrangle the dataframe ------ #
  
  # Next, we need to convert the columns into their appropriate names. 
  # There could be many names for a single column. 
  # For example, GISAID_ID in one dataframe could be GISAID.ID or virusname in another. 
  # Convert them all to say GISAID_ID.
  
  # The coalesce function below will check the names of `completed_submissions_raw` against the list of names we made above. 
  # Then it will convert them into one column
  
  # find all the gisaid_id named columns
  gisaid_cols <- syms(intersect(gisaid_id_names, names(completed_submissions_raw)))
  
  # take out SEQUENCE_ACCESSION and others as they are not clinical accession numbers
  clinical_accession_names <- setdiff(accession_names,c("Link_test_to_parent_accession","SEQUENCE_ACCESSION"))
  
  accession_cols <- syms(intersect(clinical_accession_names, names(completed_submissions_raw)))
  
  # date_cols <- syms(intersect(DATE_names, names(completed_submissions_raw)))
  
  # These records need to be removed - they have been filtered out of the PHL script because they have reason == PT or missing Sequencing Result
  to_remove <- board %>% pin_read("records_to_remove")
  
  # manipulate
  completed_submissions_clean <- completed_submissions_raw %>%
    as_tibble() %>%
    mutate(across(where(is.character),
                  ~if_else(. %in% na_strings, NA_character_, .))) %>%
    mutate(SPECIMEN_COLLECTION_DATE = case_when(
      !is.na(SPECIMEN_COLLECTION_DATE) ~ SPECIMEN_COLLECTION_DATE,
      !is.na(SEQUENCE_SPECIMEN_COLLECTION_DATE) ~ SEQUENCE_SPECIMEN_COLLECTION_DATE,
      !is.na(`Collected Date`) ~ `Collected Date`,
      TRUE ~ SPECIMEN_COLLECTION_DATE
    )) %>%
    mutate(clean_date = date_cleanup(SPECIMEN_COLLECTION_DATE)) %>%
    # coalesce function will take the columns we found above and convert them all to one name. 
    # So virusname and GISAID.ID will convert to GISAID_ID, as an example
    
    mutate(GISAID_ID = str_trim(coalesce(!!!gisaid_cols))) %>%
    mutate(LAB_ACCESSION_ID = str_trim(coalesce(!!!accession_cols))) %>%
    mutate(SUBMITTING_LAB = case_when(
      !is.na(SUBMITTING_LAB) ~ SUBMITTING_LAB,
      !is.na(SEQUENCE_LAB) ~ SEQUENCE_LAB,
      !is.na(SUBMITTER) ~ SUBMITTER,
      !is.na(SubmittingLab) ~ SubmittingLab,
      !is.na(Submitter) ~ Submitter,
      !is.na(submitting_lab) ~ submitting_lab,
      TRUE ~ NA_character_
    )) %>%
    mutate(file_chopped = str_extract(file_name,"Completed_Submissions.*")) %>%
    select(file_chopped,
           clean_date,
           LAB_ACCESSION_ID,
           GISAID_ID,
           SPECIMEN_COLLECTION_DATE,
           SUBMITTING_LAB,
           SEQUENCE_REASON,
           SEQUENCE_STATUS,
           PANGO_LINEAGE,
           FIRST_NAME,
           LAST_NAME,
           MIDDLE_NAME,
           DOB,
           ALTERNATIVE_ID,
           raw_inbound_submission) %>%

    filter(!GISAID_ID %in% to_remove$GISAID_ID)
  
  # Dedup where lab is PHL. PHL outputs the exact same records to completed submissions every roster day, 
  # so there are many unnecessary duplicates from them. Leave other duplicates as there could be multiple GISAID_IDs in different files 
  # this will allow us to see all iterations of a record that was sent through our process
  completed_submissions_dedup <- completed_submissions_clean %>%
    group_by(GISAID_ID,LAB_ACCESSION_ID) %>%
    filter(!duplicated(str_detect(file_chopped, "PHL")) | str_detect(file_chopped,"PHL",negate = T)) %>%
    ungroup()
  
  # Check to see if the dedup process accidentally excluded any GISAID_IDs. All GISAID_IDs should be in the the dedup dataset
  check_dedup <- anti_join(completed_submissions_clean,completed_submissions_dedup, by = "GISAID_ID")
  
  if(dim(check_dedup)[1]>0){
    print("Dedup process failed. Stop and check the script")
    stop()
  } else{
    print("Dedup Success")
  }
  
  # ------ create the received_submission_archive table ------ #
  
  received_submission_archive <- completed_submissions_dedup %>%
    rowid_to_column(var = "submission_number") %>%
    select(submission_number,
           submitter = SUBMITTING_LAB,
           file_path = file_chopped,
           row_create_datetime = clean_date,
           raw_inbound_submission)
  
  # ------ historical table for querying ------ #
  
  historical_table <- completed_submissions_dedup %>%
    select(submitter = SUBMITTING_LAB,
           file_path = file_chopped,
           clean_date,
           gisaid_virus_name = GISAID_ID,
           accession = LAB_ACCESSION_ID,
           raw_inbound_submission
    )
  
  
  # ------ save the historical table as a pin ------ #
  
  # Now, save the object. Use the metadata to describe any updates made to the object
  board %>% pin_write(historical_table,
                      name = "completed_submissions",
                      type = "rds",
                      versioned = TRUE,
                      metadata = list(
                        "author" = "Frank Aragona",
                        "team" = "DIQA",
                        "subteam" = "Trash Pandas",
                        "commit" = "remove NA like values and fill dates"
                      ),
                      description = paste0("This should be the most complete as of ",Sys.Date()))
  
  return(historical_table)

}



# ------ read in completed submissions and transport object ------ #



# runs only when script is run by itself
if (sys.nframe() == 0){
  # ... do main stuff
  
  # run completed submissions
  completed_submissions_extract()
  
}

#' @export
#' 
#' @description
#' Read in completed submissions as an object
#' This makes it easier to read into an external process
completed_submissions <- function(){
  board %>% 
  pin_read("completed_submissions") %>%
  as_tibble() %>%
  mutate(across(where(is.character),
                ~if_else(. %in% na_strings, NA_character_, .)))
}