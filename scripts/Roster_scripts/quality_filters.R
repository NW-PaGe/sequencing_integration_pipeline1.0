
# Helper Functions for Cleaning Data

# remove anything but alphanumeric characters; i.e. punctuation (does not remove spaces)
kill <- function(x) {str_replace_all(x, "[^[:alnum:]]", " ")}

# remove any instance of a space followed by a letter at the end of a string; usually an initial
destroy <- function(y) {str_replace_all(y, c("[[:space:]][[:alpha:]]{1}$"), " ")}

# remove white spaces
smash <- function(z) {str_replace_all(z, c(" "), "")}

# remove all non-alphanumeric characters, then remove white space around the edges, then remove initials following names, then remove any remaining spaces
annihilate <- function(w) {toupper(smash(destroy(str_trim(kill(w)))))}


# detect format of the sequence accession
detect_sequence_accession <- function(x) {!(str_detect(x, "hCoV")) | !((str_detect(x, "USA/")) & !(x == "")) }


# confirm that date is in specified date format
# For example, for April 3, 2021 the acceptable formats include 21-04-03, 2021-4-3, 04/03/2021, and 4/3/2021
detect_date_format <- function(x) {str_detect(x, "[:digit:]{2,4}[-][:digit:]{1,2}[-][:digit:]{1,2}") |
    str_detect(x, "[:digit:]{1,2}[/][:digit:]{1,2}[/][:digit:]{4}")}

# a function to convert an excel 5-digit date to a date that is compatible with WDRS
convert_excel_date <- function(x) {as.Date(as.numeric(x), origin = "1899-12-30")}

# checks that the lineage matches the expected format
detect_lineage_fun <- function(x) {
  str_detect(x, "[[:alpha:]]{1,2}[.][[:digit:]]{1,2}")
}

# method for fuzzy matching
# matches all records on name with a string similarity distance
# less than or equal to 3
fuzzy_matching <- function(x) {stringdist_left_join(
  x,
  wdrs_entire,
  by = c("NAME_SUBMITTER" = "NAME_WDRS"),
  max_dist = 3,
  distance_col = "distance"
)}

# Error checks applied to data that is ready to be rostered
# 12 different error checks - records ready to be rostered should pass all
# checks, while records failing some of the checks will be sent for manual review
#
# params:
# x: data in roster format
# lab_vars: lab variables, such as accepted sequence reasons, read in from file
# wdrs_sa_flat_values: wdrs sequence accessions
# wdrs_sca_flat_values: wdrs sequence clinical accessions
# valid_lineages: list of all lineages from the lineages.csv file
roster_filters <- function(x, lab_vars, wdrs_sa_flat_values, wdrs_sca_flat_values, valid_lineages, roster = FALSE) {
  
  these_data <- x
  
  quality_check <- x %>% # error when case id is missing
    mutate(QA_CASE_ID = case_when(is.na(CASE_ID) ~ 1),
           # error when sequence clinical accession is na
           QA_SCA_NA = case_when(is.na(SEQUENCE_CLINICAL_ACCESSION) &
                                   !(SEQUENCE_LAB %in% c("Aegis","Quest","Helix")) ~ 1),
           # error if specimen is not missing sca and sca is internally duplicated
           QA_SCA_INT_DUPE = case_when(!(is.na(SEQUENCE_CLINICAL_ACCESSION) | SEQUENCE_CLINICAL_ACCESSION == "") & 
                                                SEQUENCE_CLINICAL_ACCESSION %in%
                                                these_data$SEQUENCE_CLINICAL_ACCESSION[duplicated(these_data$SEQUENCE_CLINICAL_ACCESSION)] ~ 1),
           # error if sca in wdrs
           QA_SCA_WDRS_DUPE = case_when(!(is.na(SEQUENCE_CLINICAL_ACCESSION) | SEQUENCE_CLINICAL_ACCESSION == "") &
                                          SEQUENCE_CLINICAL_ACCESSION %in% wdrs_sca_flat_values ~ 1),
           # error if sa is internally duplicated
           QA_SA_INT_DUPE = case_when(!(is.na(SEQUENCE_ACCESSION) | SEQUENCE_ACCESSION == "") & 
                                        SEQUENCE_ACCESSION %in% these_data$SEQUENCE_ACCESSION[duplicated(these_data$SEQUENCE_ACCESSION)] ~ 1),
           # error if wdrs duplicate sa
           QA_SA_WDRS_DUPE = case_when(!(is.na(SEQUENCE_ACCESSION) | SEQUENCE_ACCESSION == "") &
                                         SEQUENCE_ACCESSION %in% wdrs_sa_flat_values ~ 1),
           # error if detect "NONE" in sequence notes or if lineage isn't in the list of all lineages
           QA_SEQ_VARIANT = case_when(str_detect(toupper(SEQUENCE_NOTES), "NONE|NA|UNASSIGNED") |
                                      (!SEQUENCE_VARIANT_OPEN_TEXT %in% valid_lineages &
                                         !(is.na(SEQUENCE_VARIANT_OPEN_TEXT) | SEQUENCE_VARIANT_OPEN_TEXT == "")) ~ 1),
           #  sequence status SA error or sequence status is missing
           QA_SEQ_STAT = case_when(is.na(SEQUENCE_STATUS) ~ 1,
                                   # error when status is complete but sa is missing,
                                   SEQUENCE_STATUS == "COMPLETE" & (is.na(SEQUENCE_ACCESSION) | SEQUENCE_ACCESSION == "") ~ 1,
                                    # error when status is not complete and sa is not missing
                                    SEQUENCE_STATUS %in% c("NOT DONE",
                                                           "HIGH CT") & !(is.na(SEQUENCE_ACCESSION) | SEQUENCE_ACCESSION == "")~ 1),
           # error when lab is cdc lab but reason is not an accepted cdc lab output reason
           QA_SEQ_REASON = case_when(SEQUENCE_LAB %in% lab_vars$cdc_labs & !(SEQUENCE_REASON %in% lab_vars$output_seq_reasons_cdc) ~ 1,
                                    # error when lab is not a cdc lab o phl and sequence reason is not an accepted cdc lab output reason
                                    !(SEQUENCE_LAB %in% c(lab_vars$cdc_labs, "PHL")) &!(SEQUENCE_REASON %in% lab_vars$output_seq_reasons_non_cdc) ~ 1,
                                    # error when sequence lab is phl but sequence reason is not an accepted phl output reason
                                    SEQUENCE_LAB == "PHL" & !(SEQUENCE_REASON %in% lab_vars$output_seq_reasons_phl) ~ 1),
           # error when status is complete and sequence notes are blank
           QA_SEQ_NOTES = case_when(SEQUENCE_STATUS == "COMPLETE" & (SEQUENCE_NOTES == "" | is.na(SEQUENCE_NOTES)) ~ 1,
                                   # error when status is not complete and sequence notes are not blank
                                   SEQUENCE_STATUS %in% c("FAILED",
                                                          "NOT DONE",
                                                          "LOW QUALITY",
                                                          "HIGH CT") & !(SEQUENCE_NOTES == "" | is.na(SEQUENCE_NOTES)) ~ 1))
  
  
    if(!roster) {
      quality_check <- quality_check %>%
        mutate(int = interval(
          mdy(SEQUENCE_SPECIMEN_COLLECTION_DATE),
          mdy(COLLECTION_DATE_WDRS)
        ),
        COLLECTION_DATE_DISTANCE = abs(time_length(int, unit = "day"))) %>%
        select(-int)
      
      quality_check <- quality_check %>%
        # error if the sequence sample and wdrs record collection dates differ by more than 14 days
        mutate(QA_COLLECT_DATE = case_when(COLLECTION_DATE_DISTANCE > 14 ~ 1,
                                           is.na(COLLECTION_DATE_DISTANCE) ~ 1),
                # QA_OTHER checks not for non roster outputs
                QA_OTHER = NA) %>%
        select(-COLLECTION_DATE_DISTANCE)
      
    } else {
      years <- year(seq(ymd("2020-01-01"), today(), by = "years"))
      valid_years <- paste(years, collapse = "|")
      quality_check <- quality_check %>%
        
        mutate(
              # add collection date wdrs column
              COLLECTION_DATE_WDRS = NA,
              # missing SCA allowed at roster step
              QA_SCA_NA = NA,
               # check collection date within individual scripts, not at roster
               QA_COLLECT_DATE = NA,
              # Temporary Fix: This QA_SCA_WDRS_DUPE and QA_SCA_INT_DUPE step to be removed by DIQA (Refer to GitHub issues #488, #645 for details)
              # allow SCA WDRS duplicates records that have been checked by DIQA 
              QA_SCA_WDRS_DUPE = if_else(!str_detect(index, "duplicates_roster"), 
                                         QA_SCA_WDRS_DUPE, NA_real_),
              # allow SCA internal duplicate records that have been checked by DIQA
              QA_SCA_INT_DUPE = if_else(!str_detect(index, "duplicates_roster"), 
                                        QA_SCA_INT_DUPE, NA_real_),
               # Other checks on roster format
               QA_OTHER = case_when(
                 # SEQUENCE_SGTF should be blank
                 SEQUENCE_SGTF != "" | !is.na(SEQUENCE_SGTF) ~ 1,
                 # SEQUENCE_SPECIMEN column should be YES
                 SEQUENCE_SPECIMEN != "YES" ~ 1,
                 # SEQUENCE_DATE should be blank
                 SEQUENCE_DATE != "" | !is.na(SEQUENCE_DATE) ~ 1,
                 # SEQUENCE_LAB should not be NA and should be valid
                 is.na(SEQUENCE_LAB) | !SEQUENCE_LAB %in% lab_vars$lab_names_wdrs ~ 1,
                 # SEQUENCE REPOSITORY isn't GISAID
                 SEQUENCE_REPOSITORY != "GISAID" ~ 1,
                 # SEQUENCE_REVIEWED isn't blank
                 SEQUENCE_REVIEWED != "" | !is.na(SEQUENCE_REVIEWED) ~ 1,
                 # SEQUENCE_ACCESSION is in accepted format 
                   # includes "USA/" 
                  !str_detect(SEQUENCE_ACCESSION, "USA/") ~ 1,
                   # does not include "hCoV"
                   str_detect(SEQUENCE_ACCESSION, "hCoV") ~ 1,
                   # ends in a valid year
                  !str_ends(SEQUENCE_ACCESSION, valid_years) ~ 1,
                 # Case.Note filled in
                 !str_detect(Case.Note, "External data question package updated by COVID19 Sequencing Roster.") ~ 1,
                 # CASE_ID doesn't include letters
                 str_detect(CASE_ID, "[:alpha:]") ~ 1,
                 # SEQUENCE_VARIANT_OPEN_TEXT is complete when SEQUENCE_STATUS is COMPLETE
                 SEQUENCE_STATUS == "COMPLETE" & is.na(SEQUENCE_VARIANT_OPEN_TEXT) ~ 1,
                 
                 # SEQUENCE_VARIANT_OPEN_TEXT is empty when SEQUENCE_STATUS is LOW QUALITY or FAILED
                 # Low Quality records can have a SEQUENCE_VARIANT_OPEN_TEXT equal to "Unassigned" - not all Low Quality will be blank sequence variant
                 (SEQUENCE_STATUS == "LOW QUALITY" | SEQUENCE_STATUS == "FAILED") & (!is.na(SEQUENCE_VARIANT_OPEN_TEXT) & SEQUENCE_VARIANT_OPEN_TEXT != "Unassigned") ~ 1,
                 
                 # SEQUENCE_SPECIMEN_COLLECTION_DATE isn't in correct format
                 # SEQUENCE_SPECIMEN_COLLECTION_DATE is in a 5-digit format
                 na.omit(str_detect(SEQUENCE_SPECIMEN_COLLECTION_DATE, "^[[:digit:]]{5}$")) ~ 1,
                 # SEQUENCE_SPECIMEN_COLLECTION_DATE couldn't be parsed
                is.na(parse_date_time(SEQUENCE_SPECIMEN_COLLECTION_DATE, c("mdy", "ymd"))) ~ 1,
                 # SEQUENCE_SPECIMEN_COLLECTION_DATE is not in the correct format
                 !( SEQUENCE_SPECIMEN_COLLECTION_DATE ==
                    as.character(format(parse_date_time(SEQUENCE_SPECIMEN_COLLECTION_DATE, c("mdy", "ymd")), "%m/%d/%Y")) | 
                    SEQUENCE_SPECIMEN_COLLECTION_DATE !=
                    gsub("0(\\d)", "\\1", format(parse_date_time(SEQUENCE_SPECIMEN_COLLECTION_DATE, c("mdy", "ymd")), "%m/%d/%Y"))) ~ 1
               )) %>% 
                # move COLLECITON_DATE_WDRS to match template
                relocate(COLLECTION_DATE_WDRS, .before = QA_CASE_ID)
        
    }
  
  
    quality_check <- quality_check %>% 
    rowwise() %>%
    mutate(sum = sum(
      c_across(QA_CASE_ID:QA_OTHER),
      na.rm = TRUE))
  
  
  quality_table <- quality_check %>% 
    select(QA_CASE_ID:QA_OTHER) %>%
    colSums(na.rm = TRUE)
  
  print(quality_table)
  
  return(quality_check) 
}

# FUZZY_MATCHING specific functions

### Print Output Functions

print_roster <- function(x, for_review = FALSE, columns_included = "") {
  
  select_columns <- c(
    'CASE_ID',
    'SEQUENCE_SGTF',
    'SEQUENCE_SPECIMEN',
    'SEQUENCE_DATE',
    'SEQUENCE_REASON',
    'SEQUENCE_LAB',
    'SEQUENCE_STATUS',
    'SEQUENCE_REPOSITORY',
    'SEQUENCE_ACCESSION',
    'SEQUENCE_EPI_ISL',
    'SEQUENCE_VARIANT_OPEN_TEXT',
    'SEQUENCE_CLINICAL_ACCESSION',
    'SEQUENCE_SPECIMEN_COLLECTION_DATE',
    'SEQUENCE_ROSTER_PREPARE_DATE',
    'SEQUENCE_NOTES',
    'SEQUENCE_REVIEWED',
    'Case.Note')
  
  if(for_review) {
    select_columns <- c(select_columns,
                        'NAME_SUBMITTER',
                        'DOB',
                        'DISTANCE_NAME',
                        'NAME_WDRS')
    
    if(columns_included != "") {
      select_columns <- c(select_columns, columns_included)
    }
  }
  # filter based on the error sums
  final_roster <- x %>% 
    filter(sum == 0) %>%
    mutate(SEQUENCE_CLINICAL_ACCESSION = replace_na(SEQUENCE_CLINICAL_ACCESSION, ""),
           SEQUENCE_SPECIMEN_COLLECTION_DATE = replace_na(SEQUENCE_SPECIMEN_COLLECTION_DATE, "")) %>%
    select(all_of(select_columns))
  
  roster_name <- deparse(substitute(x)) 
  
  print(roster_name)
  
  print(paste0(roster_name, ": ", nrow(final_roster)))
  
  roster_folder_file_path <- ""
  if(for_review) {
    # filter low quality and failed specimens from rows being sent to for_review or fuzzy_error_checks
    final_roster <- final_roster %>%
      filter(!(SEQUENCE_STATUS %in% c("FAILED", "LOW QUALITY")) | SEQUENCE_LAB =='PHL')
    
    roster_folder_file_path <- file.path(roster_folder_file_path,"Fuzzy_matches")
    
  } else {
    roster_folder_file_path <- file.path(roster_folder_file_path, "write_roster_here")
    
  }
  
  if(nrow(final_roster) > 0) {
    write_csv(final_roster, file.path(roster_folder_file_path, paste0(roster_name, 
                                                                      "_", 
                                                                      today(),
                                                                      ".csv")), na = "")
  }
  
}




## Error Rows printer: A FUZZY_MATCHING specific function to print error rows after a roster has been printed using the print_roster function

print_error_rows <- function(x, roster_now = FALSE) {
  
  error_rows <- x %>% 
    filter(sum > 0, 
           !(SEQUENCE_STATUS %in% c("FAILED", "LOW QUALITY")) | SEQUENCE_LAB == 'PHL') %>%
    mutate(SEQUENCE_CLINICAL_ACCESSION = replace_na(SEQUENCE_CLINICAL_ACCESSION, ""),
           SEQUENCE_SPECIMEN_COLLECTION_DATE = replace_na(SEQUENCE_SPECIMEN_COLLECTION_DATE, "")) %>%
    select(-sum, -rowid)
  
  file_name <- deparse(substitute(x))
  
  print(paste0(file_name, " error rows", " : ", nrow(error_rows)))
  
  if (nrow(error_rows) > 0) {
    if (roster_now) {
      write_csv(
        error_rows,
        paste0(
          "",
          "//For_Review//",
          file_name,
          "_error_rows_",
          today(),
          ".csv"
        ), na = ""
      )
    } else {
      write_csv(
        error_rows,
        paste0(
          "",
          "//Fuzzy_matches//Fuzzy_Error_Checks//",
          file_name,
          "_error_rows_",
          today(),
          ".csv"
        ), na = ""
      )
    }
  }
  
}


