#' Sequence QA Checks
#' 
#' @description 
#' `seq_qa_checks` returns a data.frame with QA columns attached
#' 
#' @details 
#' The returned data.frame will have warning columns, each ending with `_warn`
#' at the end. The column will be NA when a QA check has not been flagged
#' and will start with the string `Warning:` when the QA check has been flagged along
#' with a message of what is being flagged. This should help the users distinguish
#' which flags are present in the data.frame.
#' 
#' @param df a data.frame or tibble
#' @param specimen SEQUENCE_SPECIMEN should be Yes, No or NA
#' @param reason SEQUENCE_REASON ex. SENTINEL_SURVEILLANCE
#' @param lab SEQUENCE_LAB a lab name
#' @param lineage SEQUENCE_VARIANT_OPEN_TEXT a lineage
#' @param status SEQUENCE_STATUS should be one of FAILED, LOW QUALITY or COMPLETE
#' @param accession SEQUENCE_ACCESSION should match the format in an external repository
#' @param clinical_accession SEQUENCE_CLINICAL_ACCESSION the FILLER__ORDER__NUM or test accession
#' @param specimen_collection_date the date the specimen was collected 
#' @param case_id CASE_ID in WDRS
#' @param notes SEQUENCE_NOTES
#' @param epi_isl should match the epi_isl in an external repository
#' @param date SEQUENCE_DATE - does anyone actually use this?
#' @param external_repo_accession an accession number from an external repo - like wa_gisaid$virus_name
#' @param list_of_accepted_lineages user defined formatted lineages
#' @param list_of_lab_names user defined list of formatted lab names
#' @param list_of_reasons user defined list of formatted sequence reasons
#' 
#' @export
#' 
#' @examples
#' logic_checks <- wdrs_sequence_table %>% seq_qa_checks(specimen = SEQUENCE_SPECIMEN...)
#' 
#' @note 
#' 2/15/2022 FA - As we discussed, add !is.na(SEQUENCE_LAB) to the reason-null
#' check because some records with missing data for all columns were getting
#' flagged as reason_null but they have other issues.We want to use this check
#' to determine aprox. when (month/year) when a record was missing a reason. 
#' So records before July were not regularly filling out the reason so those are
#' less of a concern,but we want to flag all because molecular is trying to fill
#' all of them in roster updates. 
seq_qa_checks <- function(df,
                          specimen,
                          reason,
                          lab,
                          lineage,
                          status,
                          accession,
                          clinical_accession,
                          specimen_collection_date,
                          case_id,
                          notes,
                          epi_isl,
                          sequence_date, # sequence_date
                          external_repo_accession, # wa_gisaid$SEQUENCE_ACCESSION_NUMBER
                          list_of_accepted_lineages,
                          list_of_lab_names,
                          list_of_reasons
                          ){
  df %>%
    mutate(
      ## ---- reason-null
      reason_null_warn = if_else(
        {{specimen}} == "Yes" & 
          is.na({{reason}}) & 
          !is.na({{lab}}),
        "Warning: SEQUENCE_REASON is NULL",
        NULL
      ),
      ## ---- stop
      ## ---- reason-warn
      reason_warn = if_else(
        {{specimen}} == "Yes" & !{{reason}} %in% list_of_reasons,
        "Warning: SEQUENCE_REASON not standardized",
        NULL
      ),
      ## ---- stop
      ## ---- var-status-warn
      var_status_warn = if_else(
        (!is.na({{lineage}}) & 
           {{lineage}} != "Unassigned") & 
          str_detect(toupper({{status}}),"COMPLETE",negate = T),
          "Warning: SEQUENCE_VARIANT_OPEN_TEXT filled but SEQUENCE_STATUS is not COMPLETE",
        NULL
      ),
      ## ---- stop
      
      ## ---- labs-check-warn
      sa_null_status_complete_warn = if_else(
        is.na({{accession}}) &
          !(toupper({{status}}) %in% c("FAILED", "LOW QUALITY", "FAILED, LOW QUALITY", NA)),
          "Warning: SEQUENCE_ACCESSION number NULL but status not FAILED/LOW QUALITY",
        NULL
      ),
      ## ---- stop
      ## ---- SA-null-variant-exists-warn
      # NW Genomics is spelled wrong, need to include partial strings
      # IF failed or low quality and lab not in PHL or NW Genomics then mark
      sa_null_variant_exists_warn = if_else(
        is.na({{accession}}) &
          {{specimen}} =="Yes" & 
          !is.na({{lineage}}) &
          (!{{accession}} %in% external_repo_accession),
        "Warning: SEQUENCE_VARIANT_OPEN_TEXT exists but SEQUENCE_ACCESION number is null",
        NULL
      ),
      ## ---- stop
      ## ---- variant-check-warn
      variant_check_warn = if_else(
        !({{lineage}} %in% variants) & 
          {{lineage}} != "Unassigned", 
        "Warning: SEQUENCE_VARIANT not of concern/interest - check or update list",
        NULL
      ),
      ## ---- stop
      ## ---- lab-name-warn
      lab_name_warn = if_else({{specimen}} == "Yes" &
                                (!{{lab}} %in% list_of_lab_names), 
                              "Warning: SEQUENCE_LAB not standardized - check or update list",
                              NULL
      ),
      ## ---- stop
      ## ---- date-warn
      date_warn = if_else(
        {{specimen_collection_date}} < "2020-01-05" | 
          {{specimen_collection_date}} > today(),
        "Warning: SEQUENCE_SPECIMEN_COLLECTION_DATE out of range. Before 1/05/2020 or after today's date",
        NULL
      ),
      ## ---- stop
      ## ---- seq-lab-neg-warn
      seq_lab_neg_warn = if_else({{specimen}} == "No" &
                                   (!is.na({{accession}}) |
                                      !is.na({{lineage}})), 
                                 "Warning: SEQUENCE_SPECIMEN = 'No' but sequencing data attatched",
                                 NULL
      ),
      ## ---- stop
      ## ---- SCA-SA-null-warn
      sca_sa_null_warn = if_else(
        (is.na({{accession}}) & 
           is.na({{clinical_accession}})) & 
          {{specimen}} == "Yes" &
          (!str_detect(toupper({{status}}),"FAILED|LOW QUALITY") | 
             is.na({{status}})),
          "Warning: SEQUENCE_ACCESSION number and SEQUENCE_CLINICAL_ACCESSION numbers missing",
        NULL
      ),
      ## ---- stop
      ## ---- unexpected-char-warn
      #11/22/2021 Flag values that aren't expected in a given column - ex. Notes shouldn't be in accession columns
      unexpected_char_warn = if_else(
        (str_count({{clinical_accession}}, " ") & 
           !str_detect({{clinical_accession}}, "TSC|SPC|FH|MOLE|ACOV|KCMEO|PCME")) |
          str_detect({{clinical_accession}},"\\.") |
          str_count({{accession}}, " ") > 0 |
          str_count({{specimen}}, " ") > 0 |
          str_count({{case_id}}, " ") > 0 | 
          str_count({{reason}}, " ") > 3 |
          str_count({{sequence_date}}, " ") > 0 |
          str_count({{status}}, " ") > 1 |
          str_count({{lineage}}, " ") > 0 |
          str_detect({{epi_isl}},"EPI_",negate = T),
          "Warning: Unexpected characters in a column",
        NULL
      ),
      ## ---- notes-warn
      notes_warn = if_else(
        str_detect({{notes}},"(?<=identified as )") &
          str_extract({{notes}},"(?<=identified as ).*(?= on [[:digit:]]{4}-[:digit:]{2}-[:digit:]{2}.)") != "None" &
          is.na({{lineage}}),
        "Warning: Lineage found in SEQUENCE_NOTES but SEQUENCE_VARIANT_OPEN_TEXT is NULL",
        NULL
      ),
      ## ---- stop
      ## ---- gisaid-flag-warn
      # gisaid_flag_warn = if_else(!is.na(SEQUENCE_ACCESSION) & 
      #                  (!SEQUENCE_ACCESSION %in% wa_gisaid$virus_name_clean), 
      #                         print("Warning: SEQUENCE_ACCESSION not found in GISAID"),
      #                         NULL
      # ),
      ## ---- stop
      ## ---- missing-lineage-warn
      missing_lineage_warn = if_else(
        is.na({{lineage}}) &
          {{status}} == "Complete",
        "Warning: SEQUENCE_STATUS = 'Complete' and SEQUENCE_VARIANT_OPEN_TEXT is NULL",
        NULL
      )
      ## ---- stop
    ) %>%
    # Flag duplicates
    ## ---- find-duplicates
    group_by({{lineage}}, {{accession}}, {{clinical_accession}}) %>% 
    mutate(sa_sca_var_dup_warn = if_else(n()>1 &
                                           !is.na({{lineage}}) &
                                           !is.na({{accession}}) &
                                           !is.na({{clinical_accession}}),
                                         "Warning: Duplicate - SCA, SA and Variant duplicated",
                                         NULL
    )
    ) %>%
    ungroup() %>%
    group_by({{accession}}, {{clinical_accession}}) %>% 
    mutate(sa_sca_dup_warn = if_else(n()>1 &
                                       !is.na({{accession}}) &
                                       !is.na({{clinical_accession}}) &
                                       is.na(sa_sca_var_dup_warn),
                                     "Warning: Duplicate - SCA and SA duplicated",
                                     NULL
    )
    ) %>%
    ungroup() %>%
    group_by({{accession}}) %>% 
    mutate(sa_dup_warn = if_else(n()>1 &
                                   !is.na({{accession}}) &
                                   is.na(sa_sca_dup_warn) &
                                   is.na(sa_sca_var_dup_warn),
                                 "Warning: Duplicate - SA duplicated",
                                 NULL
                                 )
    ) %>%
    ungroup()
  # ---- stop
  
}

#' Standardize lab names
#' 
#' @description 
#' Take known lab names and standardized them to match what's in WDRS
#' 
#' @param lab the original column of lab names - SEQUENCE_LAB
#' 
#' @examples 
#' wdrs_seq_table %>% mutate(standardize_lab_names(lab = SEQUENCE_LAB))
#' 
standardize_lab_names <- function(standardized_lab_column_name,lab){
      case_when(
        str_detect(toupper({{lab}}), "AEGIS") ~ "Aegis",
        str_detect(toupper({{lab}}), "ALTIUS") ~ "Altius",
        str_detect(toupper({{lab}}), "ATLAS") ~ "Atlas Genomics",
        str_detect(toupper({{lab}}), "CENTERS FOR DISEASE") ~ "CDC",
        str_detect(toupper({{lab}}), "FULGENT") ~ "Fulgent Genetics",
        str_detect(toupper({{lab}}), "GRAVITY") ~ "Gravity Diagnostics",
        str_detect(toupper({{lab}}), "HELIX") ~ "Helix",
        str_detect(toupper({{lab}}), "INFINITY BIOLOGIX") ~ "Infinity Biologix",
        str_detect(toupper({{lab}}), "LABCORP|LABORATORY CORPORATION") ~ "Labcorp",
        str_detect(toupper({{lab}}), "OHSU|OREGON HEALTH AND SCIENCE|OREGON HEALTH & SCIENCE") ~ "OHSU",
        str_detect(toupper({{lab}}), "WA STATE PHL|WASHINGTON STATE|WA STATE DEPARTMENT OF HEALTH") ~ "PHL",
        str_detect(toupper({{lab}}), "PROVIDENCE") ~ "Providence Swedish",
        str_detect(toupper({{lab}}), "QUEST") ~ "Quest",
        str_detect(toupper({{lab}}), "USAMRIID") ~ "USAMRIID",
        # There is one {{lab}} that got incorrectly flagged as UW Virology
        # when it should be Seattle Flu, so updating here
        {{lab}} == "University of Washington Medical Center, Seattle Flu Study" ~ "NW Genomics",
        str_detect(toupper({{lab}}), "UW VIROLOGY|UNIVERSITY OF WASHINGTON") ~ "UW Virology",
        str_detect(toupper({{lab}}), "THE JACKSON LAB") ~ "The Jackson Laboratory",
        str_detect(toupper({{lab}}), "NAVAL HEALTH") ~ "Naval Health Research Center",
        str_detect(toupper({{lab}}), "FLOW DIAGNOSTICS") ~ "Flow Diagnostics",
        str_detect(toupper({{lab}}), "US AIRFORCE|AEROSPACE") ~ "U.S. Airforce School of Aerospace Medicine",
        str_detect(toupper({{lab}}), "KAISER") ~ "KP WA Research Inst",
        str_detect(toupper({{lab}}), "BOISE VA") ~ "Boise VA",
        str_detect(toupper({{lab}}), "GRUBAUGH LAB") ~ "Grubaugh Lab",
        str_detect(toupper({{lab}}), "LAURING LAB") ~ "Lauring Lab",
        str_detect(toupper({{lab}}), "ANDERSEN") ~ "Andersen",
        str_detect(toupper({{lab}}), "ARIZONA STATE|ASU") ~ "ASU",
        str_detect(toupper({{lab}}), "ATLAS") ~ "Atlas",
        str_detect(toupper({{lab}}), "USAMRIID") ~ "USAMRIID",
        str_detect(toupper({{lab}}), "FRED HUTCHINSON") ~ "Fred Hutchinson",
        str_detect(toupper({{lab}}), "GINKGO") ~ "Ginkgo",
        str_detect(toupper({{lab}}), "YALE") ~ "Yale",
        str_detect(toupper({{lab}}), "HYDE") ~ "Hyde",
        str_detect(toupper({{lab}}), "OHSU") ~ "OHSU",
        TRUE ~ {{lab}}
      )
} 

#' Split a gisaid id into components
#' 
#' @param df placeholder for a dataframe
#' @param gisaid_id a gisaid_id column in the df. must use .$gisaid_id
#' @export
#' @examples 
#' df %>% split_gisaid_id(.$SEQUENCE_ACCESSION)
#' wa_gisaid %>% split_gisaid_id(.$virus_name_clean)
split_gisaid_id <- function(df,gisaid_id){
  
  df %>%
    mutate(GISAID_ID_FULL = str_trim({{gisaid_id}})) %>%
    
    # Take out HCOV as not all have that string
    mutate(GISAID_ID_NO_HCOV = str_remove(toupper({{gisaid_id}}),"HCOV-19/"))%>%
    
    # Extract State
    mutate(GISAID_ID_STATE = case_when(
      # Hardcode Aegis
      str_detect({{gisaid_id}},"ASC[:digit:]{1,}-B[:digit:]{1,}") ~ str_extract({{gisaid_id}},"[^-]+"),
      str_detect({{gisaid_id}},"USA/[[:alpha:]]{2,}") ~ str_extract(toupper(GISAID_ID_NO_HCOV),"(?<=USA/)[[:alpha:]]{2,}"),
      str_detect({{gisaid_id}},"^[[:alpha:]]{2,}-") ~ str_extract(toupper({{gisaid_id}}),"[[:alpha:]]{2,}(?=-)"),
      TRUE ~ NA_character_
    )) %>%
    
    # Extract year - make sure to use GISAID_ID_NO_HCOV to avoid the hcov/ in the extract
    mutate(GISAID_ID_YEAR = case_when(
      str_detect(GISAID_ID_NO_HCOV,"/[[:digit:]]{4}") ~ str_remove(str_extract(GISAID_ID_NO_HCOV,"/[[:digit:]]{4}"),"/"),
      TRUE ~ NA_character_
    )) %>%
    
    # Extract the middle portion of the ID (digit and lab included. Everything after a state and before the /year)
    mutate(GISAID_ID_MIDDLE = case_when(
      # Hardcode Altius and Aegis
      str_detect({{gisaid_id}},"ALTCOV") ~ str_remove(str_extract({{gisaid_id}},"(?<=ALTCOV-).*"),"(/[:digit:]{4})"),
      str_detect({{gisaid_id}},"ASC[:digit:]{1,}-B[:digit:]{1,}") ~ str_extract({{gisaid_id}},"[^-]+"),
      str_detect({{gisaid_id}},"USA/[[:alpha:]]{2}") ~ str_remove(str_remove(toupper(GISAID_ID_NO_HCOV),"(USA/[[:alpha:]]{2,}-)"),"(/[:digit:]{4})"),
      str_detect({{gisaid_id}},"^[[:alpha:]]{2,}-") ~ str_extract(toupper({{gisaid_id}}),"(?<=-)[:graph:]{1,}"),
      str_detect({{gisaid_id}},"[[:alpha:]]{2}(?=)") ~ str_remove({{gisaid_id}},"/[:digit:]{4}"),
      TRUE ~ NA_character_
    )) %>%
    
    # Take out any punctuation from the middle portion as it might be different in different datasets
    mutate(GISAID_ID_NO_PUNCT = str_replace_all(toupper(GISAID_ID_MIDDLE),"[^[:alnum:]]","")) %>%
    
    # Extract the digits only
    mutate(GISAID_ID_DIGIT = case_when(
      # The IDs that are already only numbers need to be treated differently
      is.na(GISAID_ID_MIDDLE) ~ str_extract_all(GISAID_ID_NO_HCOV,"[:digit:]+"),
      TRUE ~ str_extract_all(GISAID_ID_MIDDLE,"[:digit:]+")
    ))
}


#' Abbreviate lab names
#' 
#' @description 
#' Make abbreviations for the lab names to abbreviate the excel file names
#' 
#' @param charVec some character vector
#' @export
makeInitials <- function(charVec) {
  make.unique(vapply(strsplit(str_trunc(charVec, 20), " "),
                     function(x)
                       paste(substr(x, 1, 20), collapse = ""),
                     vector("character", 1L)))
}




#' Output data that doesn't exist in GISAID 
#' 
#' @description 
#' Create a function that outputs the lab to a excel file by lab name
#' use mapply to pass the vector of labs with >= 30 obs (from num_unmatchbylab)
#' 
#' @details 
#' This function will take a list of data that does not exist in GISAID and write it to a csv by lab.
#' It is also broken into data that has a variant and data that doesn't have a variant.
#' It will create the newday directory if there is unmatched data here
#' Then it will take all labs that have 30 or more unmatched data and output a file for each one
#' Each csv file will then be named an abbreviated name 'outlab' in the output function. 
#'
#' @param lab - This is a list of labs
#'
#' @return Creates csv into 3 separate folders that are created with today's date. 
#' @export
#'
#' @examples
output <- function(lab) {
  
  #the 'newday' variable is a file path labeled as today's date. It will be created below, but need to call it in the function first
  if(!dir.exists(newday)){
    
    dir.create(newday)
  }
  
  #All the individual files will output to the 'newday' directory when it's created in the chunk below this one
  std_unmatched_clean %>%
    filter(str_detect(toupper(std_unmatched_clean$SEQUENCE_LAB), toupper(lab)))%>%
    # #make all seattle flu - NW Genomics per data support's request
    mutate(SEQUENCE_LAB=if_else(SEQUENCE_LAB=="Seattle Flu Study","NW Genomics",SEQUENCE_LAB))%>%
    # Convert the record to template submitter format to send back to labs
    mutate(LAB_ACCESSION_ID = NA_character_,
           GISAID_ID = SEQUENCE_ACCESSION_NUMBER,
           SPECIMEN_COLLECTION_DATE = SEQUENCE_SPECIMEN_COLLECTION_DATE,
           SUBMITTING_LAB=SEQUENCE_LAB,
           PANGO_LINEAGE = SEQUENCE_VARIANT_OPEN_TEXT,
           FIRST_NAME= NA_character_,
           LAST_NAME = NA_character_,
           MIDDLE_NAME = NA_character_,
           DOB = NA_character_,
           ALTERNATIVE_ID = NA_character_) %>%
    select(LAB_ACCESSION_ID,
           GISAID_ID,SPECIMEN_COLLECTION_DATE,
           SUBMITTING_LAB,
           SEQUENCE_REASON,
           SEQUENCE_STATUS,
           PANGO_LINEAGE,
           FIRST_NAME,
           LAST_NAME,
           MIDDLE_NAME,
           DOB,
           ALTERNATIVE_ID) %>%
    fwrite(file.path(
      newday,
      paste0(lab,
             "_",
             today(),
             ".csv")))
}


  
style_table <- function(df){
  
  df %>%
    #All column headers are capitalised
    opt_all_caps() %>% 
      #Use the Chivo font
      #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      tab_options(
        #Remove border between column headers and title
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "black",
        #Remove border around table
        table.border.top.color = "black",
        table.border.bottom.color = "black",
        #Reduce the height of rows
        data_row.padding = px(3),
        #Adjust font sizes and alignment
        source_notes.font.size = 12,
        heading.align = "left"
      ) %>%
      # We use tab_style() to change style of cells
      # cell_borders() provides the formatting
      # locations tells it where
      # Add black borders to the bottom of all the column labels
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_column_labels(
            columns = gt::everything()
          )
        )
      )
}

  