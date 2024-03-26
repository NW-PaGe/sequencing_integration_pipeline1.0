box::use(
  DBI[dbConnect,dbGetQuery],
  odbc[odbc],
  lubridate[...],
  pins[...],
  tibble[...],
  dplyr[...],
  magrittr[`%>%`],
  purrr[...],
  data.table[...],
  readxl[read_xlsx],
  vroom[vroom],
  readr[...],
  rio[import],
  stringr[...],
  stats[na.omit],
  gt[...],
  gtExtras[...]
)

# ------ set file paths ------ #
# read in r_creds.RDS
#' @export
creds <-yaml::read_yaml(file.path(Sys.getenv("USERPROFILE"),
                                  "Projects/personal-repos/sanitized-sequencing",
                                  "creds.yml"))

#' @export
project_folder <- paste0("\\",creds$default$paths$network_drive)

#' @export
# Set the board for completed submissions
board <- pins::board_folder(file.path(project_folder,"WDRS QA/pins"),versioned = FALSE)



#' @export
#' Import the board for test submissions for pin_read/write
test_submissions_board <- pins::board_folder(file.path(
  project_folder,
  "2.0_dev_env/TEST_SUBMISSIONS"),
  versioned = TRUE)



#' @export
# Set strings that should be NA
na_strings <- c("NA",
                "N A",
                "N / A",
                "N/A",
                "N/ A",
                "Not Available",
                "NOt available",
                "NaN",
                "NAN",
                "",
                " ")

# ------ Add custom functions ------ #

#' @export
#' @description
#' Function to add the word TEST to the GISAID_ID
#' 
#' @details
#' The function adds multiple variables to split a GISAID_ID into components
#' such as the middle portion of the ID, the digit portion, the year, etc.
#' This is helpful when we need to match certain elements of the ID. 
#' The TEST GISAID_ID is helpful to add a 'TEST' string so the GISAID_ID will not
#' match other datasets (useful in testing)
#' 
#' 
#' @param df placeholder for a dataframe. 
#' Allows you to chain it like a dplyr verb (i.e. df %>% mutate() %>% tweak_gisaid_id)
#' @param gisaid_id input the name of the gisaid_id like .$gisaid_id
#' @examples
#' testdf_all <- completed_submissions %>% tweak_gisaid_id(.$GISAID_ID)
tweak_gisaid_id <- function(df,gisaid_id){
  df %>%
    mutate(GISAID_ID_FULL = str_trim({{gisaid_id}})) %>%
    
    # Take out HCOV as not all have that string
    mutate(GISAID_ID_NO_HCOV = str_remove(toupper({{gisaid_id}}),"HCOV-19/"))%>%
    
    # Take out HCOV as not all have that string
    mutate(GISAID_ID_CUT = sub("/[^/]+$",
                               "",
                               str_remove(toupper({{gisaid_id}}),
                                          "HCOV-19/")))%>%
    
    # Extract year - make sure to use GISAID_ID_NO_HCOV to avoid the hcov/ in the extract
    mutate(GISAID_ID_YEAR = case_when(
      str_detect(GISAID_ID_NO_HCOV,"/[[:digit:]]{4}") ~ 
        str_remove(str_extract(GISAID_ID_NO_HCOV,"/[[:digit:]]{4}"),"/"),
      TRUE ~ NA_character_
    )) %>%
    
    mutate(GISAID_ID_TEST = case_when(
      !is.na(GISAID_ID_FULL) ~ paste0(GISAID_ID_CUT,"TEST/",GISAID_ID_YEAR),
      TRUE ~ NA_character_)
    ) %>%
    
    # Extract the middle portion of the ID (digit and lab included. Everything after a state and before the /year)
    mutate(GISAID_ID_MIDDLE = case_when(
      # Hardcode Altius and Aegis because they are non-standard format.
      str_detect({{gisaid_id}},"ALTCOV") ~ 
        str_remove(str_extract({{gisaid_id}},"(?<=ALTCOV-).*"),"(/[:digit:]{4})"),
      
      str_detect({{gisaid_id}},"ASC[:digit:]{1,}-B[:digit:]{1,}") ~ 
        str_extract({{gisaid_id}},"[^-]+"),
      
      str_detect({{gisaid_id}},"USA/[[:alpha:]]{2}") ~ 
        str_remove(str_remove(toupper(GISAID_ID_NO_HCOV),
                              "(USA/[[:alpha:]]{2,}-)"),"(/[:digit:]{4})"),
      
      str_detect({{gisaid_id}},"^[[:alpha:]]{2,}-") ~ 
        str_extract(toupper({{gisaid_id}}),"(?<=-)[:graph:]{1,}"),
      
      str_detect({{gisaid_id}},"[[:alpha:]]{2}(?=)") ~ 
        str_remove({{gisaid_id}},"/[:digit:]{4}"),
      TRUE ~ NA_character_
    )) %>%
    
    # Take out any punctuation from the middle portion as it might be different in different datasets
    mutate(GISAID_ID_NO_PUNCT = str_replace_all(toupper(GISAID_ID_MIDDLE),
                                                "[^[:alnum:]]","")) %>%
    
    # Extract the digits only
    mutate(GISAID_ID_DIGIT = case_when(
      # The IDs that are already only numbers need to be treated differently
      is.na(GISAID_ID_MIDDLE) ~ str_extract_all(GISAID_ID_NO_HCOV,"[:digit:]+"),
      TRUE ~ str_extract_all(GISAID_ID_MIDDLE,"[:digit:]+")
    ))
}

#' @export
#' Process_File
#' 
#' @description
#' Read in files depending on type
#' 
#' @param myfile a file or list of files
#' 
process_file <- function(myfile){
  if(str_detect(myfile,"xlsx")){
    read_xlsx(myfile,col_types = "text")
  }else if(str_detect(myfile,"csv")){
    # vroom::vroom(myfile,col_types = "c",altrep = TRUE, .name_repair = janitor::make_clean_names)
    fread(myfile,colClasses = "character",fill=T, sep= ",")
  }else if(str_detect(myfile,"tsv")){
    read_tsv(myfile,col_types = "text")
  }else{
    rio::import(myfile)
  }
}

#' @export
#' safer_process_file
#' 
safer_process_file <- purrr::possibly(process_file,otherwise = "Error in this file my dude")


#' @export
#' Style GT Table
#' 
#' @description
#' Formats used to style a gt table
#' 
style_table <- function(df){
  
  df %>%
    #All column headers are capitalised
    opt_all_caps() %>% 
    #Use the Chivo font
    #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
    opt_table_font(
      font = list(
        google_font("Roboto"),
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
      data_row.padding.horizontal = px(10),
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

#' @export
#' make_json_column
#' 
#' @description make a column with all rows in a json format
#' 
#' @details 
#' This is useful to save the exact raw data in the final table for complete submissions
#' 
#' @param .data a dataframe or data.table
#' 
#' @examples 
#' map(df, ~ purrr::possibly(make_json_column,otherwise = "error_here")(.x))
make_json_column <- function(.data){
  tc <- textConnection("jsontxt", "w")
  stream_out(.data, con=tc)
  .data$raw_inbound_submission <- jsontxt
  close(tc)
  
  return(.data)
}

#' @export
#' 
#' date_cleanup
#' 
#' @description
#' It will convert any date type in completed submissions
#' 
date_cleanup <- function(x){
  case_when(
    # Convert date times with T and Z in them - these are datetimes like 01/01/2021T00::00:00Z
    str_detect({{x}},"T|Z") ~ as.Date(as_datetime({{x}})),
    # Some date formats don't work, like excel dates, so we'll take care of these differently
    str_count({{x}}) == 5 & is.numeric({{x}}) ~ as.Date(openxlsx::convertToDate(as.numeric({{x}}))),
    # Anything else, convert based on it's format. If it's in m/d/y format do that, else d/m/y, etc
    TRUE ~ as.Date( parse_date_time({{x}}, orders = c('mdy', 'dmy','ymd','mdy_HMS')))
  )
  
}