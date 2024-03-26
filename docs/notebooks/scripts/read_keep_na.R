# ------ Libraries ------ #
box::use(
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
  fs[...],
  tidyr[...],
  stats[na.omit],
  scripts/functions[...]
)

# ------ Read in data ------ # 

#' read all keep_na data
#' 
#' @description
#' only run this if you want to read in all the keep_na files and make a new object
#' 
#' @export
keep_na_extract <- function(){
  
  # There are a couple hundred files in Keep Na, this may take about 5 min to run
  keep_na_files <- fs::dir_ls(file.path(project_folder, "keep_na\\"),
                        recurse = TRUE,
                        type = "file")
  
  # Read in all the keep_na files and put it into a list
  read_keep_na <- map(keep_na_files,safer_process_file) %>%
    # Assign and save the original list (in case there are corrupt files)
    {. ->> keep_na_full_list} %>%
    # Remove any file that failed to be read
    keep(~!inherits(.x, 'character')) %>%
    # Convert all columns to character so all rows can be bound
    rapply(as.character, how = "replace") %>%
    # Bind all rows and attach the file name to the row
    bind_rows(.id = "file_name") %>%
    # Take only distinct SA and SCA values
    distinct(SEQUENCE_ACCESSION,SEQUENCE_CLINICAL_ACCESSION,.keep_all = T) %>%
    # Convert the variant column
    mutate(SEQUENCE_VARIANT_OPEN_TEXT = if_else(!is.na(SEQUENCE_VARIANT),
                                                SEQUENCE_VARIANT,
                                                SEQUENCE_VARIANT_OPEN_TEXT))%>%
    # Trim and remove missing SEQUENCE ACCESSIONS
    mutate(SEQUENCE_ACCESSION = str_trim(SEQUENCE_ACCESSION)) %>%
    # Convert everything to a string format
    mutate(across(everything(), as.character)) %>%
    # Replace all missing with NA. na_strings in the function script has all potential NA's
    mutate(across(where(is.character),
                  ~if_else(. %in% na_strings, NA_character_, .)))

  
  return(read_keep_na)
}

# runs only when script is run by itself
if (sys.nframe() == 0){
  # ... do main stuff
  
  # run keep_na script
  keep_na_df <- keep_na_extract()
  
}
