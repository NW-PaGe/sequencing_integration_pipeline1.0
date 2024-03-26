# ------ Libraries ------ #
box::use(
  lubridate[...],
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

# ----- Find Fuzzy Match Counts ----- #

#' Fuzzy files
#' @description
#' Read in all fuzzy files from file storage
#' 
#' @export
fuzzy_extract <- function(){
  fuzzy_files <- fs::dir_ls(file.path(project_folder,"For_Review/to_process_fuzzy_match"),
                          regexp = "example_template",
                          invert = T,
                          recurse = T,
                          type="file") 

  # First read in all the files
  fuzzy_df <- map(fuzzy_files,safer_process_file) %>%
    # Remove the files that are corrupt or unable to read in
    keep(~!inherits(.x, 'character')) %>%
    # Replace all columns as character so we can bind the rows
    rapply(as.character, how = "replace") %>%
    # Bind rows and assign each row the file name it comes from
    bind_rows(.id="file_name") %>%
    # Convert everything to a string format
    mutate(across(everything(), as.character)) %>%
    # Replace all missing with NA. na_strings in the function script has all potential NA's
    mutate(across(where(is.character),
                  ~if_else(. %in% na_strings, NA_character_, .)))
  
  return(fuzzy_df)
}

# runs only when script is run by itself
if (sys.nframe() == 0){
  # ... do main stuff
  
  # run this script
  fuzzy_extract()
  
}