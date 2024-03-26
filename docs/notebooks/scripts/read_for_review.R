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

# ------ Find For_Review Counts ------ #

# for_review files without the archive
# the for_review archive should be data that has already been explored
# it shouldn't be necessary to read in, but i have code for that below

#' For_Review without Archive
#' 
#' @export
for_review_extract <- function(){
  for_review_files <- fs::dir_ls(file.path(project_folder,"For_Review/to_process"),
                             regexp = "example_template|Archive",
                             invert = T,
                             recurse = T,
                             type="file") 

  # First read in all the files
  for_review_df <- map(for_review_files,safer_process_file) %>%
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
  
  return(for_review_df)
}




#' For_Review with Archive
#' 
#' @export
#' @description
#' Check all of the for_review archive - The Archive should be records we've already processed
for_review_archive_extract <- function(){
  for_review_archive_files <- fs::dir_ls(file.path(project_folder,"For_Review/to_process/Archive"),
                                     regexp = "example_template",
                                     invert = T,
                                     recurse = T,
                                     type="file")
  
  # First read in all the files
  for_review_archive_df <- map(for_review_archive_files,safer_process_file) %>%
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
  
  return(for_review_archive_df)

}

# runs only when script is run by itself
if (sys.nframe() == 0){
  # ... do main stuff
  
  # run for_review
  for_review_archive_extract()
  
  for_review_extract()

}
