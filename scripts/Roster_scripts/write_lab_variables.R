library(tidyverse)
library(here)


# get file location for current file
current_location <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
# get file path for local repo
sequencing_repo <- file.path(dirname(current_location))


# If running in production, set test to false
test <- FALSE

if(test == FALSE){
  # Set the folder to Network location
  project_folder <- readRDS(file.path(sequencing_repo, "Data_Objects//dir_object.RDS"))
} else{
  # If running in test mode, read from and write to your local folder
  project_folder <- sequencing_repo
}


cdc_labs <- c("Quest",
              "Labcorp",
              "Fulgent Genetics",
              "Infinity",
              "Helix",
              "Aegis",
              "University Of Washington Medical Center Laboratory",
              "UW Virology")

input_seq_reasons_cdc <- c("SENTINEL SURVEILLANCE",
                           "SUSPECTED REINFECTION",
                           "SUSPECTED VACCINE BREAKTHROUGH",
                           "OUTBREAK",
                           "OUTBREAK INVESTIGATION",
                           "OTHER",
                           "TRAVEL ASSOCIATED",
                           "UNKNOWN",
                           "",
                           NA)

# sequence reasons acceptable in a final roster for cdc labs. apply this list in quality checks and filters
output_seq_reasons_cdc <- c("SENTINEL SURVEILLANCE",
                            "SUSPECTED REINFECTION",
                            "SUSPECTED VACCINE BREAKTHROUGH",
                            "OUTBREAK",
                            "TRAVEL ASSOCIATED",
                            "OTHER",
                            "UNKNOWN"
)

# possible input sequence reasons for PHL data sent to fuzzy matching, not all of these reasons are used for output
input_seq_reasons_phl <- c("SENTINEL SURVEILLANCE",
                           "SUSPECTED REINFECTION",
                           "SUSPECTED VACCINE BREAKTHROUGH",
                           "OUTBREAK",
                           "OUTBREAK INVESTIGATION",
                           "OTHER",
                           "S-DROPOUT",
                           "TRAVEL ASSOCIATED",
                           "TRAVEL",
                           "PHL DIAGNOSTIC",
                           "UNKNOWN"
)


# output sequence reasons accepted for rostered data
# Order of this list matches aligns with the phl_seq_reasons list
# to convert from the input to output format
output_seq_reasons_phl <-  c(
  "SENTINEL SURVEILLANCE",
  "SUSPECTED REINFECTION",
  "SUSPECTED VACCINE BREAKTHROUGH",
  "TRAVEL ASSOCIATED",
  "TRAVEL ASSOCIATED",
  "OUTBREAK",
  "S-DROPOUT",
  "OTHER",
  "SENTINEL SURVEILLANCE",
  "UNKNOWN"
)

# sequence reasons expected in the input PHL data
# The order of this list matches with the output_seq_reasons_phl list
# to map from the input reasons to the accepted output format
phl_seq_reasons <- c("SENTINEL SURVEILLANCE",
                     "SUSPECTED REINFECTION",
                     "SUSPECTED VACCINE BREAKTHROUGH",
                     "TRAVEL ASSOCIATED",
                     "TRAVEL",
                     "OUTBREAK INVESTIGATION",
                     "S-DROPOUT",
                     "OTHER",
                     "PHL DIAGNOSTIC",
                     "PT",
                     "UNKNOWN") 

# sequence reasons that are mapped as they are for NON-CDC labs
input_seq_reasons_non_cdc <- c(
  "SENTINEL SURVEILLANCE",
  "SUSPECTED REINFECTION",
  "SUSPECTED VACCINE BREAKTHROUGH",
  "TRAVEL ASSOCIATED",
  "OUTBREAK",
  "OUTBREAK INVESTIGATION",
  "OTHER",
  "CLINICAL",
  "",
  NA
)

output_seq_reasons_non_cdc <-  c(
  "SENTINEL SURVEILLANCE",
  "SUSPECTED REINFECTION",
  "SUSPECTED VACCINE BREAKTHROUGH",
  "TRAVEL ASSOCIATED",
  "OUTBREAK",
  "OTHER",
  "CLINICAL"
)

# sequence reasons that are transformed to SENTINEL SURVEILLANCE, apply in conditional transformations using mutate(case_when())
# for use with PHL and cdc labs
seq_reason_sent_surveillance <- c("PHL DIAGNOSTIC", "")

# sequence reasons mapped to OUTBREAK INVESTIGATION, use with all labs
seq_reason_outbreak <- c("OUTBREAK INVESTIGATION")


# Sequencing lab names 
# These are the lab names as spelled in WDRS
# When a new lab is added, add the name to the lab_names_wdrs list for QA check in ROSTER_COMPILE.Rmd 
# This list was originally based on the SequencingDataMapping Excel file, LABNAMES sheet
# Updates should be made from the GitHub wiki General Sequencing Information with current list of labnames 
 lab_names_wdrs  <-  c("Aegis",
                       "Altius",
                       "Atlas Genomics",
                       "ASU",
                       "CDC",
                       "Curative Labs",
                       "Fulgent Genetics",
                       "Gravity Diagnostics",
                       "Helix",
                       "Infinity Biologix",
                       "Labcorp",
                       "NW Genomics",
                       "OHSU",
                       "PHL",
                       "Providence Swedish",
                       "Quest",
                       "USAMRIID",
                       "UW Virology",
                       "The Jackson Laboratory",
                       "Naval Health Research Center",
                       "Flow Diagnostics",
                       "Madigan Army Medical Center",
                       "USAFSAM",
                       "KP WA Research Inst",
                       "Boise VA",
                       "Grubaugh Lab",
                       "Lauring Lab", 
                       "Curative",
                       "IDBOL",
                       "OSPHL", 
                       "Incyte Diagnostics Spokane",
                       "Montana Public Health Lab")

 # TEMPLATE SUBMITTERS
 # Labs submitting via template (SUBMITTING_LAB)
 lab_names_template  <-  c(
   "Aegis", "Aegis Sciences Corporation",
   "Altius",
   "ASU",
   "Atlas Genomics",
   "Boise VA",
   "CDC",
   "Curative Labs",
   "Fulgent Genetics",
   "Gravity Diagnostics",
   "Helix",
   "Infinity", "Infinity Biologix",
   "KP WA Research Inst",
   "Labcorp",
   "Lauring Lab",
   "NW Genomics",
   "OSPHL",
   "PHL",
   "Quest",
   "USAFSAM",
   "UW Virology", 
   "UW Virology Lab",
   "University Of Washington Medical Center Laboratory"
 )
 
 # Valid values for SEQUENCE_REASON (NA/NULL not included, records cannot be missing reason)
 seq_reason_template <- c(
   "S-DROPOUT",
   "SUSPECTED REINFECTION", 
   "SUSPECTED VACCINE BREAKTHROUGH", 
   "SENTINEL SURVEILLANCE", 
   "OUTBREAK", 
   "OTHER",
   "CLINICAL"
 )
 
# Valid values for SEQUENCE_STATUS (NA/NULL not included, no record should be missing SEQUENCE_STATUS)
 seq_status_template <- c(
   "COMPLETE", 
   "PENDING", 
   "NOT DONE", 
   "LOW QUALITY", 
   "HIGH CT", 
   "FAILED"
 )
 
# ELR
# Labs submitting via ELR
lab_names_elr <- c(
  "Aegis Sciences Corporation",
  "Helix Diagnositics", # this is the spelling submitted via ELR
  "Laboratory Corporation Of America (LabCorp)",
  "Quest San Juan Capistrano Laboratory",
  "UW Virology",
  "University Of Washington Medical Center Laboratory"
)
 
# PREPARE FINAL OBJECT
lab_vars <- list(cdc_labs = cdc_labs,
                 input_seq_reasons_cdc = input_seq_reasons_cdc,
                 output_seq_reasons_cdc = output_seq_reasons_cdc,
                 input_seq_reasons_phl = input_seq_reasons_phl,
                 output_seq_reasons_phl = output_seq_reasons_phl,
                 phl_seq_reasons = phl_seq_reasons,
                 input_seq_reasons_non_cdc = input_seq_reasons_non_cdc,
                 output_seq_reasons_non_cdc = output_seq_reasons_non_cdc,
                 seq_reason_sent_surveillance = seq_reason_sent_surveillance,
                 seq_reason_outbreak = seq_reason_outbreak,
                 lab_names_wdrs = lab_names_wdrs,
                 lab_names_template = lab_names_template,
                 seq_reason_template = seq_reason_template,
                 seq_status_template = seq_status_template,
                 lab_names_elr = lab_names_elr)


write_rds(lab_vars, file.path(project_folder, "Data_Objects/lab_variables.rds"))
