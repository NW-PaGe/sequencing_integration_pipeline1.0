### Daily WDRS Changes
###
### This script loads the most recent and second-most recent SEQUENCING R objects from the DataSnapshots
### folder on the confidential drive, and identifies differences between the two tables.
### Common differences which are handled specifically include changes due to roster uploads, changes in
### case id, and changes in lineages. Roster uploads produce expected changes between the two tables. If
### the script is ran Tuesday - Thursday, yesterday's roster compile file(s) will be loaded and compared
### to the identified changes in the R objects. Case id's that are changed (but Sequencing values all remain
### unchanged) are also identified and put into a separate table. The same is done for changes in lineages
### (sequence_variant_open_text + sequence_notes). Molecular epi will update lineages so these changes are
### expected. All other changes will be printed in a separate table. 
###
### Cases in yesterday's R object that are missing from today's object indicate the sequencing data were likely
### deleted from WDRS. Case in today's R object that are missing from yesterday's object indicate sequencing data
### may have been added to WDRS outside of the rostering process (e.g., manual entry). Cases with changes in today
### AND yesterday's objects indicate there is likely some unexpected change (i.e., not in only CASE_ID or only lineage
### variables). Comparing the sequencing variables between these cases should show what changes were made.
###
### This script will send out a summary email with all categories of changes in WDRS. Tables that include all the
### sequencing variables have variables removed in the email summary, but will have a csv attached to the email
### which includes all the variables. All tables are also saved as dated .rds objects in the Archive folder.


library(dplyr)
library(lubridate)
library(vroom)
library(blastula)
library(gt)
library(flextable)
library(glue)
library(data.table)

folders <- list.dirs('',
                     recursive = F)

folders <- sort(folders, decreasing = T)

# Get the two most recent sequencing files: ---------------------------------------------------
folders_sequence <- ''
count = 0
# increase i +1 for every workday you want to go backward 
# i.e., assuming r objects are produced once per weekday, starting at i = 4 will make "today" equal 4 weekdays ago
i = 0

while (count < 2) {
  i = i+1
  
  file = list.files(folders[i], pattern = '^SEQUENCE.RData$', full.names = T)
  
  if(length(file) == 1) {
    count = count + 1
    folders_sequence[count] <- file
  }
  
}

date_current <- gsub('^.+WDRS/(.+)/SEQUENCE.RData', '\\1', folders_sequence[1])
date_prev <- gsub('^.+WDRS/(.+)/SEQUENCE.RData', '\\1', folders_sequence[2])

load(folders_sequence[1])
SEQUENCE <- SEQUENCE %>% select(CASE_ID, starts_with('SEQUENCE')) %>% mutate(across(everything(), as.character))
seq_current <- SEQUENCE
rm(SEQUENCE)

load(folders_sequence[2])
SEQUENCE <- SEQUENCE %>% select(CASE_ID, starts_with('SEQUENCE')) %>% mutate(across(everything(), as.character))
seq_prev <- SEQUENCE
rm(SEQUENCE)

# Create df's with differences in rows from today and prev object: -----------------------------
seq_diff_current <- seq_current %>% 
  anti_join(seq_prev) %>% 
  mutate(r_obj_date = date_current)

seq_diff_prev <- seq_prev %>% 
  anti_join(seq_current) %>% 
  mutate(r_obj_date = date_prev)

today <- as.Date(gsub('^(.+)?([0-9]{4}-[0-9]{2}-[0-9]{2}).+$', '\\2', date_current))

# get if tues - thurs get yesterday's compiled roster: ----------------------------------------
compiled_roster <- data.frame()
missing_new <- data.frame()

if(wday(today) %in% 3:5) {
  compiled_file <- list.dirs('',
                             recursive = F)
  compiled_file <- compiled_file[grepl(format(today-1, '_%b.{0,} %Y'), compiled_file)]
  compiled_file <- list.files(path = compiled_file, pattern = as.character(today-1), full.names = T)
    
  if(length(compiled_file) == 1) {
    compiled_roster <- vroom(compiled_file) %>% 
      mutate(across(everything(), as.character),
             SEQUENCE_SPECIMEN_COLLECTION_DATE = as.character(mdy(SEQUENCE_SPECIMEN_COLLECTION_DATE)),
             SEQUENCE_STATUS = case_when(SEQUENCE_STATUS == 'COMPLETE' ~ 'Complete',
                                         SEQUENCE_STATUS == 'FAILED' ~ 'Failed',
                                         SEQUENCE_STATUS == 'PENDING' ~ 'Pending',
                                         SEQUENCE_STATUS == 'NOT DONE' ~ 'Not Done',
                                         SEQUENCE_STATUS == 'LOW QUALITY' ~ 'Low Quality',
                                         SEQUENCE_STATUS == 'HIGH CT' ~ 'High CT',
                                         T ~ SEQUENCE_STATUS),
             SEQUENCE_SPECIMEN = case_when(SEQUENCE_SPECIMEN == 'YES' ~ 'Yes',
                                           SEQUENCE_SPECIMEN == 'NO' ~ 'No',
                                           T ~ SEQUENCE_SPECIMEN)) %>% 
      rename('SEQUENCE_ACCESSION_NUMBER' = SEQUENCE_ACCESSION,
             'SEQUENCE_CLINICAL_ACCESSION_NUMBER' = SEQUENCE_CLINICAL_ACCESSION) %>% 
      select(any_of(names(seq_diff_current)))
    
    # Ensure latest compiled roster is included in the diff_current object:
    # If not, there may be problems with roster upload, or sequence data was already included in WDRS
    missing_new <- compiled_roster %>% anti_join(seq_diff_current)
    
    # Remove data missing from new differences from the combiled roster object:
    compiled_roster <- compiled_roster %>% anti_join(missing_new)
    # And then remove the compiled roster data from the new differences. These are expected and don't need to be flagged:
    seq_diff_current <- seq_diff_current %>% anti_join(compiled_roster)
  }
}


# Identify cases with only differences being changes in CASE_ID: -----------------------------
join_vars <- names(seq_diff_prev)
join_vars <- join_vars[!(join_vars %in% c('CASE_ID', 'r_obj_date'))]
case_id_changes <- seq_diff_prev %>% 
  inner_join(seq_diff_current, 
             by = join_vars,
             suffix = c('_prev', '')) %>% 
  select('CASE_ID', 'CASE_ID_prev')

seq_diff_prev <- seq_diff_prev[!(seq_diff_prev$CASE_ID %in% case_id_changes$CASE_ID_prev), ]
seq_diff_current <- seq_diff_current[!(seq_diff_current$CASE_ID %in% case_id_changes$CASE_ID), ]

# Check for lineage updates, usually manually done by mol epi --------------------------------------------------
join_vars <- names(seq_diff_prev)
join_vars <- join_vars[!(join_vars %in% c('SEQUENCE_VARIANT_OPEN_TEXT', 'SEQUENCE_NOTES', 'r_obj_date'))]

lineage_changes <- seq_diff_prev %>% 
  inner_join(seq_diff_current,
             by = join_vars,
             suffix = c('_prev', '')) %>% 
  select(CASE_ID, starts_with('SEQUENCE_VARIANT'), starts_with('SEQUENCE_NOTES'))

seq_diff_prev <- seq_diff_prev[!(seq_diff_prev$CASE_ID %in% lineage_changes$CASE_ID), ]
seq_diff_current <- seq_diff_current[!(seq_diff_current$CASE_ID %in% lineage_changes$CASE_ID), ]


# Rbind all remaining rows: -----------------------------------------
seq_diff_all <- rbind(seq_diff_current, seq_diff_prev) %>% group_by(CASE_ID) %>% arrange(SEQUENCE_ACCESSION_NUMBER) %>% ungroup()

print('Cases with CASE_ID changes:')
glimpse(case_id_changes)

print('Rostered cases successfully uploaded to WDRS:')
glimpse(compiled_roster)

print('Rostered cases with issues entering WDRS or data quality issues within WDRS:')
glimpse(missing_new)

print('Remaining cases with changes in today\'s or the previous sequencing R ojbect:')
glimpse(seq_diff_all)

# delete old csv's sent in previous emails: -----------------------------------------
attachments <- list.files('',
                          pattern = '_attachment.csv',
                          full.names = T)

for(file in attachments) file.remove(file)

# Create email summary --------------------------------------------------------------
if(nrow(compiled_roster) == 0
   & nrow(missing_new) == 0
   & nrow(case_id_changes) == 0
   & nrow(seq_diff_all) == 0
   & nrow(lineage_changes) == 0) {
  message_body <-
    glue(
      "Good Morning,
      
      No WDRS changes have been identified between R objects produced on {date_prev} and {date_current}.
      
      Thanks,
      
      DIQA")
} else {
  
  # Make the email body - make updates to email here if needed
  message_body <-
    glue(
      "Good Morning,

WDRS changes have been identified between R objects produced on {date_prev} and {date_current}.

Below are the summary of changes:


")
  
  if(nrow(seq_diff_all) > 0) {
    
    # save full tables:
    fwrite(seq_diff_all, 
           '')
    saveRDS(seq_diff_all, 
            glue(''))
    
    # shorten col names for summary email:
    names(seq_diff_all) <- gsub('SEQUENCE_', '', names(seq_diff_all))
    
    # Create html table for summary email
    path = ''
    
    seq_diff_all %>% 
      select(CASE_ID, LAB, STATUS, ACCESSION_NUMBER, SPECIMEN_COLLECTION_DATE, CLINICAL_ACCESSION_NUMBER, r_obj_date) %>% 
      flextable::flextable() %>% 
      flextable::bg(bg = '#ebfaeb', part = 'all') %>%
      flextable::bg(bg = '#adebad', part = 'header') %>%
      flextable::save_as_image(path = path)
    
    png_diff_all <- blastula::add_image(path,
                                        width = 800,
                                        align = 'left')
    
    rm(path)
    
    # Add message to email
    message_body <- glue("{message_body}
                             Cases with changes in today's or the previous sequencing R object. See attached 'seq_diff_all' for full table:
                             {png_diff_all}
                             
                             ")
  }
  
  if(nrow(missing_new) > 0) {
    
    # save full tables:
    fwrite(missing_new, 
           '')
    saveRDS(missing_new, 
           glue(''))
    
    # shorten col names for summary email:
    names(missing_new) <- gsub('SEQUENCE_', '', names(missing_new))
    
    # Create html table for summary email
    html_missing <- missing_new %>% 
      select(CASE_ID, LAB, STATUS, ACCESSION_NUMBER, SPECIMEN_COLLECTION_DATE, CLINICAL_ACCESSION_NUMBER) %>% 
      gt() %>% 
      as_raw_html()
    
    # Add message to email
    message_body <- glue("{message_body}
                             Rostered cases with issues entering WDRS or data quality issues within WDRS. See attached 'missing_new' for full table:
                             {html_missing}
                             
                             ")
  }
  
  if(nrow(compiled_roster) > 0) {
    
    # save full tables:
    fwrite(compiled_roster, 
           '')
    saveRDS(compiled_roster, 
            glue(''))
    
    # Create SUMMARY html table for summary email
    library(gtsummary)
    
    path = ''
    
    month_orders = format(seq.Date(as.Date('1800-01-01'), as.Date('3000-01-01'), by = 'month'), '%B %Y') #is this excessive? maybe
    
    compiled_roster %>% 
      mutate(SEQUENCE_SPECIMEN_COLLECTION_DATE = format(as.Date(SEQUENCE_SPECIMEN_COLLECTION_DATE), '%B %Y')) %>% 
      mutate(SEQUENCE_SPECIMEN_COLLECTION_DATE = factor(
        SEQUENCE_SPECIMEN_COLLECTION_DATE,
        levels = month_orders[month_orders %in% SEQUENCE_SPECIMEN_COLLECTION_DATE], 
        ordered = T)
      ) %>% 
      select(SEQUENCE_LAB, SEQUENCE_STATUS, SEQUENCE_SPECIMEN_COLLECTION_DATE) %>%
      gtsummary::tbl_summary(by = SEQUENCE_LAB) %>% 
      gtsummary::as_flex_table() %>% 
      flextable::bg(bg = '#e6f7ff', part = 'all') %>%
      flextable::bg(bg = '#99ddff', part = 'header') %>%
      flextable::save_as_image(path = path)
    
    png_compiled <- blastula::add_image(path,
                                        width = 800,
                                        align = 'left')
    
    rm(path)
    
    # Add message to email
    message_body <- glue("{message_body}
                             Summary of rostered cases successfully uploaded to WDRS. See attached 'compiled_roster' for full table:
                             {png_compiled}
                             
                             ")
  }
  
  if(nrow(lineage_changes) > 0) {
    

    # Add message to email
    message_body <- glue("{message_body}
                         {nrow(lineage_changes)} {ifelse(nrow(lineage_changes) == 1, 'case was', 'cases were')} identified with changes only in lineage. See attached 'lineage_changes' for full table.
                         
                             
                             ")
  }
  
  if(nrow(case_id_changes) > 0) {
    
    # save full tables:

    # Create html table for summary email
    html_case_id <- case_id_changes %>% 
      gt() %>% 
      as_raw_html()
    
    # Add message to email
    message_body <- glue("{message_body}
                         {nrow(case_id_changes)} {ifelse(nrow(lineage_changes) == 1, 'case was', 'cases were')} identified with changes only in case id. See attached 'case_id_changes' for full table.
                         
                         
                             ")
  }
  
  
  
message_body <- glue("{message_body}
                     
                     Thanks,
                     
                     DIQA")
}

# Create email with composed message body:
email <- blastula::compose_email(body = md(message_body))

# Add attachments to email, if any:

if(length(attachments) > 0) {
  for(i in 1:length(attachments)) {
    email <- email %>% add_attachment(attachments[i], filename = gsub('^.+Daily_WDRS_Changes/(.+)_attachment.csv$', '\\1.csv', attachments[i]))
  }
}


# Sending email by SMTP using a credentials file
email %>%
  smtp_send(
    to = c(""),
    from = "",
    subject = "Sequencing - Daily WDRS Changes",
    credentials = creds_key(id = "")
  )
