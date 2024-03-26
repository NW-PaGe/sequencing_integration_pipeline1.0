library(pacman)
# options("install.lock"=FALSE)
p_load(
  DBI,
  odbc,
  tidyverse,
  lubridate,
  ggplot2,
  readxl,
  here,
  fs,
  data.table,
  gt,
  viridis,
  kableExtra,
  ggthemes,
  janitor,
  gtExtras,
  blastula,
  keyring,
  curl,
  ggtext,
  webshot2
)

# ------ source in the objects, table reads and functions ------ #
source(here::here("QA/wdrs_qa/wdrs_qa_functions.R"))
source(here::here("QA/wdrs_qa/wdrs_qa_objects.R"))


# ------ apply the qa checks ------ #

#' @details 
#' remove where status is FAILED and missing SA
#' remove deleted cases
#' remove out of state records
#' apply seq_qa_checks
logic_checks <- wdrs_seq %>%
  mutate(failedflag = if_else(
    (toupper(SEQUENCE_STATUS) == "FAILED" &
    is.na(SEQUENCE_ACCESSION_NUMBER)),1,0)
    ) %>%
  filter((failedflag != 1 | is.na(failedflag))) %>%
  filter(CASE_STATUS != 6) %>%
  filter(ACCOUNTABLE_COUNTY != "WA-99") %>%
  seq_qa_checks(specimen = SEQUENCE_SPECIMEN,
                reason = SEQUENCE_REASON,
                lab = SEQUENCE_LAB,
                lineage = SEQUENCE_VARIANT_OPEN_TEXT,
                status = SEQUENCE_STATUS,
                accession = SEQUENCE_ACCESSION_NUMBER,
                clinical_accession = SEQUENCE_CLINICAL_ACCESSION_NUMBER,
                specimen_collection_date = SEQUENCE_SPECIMEN_COLLECTION_DATE,
                case_id = CASE_ID,
                notes = SEQUENCE_NOTES,
                epi_isl = SEQUENCE_EPI_ISL,
                external_repo_accession = wa_gisaid$SEQUENCE_ACCESSION_NUMBER,
                list_of_accepted_lineages = variants,
                list_of_lab_names = lab_names,
                list_of_reasons = reasons
                )
  
# --------- find SA not in GISAID --------- #

#' SA not in GISAID
#' 
#' @description 
#' Flag whenever a whole `SEQUENCE_ACCESSION` number does not match a GISAID ID.
#' 
#' @details 
#' Filters:
#' 1. Filter out data from the past 30 days in case there is a lag 
#'    between what is in WDRS and what is in GISAID
#' 2. Filter out all 'Deleted' (STATUS = 6 cases)
#' 3. Filter out the out of state records in WDRS (ACCOUNTABLE_COUNTY = "WA-99") 
#'   - **There are some records missing ACCOUNTABLE_COUNTY**
#' 4. Filter out accession IDs that exist in the metadata file - 
#' these records may not be in the wa_gisaid object because the sequence was 
#' conducted out of state, even if the case is a WA case
#' 5. Create a flag if `SEQUENCE_ACCESSION` is filled and `SEQUENCE_ACCESSION` 
#'    is not in wa gisaid dataframe

# Filter WDRS records
wdrs_to_join <- wdrs_seq %>%
  filter(date <= (today() - 30)) %>%
  filter(CASE_STATUS != 6) %>%
  filter(ACCOUNTABLE_COUNTY != "WA-99") %>%
  filter(toupper(SEQUENCE_STATUS) != "FAILED") %>%
  filter(!is.na(SEQUENCE_ACCESSION_NUMBER)) %>%
  split_gisaid_id(.$SEQUENCE_ACCESSION_NUMBER) %>%
  # Remove duplicates so they dont match twice
  distinct(.keep_all = T)

# anti_join to wa_gisaid and metadata to make sure all records are accounted for
not_in_gisaid <- anti_join(wdrs_to_join,wa_gisaid,by = "GISAID_ID_NO_HCOV") %>%
  anti_join(gisaid_metadata2, by = "GISAID_ID_NO_HCOV") %>%
  mutate(source = factor("not in gisaid"))


# Flag the data missing from GISAID and check if they are in CDC or GenBank
gisaid_flag <- not_in_gisaid %>%
  mutate(in_cdc = if_else(
    GISAID_ID_NO_HCOV %in% cdc_cumulative$GISAID_ID_NO_HCOV,
    "CDC",
    "No Match"
    )
    ) %>%
  mutate(in_genbank = if_else(
    GISAID_ID_MIDDLE %in% genbank$GISAID_ID_MIDDLE,
    "Genbank",
    "No Match"
    )
    ) %>%
  mutate(source = case_when(
    in_cdc == "CDC" | in_genbank == "Genbank" ~ "In CDC or GenBank (but not GISAID)",
    TRUE ~ "No Match Found" 
  )
  ) %>%
  mutate(source = factor(source, levels = c("No Match Found",
                                            "In CDC or GenBank (but not GISAID)"))) %>%
  mutate(yearsub = lubridate::year(CASE_CREATE_DATE)) %>%
  # Make the flag 
  mutate(gisaid_flag_warn = "Warning: SEQUENCE_ACCESSION not found in GISAID")

# Make a totals dataframe for the ggplot total values
totals <- gisaid_flag %>%
  count(SEQUENCE_LAB,yearsub)

# Make a plot of the WDRS SA's that are not in GISAID
gisaid_flag_plot <- gisaid_flag %>%
  count(source,SEQUENCE_LAB,yearsub) %>%
  ggplot(aes(x= reorder(SEQUENCE_LAB, n, sum),
             y=n,
             fill= source)) +
  geom_col(position = "stack") +
  scale_fill_manual(values =c("lightgrey","black"),labels = label_wrap_gen(width = 20)) +
  geom_text(aes(SEQUENCE_LAB,
                n,
                label = n, 
                fill = NULL),
            data = totals,
            hjust = -.1 ) +
  facet_grid(~ yearsub) +
  coord_flip()+
  labs(
    y = "Number of records not in GISAID",
    x = "",
    title = "<b><span style = 'font-size:14pt;'>WDRS Accession numbers not found in GISAID</span></b><br>The script filters out deleted cases, records with a failed status, out of state records, and removes data from the past 30 days to account for lag. Then it checks if the ID exists in **CDC Cumulative** or **GenBank**.",
  ) +
  theme_bw()+
  expand_limits(y = totals$n + 800) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      maxwidth = unit(6,"in"),
      hjust = .0005,
      size = 10,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ),
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 12,
      color = "white", fill ="black", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    ),
    legend.title = element_blank(),
    axis.text.y = element_markdown(face = "bold",size = 12))


## Output a CSV of the table
fwrite(gisaid_flag,paste0(project_folder,"/WDRS QA/Logic Checks","/","gisaid_flag",today(),".csv"))


# ------- Create an output to be sent back to labs ------- #

#' Create output
#' 
#' @description
#' create outputs to request labs to upload to the data to GISAID
#' 
#' @details 
#' Need to collect and merge strings that are similar for labs 
#' example, all labs that contain 'CDC' should be CDC
#' Take all NW Genomics (SEATTLE FLU STUDY, ATLAS, SEATTLE CHILDRENS) and
#' CDC submitting labs and output their originating labs. That's who will be contacted
std_unmatched <- gisaid_flag %>%
  mutate(SEQUENCE_LAB = standardize_lab_names(lab = SEQUENCE_LAB)) %>%
  mutate(SEQUENCE_LAB = if_else(
    str_detect(toupper(SEQUENCE_LAB), "SEATTLE FLU|ATLAS|SEATTLE"),
    "NW Genomics",
    SEQUENCE_LAB
    )
    )%>%
  mutate(SEQUENCE_LAB = if_else(
    str_detect(toupper(SEQUENCE_LAB), "CENTERS FOR DISEASE"),"CDC",SEQUENCE_LAB))

# Take out date and originating lab
std_unmatched_clean <- std_unmatched %>%
  # filter(match_to_gisaid == "Unmatched") %>%
  # filter(match_to_gisaid == "Unmatched" & match_to_cdc == "Unmatched") %>%
  filter(source == "No Match Found") %>%
  filter(yearsub > '2020') %>%
  # filter(!(toupper(SEQUENCE_STATUS) == "LOW QUALITY" & is.na(SEQUENCE_VARIANT_OPEN_TEXT)))%>%
  select(CASE_ID,
         SEQUENCE_ACCESSION_NUMBER,
         SEQUENCE_LAB,SEQUENCE_REASON,
         SEQUENCE_STATUS,
         SEQUENCE_SPECIMEN_COLLECTION_DATE,
         SEQUENCE_VARIANT_OPEN_TEXT)

# Make a column for abbreviated names
std_unmatched_clean$name3 <- makeInitials(std_unmatched_clean$SEQUENCE_LAB)

# Get a list of labs that have variants
labs <- std_unmatched_clean %>%
  mutate(SEQUENCE_LAB = case_when(
    str_detect(SEQUENCE_LAB,"Bedford") ~ "PHL",
    TRUE ~ SEQUENCE_LAB)
  ) %>%
  count(SEQUENCE_LAB)


# Output files that didn't match GISAID
if(nrow(std_unmatched_clean)!=0){
  
  # create a new folder
  newday <- paste0(project_folder,"WDRS QA/No Match To GISAID/", Sys.Date())
  # apply a new file by lab - see @output function
  mapply(output,lab = labs$SEQUENCE_LAB)
  
} else{
  print("gap data df is empty my dude")
}


# -------- Append GISAID Flag to Logic Checks -------- #

# Clean up the gisaid_flag file
gisaid_flag_final <- gisaid_flag %>%
  select(CASE_ID,SEQUENCE_ACCESSION_NUMBER,gisaid_flag_warn)

# append to logic_checks
logic_checks2 <- logic_checks %>%
  left_join(gisaid_flag_final,by= c("CASE_ID","SEQUENCE_ACCESSION_NUMBER"))

# -------- Combine each warning into one column-------- #

#' @details 
#' Since one row/event can have more that one flag, we need to paste 
#' the flags together to be able to show all flags in one row. 
#' This also allows us to put all flags in one column to make the data easier to query.

final <- logic_checks2 %>%
  unite("flag",dplyr::ends_with('_warn'),na.rm=TRUE,remove=FALSE) %>%
  mutate_all(as.character) %>%
  mutate_all(na_if,"") %>%
  filter(str_detect(flag, "Warning")) %>%
  # mutate(SEQUENCE_SPECIMEN_COLLECTION_DATE = as.Date(SEQUENCE_SPECIMEN_COLLECTION_DATE,"%m/%d/%Y"))%>%
  mutate(date = if_else(
    is.na(SEQUENCE_SPECIMEN_COLLECTION_DATE),
    as.Date(CASE_CREATE_DATE),
    as.Date(SEQUENCE_SPECIMEN_COLLECTION_DATE)))%>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  select(-dplyr::ends_with('.x'))%>%
  select(-dplyr::ends_with('.y'))



# ------- Special Requests ------- #

#' @details 
#' Output a list of obs that have status = 6 and FAILED/Low QUALITY
#' Update a list of all deleted excluding FAILED
DeletedCases<- wdrs_seq %>%
  mutate(failedflag = if_else((
    toupper(SEQUENCE_STATUS) %in% c("COMPLETE", "LOW QUALITY") &
      is.na(SEQUENCE_ACCESSION_NUMBER)), 1, 0)) %>%
  filter((CASE_STATUS == 6 |
            is.na(CASE_STATUS)) & (failedflag == 1 | is.na(failedflag)))

if(nrow(DeletedCases)!=0){
  DeletedCases %>%
    fwrite(
      paste0(project_folder,"WDRS QA/DeletedCases_",
             today(),
             ".csv"
      )
    )
  print("Send this list to end users that these observations have status=6 and are COMPLETE or LOW QUALITY")
} else{
  print("No DeletedCases, skip me!")
}



# -------- Create Outputs For End Users -------- #

#' @description 
#' Prep the summary table
sum_table_raw<- final %>%
  select(CASE_ID,year,dplyr::ends_with('_warn')) %>%
  gather(key, value, ends_with('_warn'), na.rm = TRUE)%>%
  count(value,year) %>% 
  pivot_wider(names_from = year,values_from = n) %>%
  replace(is.na(.), 0) %>%
  rowwise()%>%
  mutate(Total = sum(c_across(where(is.numeric))))%>%
  mutate(value = str_remove(value,"Warning: "))%>%
  arrange(desc(Total))

# Pull in the previous table for comparisons
last_table_file <- dir_info(
  path = file.path(project_folder,"WDRS QA/Logic Checks"),
  recurse = TRUE,
  type = "file",
  regexp = "summary_table.*csv") %>%
  slice(which.max(as.Date(.$modification_time, '%Y-%m-%d',tz = "PST8PDT")))

last_table <- read_csv(last_table_file$path)

sum_table_clean <- full_join(sum_table_raw,last_table,by = "value") %>%
  replace(is.na(.), 0) %>%
  mutate(Change = Total.x - Total.y) %>%
  select(-(contains(".y"))) %>%
  # Remove all the .x from the columns left over
  rename_all(
    list(
      ~str_replace_all(.,'.x',''))
  ) %>%
  # Relocate the columns
  relocate(value,
           `2020`,
           `2021`,
           `2022`,
           `2023`,
           Total,
           Change)

# Create Summary Table
sum_table<- gt(sum_table_clean) %>%
  cols_label(value = "Error Type") %>%
  tab_header(title = md("Summary of COVID-19 Sequence Data Errors in WDRS")) %>%
  gtExtras::gt_fa_rank_change(Change, font_color = "match",fa_type = "arrow") %>%
  tab_footnote(
    footnote = "Change in error counts compared to previous QA check outputs",
    location = cells_column_labels("Change")) %>%
  #Add a data source footnote
  tab_source_note(source_note = "*Year is derived from SPECIMEN_COLLECTION_DATE. If missing, CREATE_DATE is used")%>%
  data_color(columns= Total,
             colors=scales::col_numeric(
               palette = c("#FFFFFF",
                           viridis(
                             350,
                             alpha=1,
                             begin=0,
                             end=1,
                             direction=-1
                             )
                           ),domain=NULL )
             )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c('2020','Change')
      )
    )
  ) %>%
  style_table()

gtsave(sum_table,
       file.path(project_folder,"WDRS QA/Logic Checks/sum_table_image.png"),
       vwidth = 1300,
       vheight = 1200 )

# Output the sum table from this week in order to make comparisons to next week's table
fwrite(sum_table_raw,file= paste0(project_folder,
                                  "WDRS QA/Logic Checks/summary_table_",
                                  today(),
                                  ".csv"))

# Flag CSV Output
final_csv <- final %>%
  select(-failedflag,flag)

# Save the csv with all the errors
fwrite(final_csv,file= paste0(project_folder,
                              "WDRS QA/Logic Checks/Logic_Checks_",
                              today(),
                              ".csv"))


# -------- Send Email -------- #
source(here::here("QA/wdrs_qa/wdrs_qa_email.R"))
