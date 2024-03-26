project_folder <- ""

## Connect to WDRS
r_creds <-readRDS(file.path(Sys.getenv("USERPROFILE"), "Projects/Sequencing/Data_Objects", "r_creds.RDS")) 

connection <- DBI::dbConnect(odbc::odbc(), 
                             Driver = r_creds$conn_list[1], 
                             Server = r_creds$conn_list[2], 
                             Database = r_creds$conn_list[3], 
                             Trusted_connection = r_creds$conn_list[4], 
                             ApplicationIntent = r_creds$conn_list[5]
)


## Pull flattened table -- WHERE CDC_N_COV_2019_SEQUENCE_SPECIMEN LIKE '%YES%' -- This is grab all that contain a Yes. 
wdrs_seq_ <- dbGetQuery(connection,
                        "
    SELECT *
    FROM DD_GCD_COVID19_SEQUENCING
    WHERE SEQUENCE_SPECIMEN IS NOT NULL
    AND CASE_STATUS IN (0, 3)
    ORDER BY SEQUENCE_ROSTER_PREPARE_DATE DESC;
                       ")

## Get ACCOUNTABLE_COUNTY
wdrs_flat <- dbGetQuery(
  connection,
  "
       SELECT DISTINCT 
       CASE_ID, 
       ACCOUNTABLE_COUNTY

       FROM [dbo].[DD_GCD_COVID_19_FLATTENED]
        WHERE CDC_N_COV_2019_SEQUENCE_SPECIMEN LIKE '%YES%'
                    "
)

DBI::dbDisconnect(connection)
  
wdrs_seq <- left_join(wdrs_seq_,wdrs_flat,by = "CASE_ID") %>%
  mutate(date = if_else(is.na(SEQUENCE_SPECIMEN_COLLECTION_DATE),
                        as.Date(CASE_CREATE_DATE),
                        as.Date(SEQUENCE_SPECIMEN_COLLECTION_DATE)))


## Getting vectors of standard values and variants - these will be used to compare what values are in WDRS
today <- Sys.Date()
reasons <- c(
  "SENTINEL SURVEILLANCE",
  "S-DROPOUT",
  "SUSPECTED REINFECTION",
  "SUSPECTED VACCINE BREAKTHROUGH" ,
  # "OUTBREAK INVESTIGATION",
  "OUTBREAK",
  "OTHER",
  "TRAVEL ASSOCIATED",
  "UNKNOWN",
  "CLINICAL",
  NA,
  ""
)

# Read in lineages
lineages <- read_csv("",
                     col_names = TRUE,
                     col_types = cols(.default = "c"),
                     na = c("", "NA", "N/A")) 
new_voc <- c("", NA)
variants <- c(lineages$lineage_extracted, new_voc)

# Make the standards for lab names
# Based on this list 
lab_names <- c(
  "Aegis", 
  "Altius",
  "Atlas Genomics",
  "CDC",
  "Fulgent Genetics",
  "Gravity Diagnostics",
  "Helix",
  "Infinity Biologix",
  "Labcorp",
  "OHSU",
  "PHL",
  "NW Genomics",
  "Providence Swedish",
  "Quest",
  "USAMRIID",
  "UW Virology",
  "Naval Health Research Center",
  "Flow Diagnostics",
  "USAFSAM",
  "KP WA Research Inst",
  "Boise VA",
  "Grubaugh Lab",
  "Lauring Lab",
  "ASU",
  "Atlas Genomics",
  "USAMRIID",
  #"Ginkgo",
  #"Yale",
  #"Hyde",
  "UW Virology",
  "OSPHL",
  "Montana Public Health Lab",
  "Grittman Medical Center",
  "IDBOL",
  "Boise VA",
  "PHL/Bedford",
  "SCAN/Bedford",
  "SFS/Bedford",
  "Curative Labs",
  "The Jackson Laboratory")


## PULL IN GISAID
wa_gisaid <- read_rds(file.path(project_folder,"GISAID Data/wa_gisaid.rds")) %>%
  split_gisaid_id(virus_name_clean)

# We need the metadata to confirm that WDRS isn't including records from the wrong state
gisaid_metadata <- fread(file.path(project_folder, "GISAID Data/metadata.tsv.gz"))

# Filter the metadata file and convert the GISAID_ID format so it can be matched to other datasets
gisaid_metadata2 <- gisaid_metadata %>%
  as_tibble() %>%
  filter(country == "USA") %>%
  # Take out HCOV as not all have that string
  mutate(GISAID_ID_NO_HCOV = str_remove(toupper(strain),"HCOV-19/"))%>%
  
  # Extract the middle portion of the ID (digit and lab included. Everything after a state and before the /year)
  mutate(GISAID_ID_MIDDLE = case_when(
    # Hardcode Altius and Aegis
    str_detect(strain,"ALTCOV") ~ str_remove(str_extract(strain,"(?<=ALTCOV-).*"),"(/[:digit:]{4})"),
    str_detect(strain,"ASC[:digit:]{1,}-B[:digit:]{1,}") ~ str_extract(strain,"[^-]+"),
    str_detect(strain,"USA/[[:alpha:]]{2}") ~ str_remove(str_remove(toupper(GISAID_ID_NO_HCOV),"(USA/[[:alpha:]]{2,}-)"),"(/[:digit:]{4})"),
    str_detect(strain,"^[[:alpha:]]{2,}-") ~ str_extract(toupper(strain),"(?<=-)[:graph:]{1,}"),
    str_detect(strain,"[[:alpha:]]{2}(?=)") ~ str_remove(strain,"/[:digit:]{4}"),
    TRUE ~ NA_character_
  ))


# Read in CDC cumulative
cdc_cumulative <- vroom::vroom(file.path(
  project_folder,
  "Submissions\\CDC_Cumulative\\Washington_cumulative.csv"
)) %>%
  split_gisaid_id(GISAID_name)

# Read in Genbank

genbank <- dir_ls(file.path(project_folder,"GenBank Data"),regexp = "wa_genbank_") %>%
  read_csv() %>%
  as_tibble() %>%
  mutate(GISAID_ID = str_replace_all(toupper(`Isolate Lineage`),"SARS-COV-2/HUMAN/","")) %>% 
  split_gisaid_id(GISAID_ID)
