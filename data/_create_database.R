library(dplyr)
library(duckdb)

here::i_am('validation-ort.Rproj')

con <- odbc::dbConnect(odbc::odbc(), 
                       .connection_string = "driver={SQL Server};server={VMTEPCLARITY201};database={CEHDR_Research};trusted_connection=true")

duckcon <- dbConnect(duckdb(), 'data/ort_data.duckdb')


#############################################################
# RAW DATA
#
#############################################################

for(tble in c('ort_scores_raw',
              'opioid_orders',
              'moud_orders',
              'oud_diagnoses',
              'smi_diagnoses',
              'demographics')) {
  
  # Read from Research Database
  df <- DBI::dbGetQuery(con, 
                        statement = readr::read_file(
                          glue::glue('data/_create_table_{tble}.sql')
                          )
  )
  
  # Write to duckdb
  dbWriteTable(conn=duckcon, name=tble, value=df)
  
  rm(df)
}

#############################################################
# NDI MORTALITY
#
# NDI-provided identifiers (e.g., death certificate number,
#   all data for non-matches with no IRB approval, etc.)
#   must remain on an on-premises server (H://)
#############################################################
pats <- DBI::dbGetQuery(duckcon, "select pat_id, pat_mrn_id
                                  from demographics")

ndi <- 
  vroom::vroom('H:/Documents/ndi/data/ndi-compiled-manifest.csv',
               col_select = c(pat_mrn_id, ndi_state_death_code,
                              ndi_year_death_full, ndi_death_cert_no,
                              ndi_status_code),
               col_types = c('c')) %>% 
  # matches only
  filter(ndi_status_code == 1) %>% 
  distinct()


ndi_cause <- 
  vroom::vroom('H:/Documents/ndi/data/ndi-compiled-cause-of-death.csv',
               col_select = c(pat_mrn_id, ndi_state_death_code,
                              ndi_year_death_full, ndi_death_cert_no,
                              ndi_month_death, ndi_day_death,
                              ndi_cause_underlying,
                              starts_with('ndi_cause_record')),
               col_types = 'c')


ndi_matches <- ndi %>% 
  inner_join(pats %>% distinct(pat_id, pat_mrn_id)) %>% 
  inner_join(ndi_cause, 
             join_by(pat_mrn_id, ndi_state_death_code,
                     ndi_year_death_full, ndi_death_cert_no)) %>% 
  mutate(
    ndi_oud_death_date = 
      as.Date(paste0(ndi_year_death_full, '/', 
                     ndi_month_death, '/',
                     ndi_day_death))
  ) %>% 
  # any OUD listed
  # excludes "unspecified" diagnoses (F19*, T40.[46])
  #   since there is no text description of the diagnoses available
  #   in the NDI data
  filter(if_any(starts_with("ndi_cause_record"), 
                ~ grepl('F11[12]|T40[0123]', .))
  ) %>% 
  select(pat_id, ndi_oud_death_date)


dbWriteTable(duckcon, 'ndi_deaths_with_oud', ndi_matches)

rm(ndi)
rm(ndi_cause)
rm(ndi_matches)
rm(pats)
gc()


#############################################################
# ANALYTIC DATA
#
#############################################################
# Read from Research Database
ort <- DBI::dbGetQuery(duckcon, 
                      statement = readr::read_file(
                        'data/_create_table_ort_first_eligible.sql')
                      )

dbWriteTable(conn=duckcon, name='ort_first_eligible', value=ort)


# 94,983 adult patients
dbGetQuery(duckcon, 
           "select count(distinct pat_id) from ort_first_eligible")

# 21,270 eligible
dbGetQuery(duckcon, 
           "select count(distinct pat_id) from ort_first_eligible
            where eligible_flag = 1")

# 244 outcomes (within 365 days)
dbGetQuery(duckcon, 
           "select count(distinct pat_id) from ort_first_eligible
            where eligible_flag = 1 and oud_incident_days <= 365")

# Double check existing tables
dbGetQuery(duckcon, "show tables")


# Disconnect from database
dbDisconnect(con)
dbDisconnect(duckcon)
