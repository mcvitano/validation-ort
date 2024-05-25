select
  pat_id,
  pat_enc_csn_id,
  contact_date,
  diagnosis_source
  
from substance_use_diagnosis_list

where
  -- opioid use disorder
  current_icd10_list like '%F11.1%'
  or current_icd10_list like '%F11.2%'
  -- overdose
  -- only T40[0123]X[14] or T404[12][14]
  -- source data has been pre-filtered
  or current_icd10_list like '%T40.0%'
  or current_icd10_list like '%T40.1%'
  or current_icd10_list like '%T40.2%'
  or current_icd10_list like '%T40.3%'
  or current_icd10_list like '%T40.41%'
  or current_icd10_list like '%T40.42%'
  -- other psychoactive or unspecified
  or (
    (current_icd10_list like '%F19.1%'
      or current_icd10_list like '%F19.2%'
      --  only T406[09][14] or T4049[14]
      -- sourrce data has been pre-filtered
      or current_icd10_list like '%T40.49%'
      or current_icd10_list like '%T40.6%')
    and
    (dx_name like '%opiate%'
      or dx_name like '%opioid%'
      or dx_name like '%heroin%'
      or dx_name like '%methadone%')
  )

