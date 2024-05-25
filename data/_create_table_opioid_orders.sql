select
  samhsa_ort_patient_patients.pat_id,
  pat_enc_csn_id,
  order_inst
  
from samhsa_grant_reporting_all_opioid_orders_jps_epic_time

  -- Keep only orders for patients with an ORT
  inner join samhsa_ort_patient_patients
    on samhsa_grant_reporting_all_opioid_orders_jps_epic_time.pat_id = 
      samhsa_ort_patient_patients.pat_id
      
where 1 = 1
  -- study begins 2021 with a 30-day look-back/forward period for opioid orders
  -- (opioid order must be within +/- 30 days of ORT)
  -- study ends April 30, 2024 (minimum of 365 day follow-up for outcomes)
  and order_inst > '11/30/2020'
  and order_inst <= '04/30/2023'
  -- exclude MOUD (an outcome for the analysis)
  and pharm_class_list not like '%opioid withdrawal%'
  and pharm_class_list not like '%opioid antagonists%'