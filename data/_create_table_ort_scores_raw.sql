-- ORT Scores (raw)
-- read from Research Database

select
  *,
 
  -- Unsure why the cast( ) is required
  cast(row_number() over(
        partition by pat_id
        order by recorded_time) as integer) as rownum
 
from samhsa_ort_patient_ort_scores
 
where 1=1
  -- study begins 2021
  -- last day to enroll is March 31, 2023
  -- study ends April 30, 2024 (minimum of 365 day follow-up for outcomes)
  and recorded_time >= '01/01/2021'
  and recorded_time <= '03/31/2023'
  -- ort total scores
  and flo_meas_id in (17928, 17934)
  and meas_value is not null
