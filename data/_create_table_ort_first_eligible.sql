/*
The ORT was constructed using a population of **new** patients seeking care at 
a **pain clinic**. This analysis sources new and returning patients seeking 
care at an Emergency Department (ED). To better align the analytic population 
with the model's original intent only patients receiving a prescription for an 
opioid within 30 days before or after an ED visit were eligible.  
The outcome of interest is **incidence** of opioid use disorder (OUD) within 
365 days of the index ED visit. The ORT was developed using only **new** 
patients to the clinic and no exclusions based on a history of OUD were listed 
in the original publication whereas this analysis has excluded patients with a 
previously diagnosed OUD -- **the absence of pre-existing OUD as an eligibility 
criteria in the development cohort leaves open the question of whether the 
population was enriched for outcomes**.
*/
with

ort_score_first as (

  select
    pat_id,
    pat_enc_csn_id as ort_csn,
    recorded_time as ort_time,
    cast(meas_value as integer) as ort_total_score,
    product_type as insurance_product,
    payor_name as insurance_payor,
    
    -- a few scores exceed the maximum value of the ORT (=26)
    case
      when cast(meas_value as integer) >= 26
      then 26
      else cast(meas_value as integer)
    end as ort_total_score,
    
    case
      when cast(meas_value as integer) <= 3 then 'low'
      when cast(meas_value as integer) <= 7 then 'moderate'
      else 'high'
    end as ort_risk_cat,
   
    case
      when product_type in ('COMMERCIAL', 'NON-CONTRACTED COMMERCIAL')
        then 'commercial'
      when product_type in ('MEDICARE', 'MANAGED MEDICARE')
        then 'medicare'
      when product_type in ('MEDICAID', 'MANAGED MEDICAID')
        then 'medicaid'
      when product_type in ('CHARITY')
        then 'hospital-based medical assistance'
      when product_type in ('GRANTS')
        then 'other'
      when product_type in ('GOVERNMENT OTHER')
        then 'other'
      when product_type in ('SELF PAY', 'SELF PAY PENDING')
        then 'self pay'
        -- includes inmate, worker's comp, and liability
      else 'unknown'
    end as insurance
    
  from ort_scores_raw
  
  -- rownum created at table creation (read from Research Database)
  where rownum = 1
  
),


opioids_last_pre_ort as (

  select
    opioid_orders.pat_id,
    max(order_inst) as last_pre_ort_opioid_time
    
  from opioid_orders
  
  inner join ort_score_first
    using (pat_id)
    
  where
    order_inst < ort_time
    and date_diff('day', order_inst, ort_time) between 0 and 30

  group by opioid_orders.pat_id

),

opioids_first_post_ort as (

  select
    opioid_orders.pat_id,
    max(order_inst) as first_post_ort_opioid_time
    
  from opioid_orders
  
  inner join ort_score_first
    using (pat_id)
    
  where
    order_inst >= ort_time
    and date_diff('day', ort_time, order_inst) between 0 and 30

  group by opioid_orders.pat_id

),

smi_first_dates as (
  -- Serious Mental Illness
  
  select
    pat_id,
    min(contact_date) as smi_date
    
  from smi_diagnoses
  
  group by pat_id
  
),

ort_scores_with_covariates as (
  
  select
    ort_score_first.pat_id as pat_id,
    birth_date,
    ort_csn,
    ort_time,
    ort_total_score,
    ort_risk_cat,
    lower(coalesce(sex, 'unknown')) as sex,
    raceth,
    language_primary,
    relationship_status,
    insurance_product,
    insurance_payor,
    insurance,
    smi_date,
    last_pre_ort_opioid_time,
    first_post_ort_opioid_time,

    date_diff('year', cast(birth_date as date), ort_time) as age_yrs,
    
    if(smi_date <= ort_time, 1, 0) as smi_flag,
    
    date_diff('day', last_pre_ort_opioid_time, ort_time)
      as last_pre_ort_opioid_days,
    
    date_diff('day', ort_time, first_post_ort_opioid_time)
      as first_post_ort_opioid_days,
    
    -- Timezero (start of follow-up for outcomes)
    case
      when date_diff('day', last_pre_ort_opioid_time, ort_time) <= 30
        then ort_time
      when date_diff('day', ort_time, first_post_ort_opioid_time) <= 30
        then first_post_ort_opioid_time
      else ort_time
    end as timezero
  
  from ort_score_first
  
    inner join demographics
      using (pat_id)
      
    left join opioids_last_pre_ort
      using (pat_id)
    
    left join opioids_first_post_ort
      using (pat_id)
    
    left join smi_first_dates
      using (pat_id)
    
  -- adults only
  where date_diff('year', cast(birth_date as date), ort_time) >= 18

),

oud_prevalent_dates as (

  select
    pat_id,
    -- most recent opioid use disorder indication in 365 day window
    --  prior to first ORT
    max(oud_date) as oud_prevalent_date
    
  from (
  
    select
      pat_id,
        contact_date as oud_date
        
    from oud_diagnoses
    
    union
    
    select
      pat_id, 
      cast(order_inst as date) as oud_date
      
    from moud_orders

  )
  
  inner join ort_scores_with_covariates
    using (pat_id)
  
  -- Lookback period of 365 days for prevalent opioid use disorder
  where date_diff('day', oud_date, timezero) between 0 and 365
  
  group by pat_id

),

oud_incident_dates as (

  select
    pat_id,
    -- most recent opioid use disorder indication after ORT
    min(oud_date) as oud_incident_date
    
  from (
  
    select
      pat_id,
        contact_date as oud_date
        
    from oud_diagnoses
    
    union
    
    select
      pat_id, 
      cast(order_inst as date) as oud_date
      
    from moud_orders
    
    union
    
    select
      pat_id,
      ndi_oud_death_date as oud_date
      
    from ndi_deaths_with_oud

  )
  
  inner join ort_scores_with_covariates
    using (pat_id)

  -- incident oud after timezero  
  where oud_date > timezero
  
  group by pat_id

)

select
  *,
  -- Adds:
  --oud_prevalent_date,
  --oud_incident_date,
  
  -- eligibility
  if(first_post_ort_opioid_days <= 30
    or last_pre_ort_opioid_days <= 30, 1, 0) as opioid_in_window_30d_flag,
    
  if((first_post_ort_opioid_days <= 30
      or last_pre_ort_opioid_days <= 30)
    and oud_prevalent_date is null, 1, 0) as eligible_flag,
  
  -- prevalent indicator
  if(oud_prevalent_date is not null, 1, 0) as oud_prevalent_flag,

  -- outcome indicator
  if(oud_prevalent_date is null 
    and oud_incident_date is not null, 1, 0) as oud_incident_flag,
  
  date_diff('day', timezero, oud_incident_date) as oud_incident_days

from ort_scores_with_covariates

  left join oud_prevalent_dates
    using (pat_id)
    
  left join oud_incident_dates
    using (pat_id)