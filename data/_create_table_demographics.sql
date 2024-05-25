
select
  pat_id, 
  pat_mrn_id,
  cast(birth_date as date) as birth_date,
  
  coalesce(pat_sex, 'unknown') as sex,

  case
      when ethnic_group_nm = 'hispanic, latino or spanish ethnicity'
          then 'hispanic'
      when patient_race like '%black%'
          then 'nh black'
      when
          patient_race like 'asian%'
          or patient_race like '%indian%'
          or patient_race like '%hawaiian%'
          or patient_race like '%other%'
          then 'nh other'
      when patient_race like '%caucasian%'
          then 'nh white'
      else 'unknown'
  end as raceth,

  case
      when language_nm = 'english'
          then 'english'
      when language_nm = 'spanish'
          then 'spanish'
      when
          language_nm in (
              'deaf (none asl)',
              'american sign language'
          )
          then 'american sign language'
      when
          language_nm = 'unknown'
          or language_nm is null
          then 'unknown'
      else 'other'
  end as language_primary,

  case
      when
          marital_status_nm in (
              'divorced',
              'legally separated',
              'single',
              'widowed'
          )
          then 'single'
      when
          marital_status_nm in (
              'common law',
              'life partner',
              'married',
              'significant other'
          )
          then 'in a relationship'
      when marital_status_nm = 'other'
          then 'other'
      else 'unknown'
  end as relationship_status

            
from samhsa_ort_patient_patients