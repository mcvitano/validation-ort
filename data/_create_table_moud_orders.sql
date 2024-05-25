select
  pat_id,
  pat_enc_csn_id,
  order_inst,
  start_date,
  ordering_mode_nm,
  pat_loc_at_order,
  ordering_prov_login_loc,
  atc_code,
  atc_title

from substance_use_medications

where 1=1
  -- buprenorphine or naloxone
  and atc_code in ('N07BC01', 'N07BC51', 'V03AB15')
  and order_status_nm != 'Canceled'