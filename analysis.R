library(dplyr)
library(lubridate)
library(duckdb)
library(ggplot2)
library(ClinicalUtilityRecal)
#library(rmda)


here::i_am('validation-ort.Rproj')

# Output folder
dir.create('figures', showWarnings = FALSE)

# Custom functions for DRY analysis
source('utils.R')


####################
# Analytic datasets
#
####################
con <- dbConnect(duckdb(), 'data/ort_data.duckdb')



# Table 8, Webster
webster <- 
  vroom::vroom('data/webster-table-8-reconstructed.tsv')

# Calculate the implied ORT intercepts
webster.mod.female <- 
  glm(event ~ 1, family='binomial', 
      offset=score, 
      data=subset(webster, sex==0))

webster.mod.male <- 
  glm(event ~ 1, family='binomial', 
      offset=score, 
      data=subset(webster, sex==1))


# ORT (first eligible score)
ort <- dbGetQuery(con, "select *
                        from ort_first_eligible
                        where eligible_flag = 1") %>% 
  mutate(
    
    # sex-specific linear predictor
    #   including intercept estimated from original data
    ort_total_score_corrected = ifelse(
      sex == 'female',
      webster.mod.female$coefficients + ort_total_score,
      #-6.969424 + TOTAL_SCORE,
      #-5.828278 + TOTAL_SCORE
      webster.mod.male$coefficients + ort_total_score),
    
    # predicted probabilities from corrected risk score
    ort_risk_corrected = 
      exp(ort_total_score_corrected) / (1 + exp(ort_total_score_corrected)),
    
    ort_risk_cat_corrected = case_when(
      ort_risk_corrected < 0.048 ~ 'Low',
      ort_risk_corrected < 0.88 ~ 'Moderate',
      TRUE ~ 'High'),
    
    # predicted probabilities implied by the ORT as implemented in EHR
    ort_risk_from_original_model =
      exp(ort_total_score)/(1 + exp(ort_total_score)),
    
    # outcome is incident opioid use disorder within 365 days
    oud_365d_flag = if_else(oud_incident_days <= 365, 1, 0, 0),
    
    oud_365d_label = ifelse(oud_365d_flag == 1, 'Incident OUD', 'No OUD')
  )




################################################################################
#                               DEMOGRAPHICS
#
################################################################################

tbl_by_outcomes <- ort %>%
  select(age_yrs, sex, raceth, language_primary, insurance, smi_flag,
         ort_risk_cat_corrected, oud_365d_label) %>% 
  mutate() %>% 
  gtsummary::tbl_summary(
    by = oud_365d_label,
    
    label = list(
      age_yrs ~ 'Age at ORT',
      sex ~ 'Sex',
      raceth ~ 'Race/Ethnicity',
      language_primary ~ 'Language (preferred)',
      insurance ~ 'Insurance',
      smi_flag ~ 'Serious Mental Illness',
      ort_risk_cat_corrected ~ 'ORT risk category (corrected)'
    )
  ) %>% 
  gtsummary::bold_labels()

tbl_overall <- ort %>%
  select(age_yrs, sex, raceth, language_primary, insurance, smi_flag,
         ort_risk_cat_corrected) %>% 
  gtsummary::tbl_summary(
    label = list(
      age_yrs ~ 'Age at ORT',
      sex ~ 'Sex',
      raceth ~ 'Race/Ethnicity',
      language_primary ~ 'Language (preferred)',
      insurance ~ 'Insurance',
      smi_flag ~ 'Serious Mental Illness',
      ort_risk_cat_corrected ~ 'ORT risk category (corrected)'
    )
  ) %>% 
  gtsummary::bold_labels()


gtsummary::tbl_merge(
  tbls = list(tbl_overall, tbl_by_outcomes),
  tab_spanner = c('Overall', 'By Outcome')
) %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave('figures/table1.docx')




################################################################################
#                               VALIDATION
#
################################################################################

# Binned plot
binned_calibration_plot(
  data = ort,
  p = ort_risk_corrected,
  outcome = oud_365d_flag, 
  nbins = 10)

ggsave(filename='figures/validation-ort-binned-365d.png')


# Smoothed using a GAM
smoothed_calibration_plot(
  data = ort,
  p = ort_risk_corrected,
  outcome = oud_365d_flag)

ggsave(filename='figures/validation-ort-smooth-365d.png')


# Discrimination and Calibration metrics
binary_calibration_metrics(
  data = ort,
  p = ort_risk_corrected,
  outcome = oud_365d_flag,
  xb = ort_total_score_corrected) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/calibration-metrics.docx')



################################################################################
#                               NET BENEFIT
#
################################################################################

#############
# Full Range
#
#############
baseline.model <- rmda::decision_curve(
  formula = oud_365d_flag ~ ort_risk_corrected, 
  data = ort,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  confidence.intervals = 'none')

png('figures/net-benefit-ort-full.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("ORT ~ OUD within 365 days"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.05, 0.02), 
  xlim = c(0, 1)
)
dev.off()

#############
# Detail
#
#############
baseline.model <- rmda::decision_curve(
  formula = oud_365d_flag ~ ort_risk_corrected, 
  data = ort,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  thresholds = c(0.0, 0.04),
  confidence.intervals = 'none')

png('figures/net-benefit-ort-detail.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("ORT ~ OUD within 365 days"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.02, 0.02), 
  xlim = c(0, 0.04)
)
dev.off()


################################################################################
#                                   BY SEX
#
################################################################################
# empty dataframe
metrics_by_sex <- data.frame()

for(sex_cat in c('female', 'male')) {
  
  this_sex <- ort[ort$sex == sex_cat, ]
  
  # Calibration plot
  this_sex_calplot <- smoothed_calibration_plot(
    data = this_sex,
    p = ort_risk_corrected,
    outcome = oud_365d_flag,
    display_plot = FALSE)
  
  ggsave(this_sex_calplot,
         filename=glue::glue('figures/calplot-{sex_cat}-smoothed.png'))
  
  # Calibration metrics
  metrics_by_sex <- bind_rows(
    metrics_by_sex,
    
    binary_calibration_metrics(
      data = this_sex,
      p = ort_risk_corrected,
      outcome = oud_365d_flag,
      xb = ort_total_score_corrected
    ) %>% mutate(SEX = stringr::str_to_title(sex_cat))
  )
  
  # Net benefit
  # { not performed }
  
  rm(this_sex_calplot)
}

tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = SEX,
  data = metrics_by_sex) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-sex.docx')


################################################################################
#                           BY RACE/ETHNICITY
#
################################################################################
# empty dataframe
metrics_by_raceth <- data.frame()

ort <- ort %>% 
  mutate(raceth_binary = ifelse(
    raceth == 'nh white', 'nh white', 'all others'))

for(raceth_cat in c('nh white', 'all others')) {
  
  this_raceth <- ort[ort$raceth_binary %in% raceth_cat, ]
  
  # Calibration plot
  this_raceth_calplot <- smoothed_calibration_plot(
    data = this_raceth,
    p = ort_risk_corrected,
    outcome = oud_365d_flag,
    display_plot = FALSE)
  
  # hyphenated race/ethnicity
  raceth_hyphen <- stringr::str_replace_all(raceth_cat, ' ', '-')
  
  ggsave(this_raceth_calplot,
         filename=glue::glue('figures/calplot-{raceth_hyphen}-smoothed.png'))
  
  # Calibration metrics
  metrics_by_raceth <- bind_rows(
    metrics_by_raceth,
    
    binary_calibration_metrics(
      data = this_raceth,
      p = ort_risk_corrected,
      outcome = oud_365d_flag,
      xb = ort_total_score_corrected
    ) %>% mutate(RACETH = stringr::str_to_title(raceth_cat))
  )
  
  # Net benefit
  # { not performed }
  
  rm(this_raceth_calplot)
}

tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = RACETH,
  data = metrics_by_raceth) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-raceth.docx')
