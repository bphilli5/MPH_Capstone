hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                        paste(hospital_scores_read, collapse = " + "),
                        paste(read_predictors, collapse = " + "))
