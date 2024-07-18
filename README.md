# Impact of Insurance Status on Hospital Readmission and Mortality Risk Prediction - MPH Capstone Project

## Overview
This project analyzes the performance of HOSPITAL and NEWS2 scores in predicting 30-day readmission and mortality across different insurance categories. It aims to evaluate whether these widely used risk prediction models perform consistently across diverse patient populations, with a focus on insurance status as a proxy for socio-economic factors and healthcare access.

## Key Objectives
1. Examine if HOSPITAL and NEWS2 scores predict 30-day readmission and mortality differently based on insurance status.
2. Evaluate if incorporating insurance status improves the accuracy of readmission and mortality risk prediction models.

## Data
The study uses data from UCHealth hospitals in Colorado for the year 2021, including:
- Patient demographics
- Insurance status
- HOSPITAL and NEWS2 scores
- 30-day readmission and mortality outcomes
Data cannot be accessed without permission from the University of Colorado IRB

## Methods
- Retrospective cohort study
- Logistic regression models
- Subgroup analysis by insurance category
- Model performance evaluation using AUROC, Brier scores, sensitivity, specificity, PPV, and NPV

## Repo Structure
- ~setup.R - Data ingestion and packages
- ~tables.R - Produces descriptive statistic tables
- ~aim1.R - Assesses the primary objective of the analysis
- ~aim2.R - Assesses the secondary objective of the analysis
- ~report.Rmd - Outputs significant analyses for easier interpretation

## Key Findings
- Tests for interaction between insurance status and physiological scores were not significant, but insurance status was a significant covariate, indicating that scores are equally valid for prediction after adjusting for insurance status
- Models were well-calibrated for predicting mortality (AUROC > 0.8), but not readmission (0.7 > AUROC > 0.6) indicating that the scores are useful for mortality, but not readmission
- Stratified analysis suggested that the predictive validity of scores was significantly higher among patients with private insurance or Medicare compared to uninsured patients. Patients with Medicaid also showed lower AUROC, but not significant

## Contributors
- Brendan Phillips
