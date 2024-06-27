Overall
- Fix age (round down over 89?)

Tables
- DONE
- Rework CMR indices to have proportion of zeros and non-zero figures

Aim 1
- Look at model coefficients for relevance
- Principal component analysis of scores


Aim 2
- k-fold cross validation
- ROC curve
- Brier scores
- Assessment of interaction
- Sensitivity/specificity calculations
- Calibration curve
- Subgroup analysis

Questions
- Interpreting bivariate analysis results
  - Put variables with p > .25 in the model then do drop1
  
- Merits of using multiple score inputs
  - Complicated. Consider a 3-day average and admission or other possibilities
  
- Modeling indexes as categorical vs. numeric
  - Consider modeling as quartiles and verify assumption of linearity
  
- Should I get fancy with additives, skip right to interactives, 
    - Interaction as last step

- How should modeling decisions affect each other?
  - They should not

- Consider creating composite scoring
- Principal component analysis of scores
  
Paper notes
- Missings excluded
- Older population
- Data fishing
- Removed LOS for clinical insignificance

Notes for Denver
- Filtering ages
- Median vs average
- Removing LOS
- Different scoring ideas
- Using average of last three days cuts sample from ~70k for discharge to ~43k