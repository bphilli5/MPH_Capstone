Overall
- Consider additional covariates
  - hospital_service???
- Fix age (round down over 89?)

Tables
- Final output
- Heavier drop_na() if one score is chosen

Aim 1
- Assess numeric assumption of indices

- Model age with splines

Aim 2
- All of it

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