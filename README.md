# PaediatricPH

## Project Description
This repository contains code to accompany the paper "Pre-operative echocardiography for predicting perioperative cardiorespiratory adverse events in children with idiopathic pulmonary arterial hypertension undergoing cardiac catheterization"

Files included in this repository:
### Developing a Scoring System.R
Identifies which covariates are associated with the occurence of an adverse event and combines them in a multivariable model. Multiple graphics are produced during the bootstrapped samples developing a model. Lastly, uses the median coefficients over the bootstrap to develop a scoring system which can be used clinically

### Column Types.R
Worker file to analyse all the input data and establish the data type for all columns of data from the input file ("Spreadsheet.xlsx").

### Prepare Data.R
Installs and loads the relevant R packages. Defines functions which are then used in the analysis. Loads the data iincluding the raw fields, column types, number of data rows and TAPSE reference ranges to allow conversion to Z scores. Formats the inputted data. Defines the complications and escalations of care and groups of variables relevant to the analysis.

Note: original data is not available due to privacy reasons. Code is provided to demonstrate analysis methods solely.

For more details please contact Tim Dawes at Timothy.Dawes [at] gosh.nhs.uk

## powercalculation.R
Code to calculate necessary sample size to achieve 90% power, alpha = 0.05 based on effect size from Taylor et al, Risk of cardiac catheterization under anaesthesia in children with pulmonary hypertension. BJA, 2007.
