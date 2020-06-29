# TWANG Shiny Tool for Propensity Score Weighting with Two Treatment Groups

This project is a Shiny wrapper for the TWANG package.

# CHANGELOG

## 1.0.1

### Ehancement
* Added ability to work with files where column names contain spaces or special characters.

### Bug-fixes
* Treatment effect estimation
  * Fixed issue where binomial treatment effect estimation wasn't showing results.
  * Fixed issue with missing values in treatment effect estimation. Missing values are now imputed and a warning will be displayed to the user.
