# UofTeco1400project
UofT Econ MA Econometrics project

This project uses two data files;
CEX data from the replication package of "Scarred Consumption" article and
Nondurables Price index from FRED, CUUR0000SAN.

CEX data file is quite large so we have the Data preprocessing.R that generates the CEX_Cleaned.rds we have uploaded here which is <5MB.
annual_nondurables_1980_2012.csv is calculated from the montly indices in the CUUR0000SAN.csv via averaging using the Nondurables yearly calculation.R

QR with CIs.R produces the quantile regressions for our 2 models, calculates the confidence intervals and stores them in results_nondurables_ci.rds and results_total_ci.rds for future use as this process takes a bit over 72 mins with ~12GB ram usage (using Google Colab).

QR Plotting from RDS files.R generates the graphs used in our project.
