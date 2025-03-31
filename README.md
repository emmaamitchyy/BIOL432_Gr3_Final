# BIOL432_Gr3_Final
BIOL 432 Group 3 Final Project 
Emma Mitchell,  
Grace Wolfe, 
Autumn Hodgins,
Noah Gandl Black, 
Anouk Dimbeanu, 
& Cameron Debellefeuille 

Dataset: https://github.com/ColauttiLab/COVID-Metabolomics 

By understanding the metabolic changes associated with COVID-19 infection, we hope to gain valuable insights into the disease’s underlying mechanisms and potential therapeutic targets.

The study which we sourced our data from (Small-molecule metabolome identifies potential therapeutic targets against COVID-19, Bennet et al., 2022) focuses on analyzing the nasopharyngeal metabolome of patients that exhibited one of three respiratory illnesses: COVID-19, influenza, or respiratory syncytial virus (RSV), as well as a control (no respiratory illness) 

Our goal with this code is to look at the COVID-19 metabolome and compare it to the other groups and discover any potential metabolic patterns in the provided dataset (linked above). To achieve this, we asked three main questions: 
1. Do  COVID-19 patients exhibit distinct metabolite profiles compared to non-COVID-19 controls?
2. How do specific metabolites correlate with the severity of COVID-19? And
3. Are there sex-specific differences in metabolic profiles in all patients?

The answers to these questions and identification of any metabolic patterns can be used to help improve diagnostic accuracy and contribute to the development of target therapies for COVID-19.

To re-run this analysis, be sure to first visit the "Data_Input" folder found in the repository. Here you can find the "data_cleaned.csv" file used throughout our analysis. The python code used to clean the data can be found under "data cleanup test.py".

After the cleaned dataset is loaded in, begin with the "Tuning Boosting parameters.RMD" (for question 1) and follow with the code for each respective code in order (Q1-3). 

For the compiled code (Q1-3) and analysis, see "Final_Report.rmd".
