# Envirofacts RadNet
Annelise Blomberg
2018-09-05

This set of code has been designed to download all RadNet data availabe from the EPA Envirofacts website. The data is available through an API interface, as described here: https://www.epa.gov/enviro/envirofacts-data-service-api. 

There are several **final files**: 

* RadNet-Air-Envirofacts.csv: all information (sample, analysis and results) for all samples collected by AIR-FILTER and AIR-CHARCOAL.   
    + The unique identifier for this output is "ana_num" (analysis number)  
    + The unique location identifier for this output is either "loc_num" or the combination of "city_state" and "station"  

* Clean-RadNet-Beta-byMeas.csv: clean beta data for future use. This is at the measurement level (ie, has not been expanded to include all dates). Data is limited to Sample dates have been expanded according to the sample duration, duplicate dates have been dropped, and multiple monitors in a city have been accounted for.  
    + The unique identifier for this output is the combination of city_state and date.   
    + The unique location identifier for this output is "city_state"   

* Clean-RadNet-Beta-citylist.csv: a list of beta monitor locations with lat/lon, number of observations and number of days. 

**Code Descriptions**: 

* 01-Investigate-Radnet-Tables.rmd: This script looks at the different types of tables available in RadNet and what data is in each. We identify the four larger tables with RadNet info: ERM_RESULT, ERM_ANALYSIS, ERM_COUNT and ERM_SAMPLE. We also download and save the crosswalk files (e.g., ERM_ANALYTE, ERM_ANA_PROC) and save as .csv files in the data folder. 

* 02-Download-Radnet-Data.rmd: This script downloads three RadNet tables: ERM_RESULT, ERM_ANALYSIS and ERM_SAMPLE (warning: this takes a while). We then merge these tables. Finally, we load the csv crosswalk files and use them to merge in important information. Finally, we limit our final table to analyses conducted as either "AIR-CHARCOAL" and "AIR-FILTER" adn save as a .csv.  

* 03-Process-Beta-Data.R: This script takes the downloaded RadNet data and processes just the beta data. We do the following: 
    + Drop unnecessary columns
    + Summarize all other columns
    + Look at collection_length and days_b4_measurement (... this could be moved to somewhere more logical).
    + Identify cities with multiple monitors and drop those monitors that don't have much data
    + Download lat/long from Google and create a location summary 
NOTE - right now the lat/long download does not work due to a change in Google API - I'm working on fixing this code. For now, I've just saved an old .csv file as a FROZEN version. 
    
 **Using this data**: 
Almost all analyses use this beta data as **daily** data. This is done by expanding the measurement data: if a sample is collected from day A to day D, then all days A-D are assigned the value of that sample. This creates some challenges: 

* If one measurement is from day A to D and another is from day D to F, then day D has two measurements. We usually deal with this by taking a mean of the two values. 

* You should drop outliers, etc before you take averages, so that you don't average in bad points. 
 