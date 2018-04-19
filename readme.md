# Envirofacts RadNet
Annelise Blomberg
2018-04-13

This set of code has been designed to download all RadNet data availabe from the EPA Envirofacts website. The data is available through an API interface, as described here: https://www.epa.gov/enviro/envirofacts-data-service-api. 

There are several final files: 

* Clean-RadNet-Air-Envirofacts: all information (sample, analysis and results) for all samples collected by AIR-FILTER and AIR-CHARCOAL.   
    + The unique identifier for this output is "ana_num" (analysis number)  
    + The unique location identifier for this output is either "loc_num" or the combination of "city_state" and "station"  

* Clean-RadNet-Beta: clean beta data for future use. Sample dates have been expanded according to the sample duration, duplicate dates have been dropped, and multiple monitors in a city have been accounted for.  
    + The unique identifier for this output is the combination of city_state and date.   
    + The unique location identifier for this output is "city_state"   

