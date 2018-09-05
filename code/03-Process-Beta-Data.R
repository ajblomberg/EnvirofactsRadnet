#' ---
#' title: "Process Beta data from Envirofacts"
#' author: "Annelise Blomberg"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      toc_float: true
#' ---

#' ## Introduction 
#' We have already downloaded all available air RadNet data from Envirofacts using the API. 
#' This data has been downloaded and saved as "RadNet-Air-Envirofacts.csv"
#' Our next step is to process Beta data. 

#+ load-packages, message = F
library(tidyverse)
library(here)

#+ load-data, message = F
data <- read_csv(here("data", "Clean-RadNet-Air-Envirofacts.csv"), col_types = cols(
        .default = col_character(),
        result_id = col_integer(),
        result_amount = col_double(),
        csu = col_double(),
        result_in_si = col_double(),
        csu_in_si = col_double(),
        mdc_in_si = col_double(),
        ana_num = col_integer(),
        ana_proc_num = col_integer(),
        ana_size = col_double(),
        samp_num = col_integer(),
        proj_num = col_integer(),
        samp_id = col_character(),
        loc_num = col_integer(),
        collect_start = col_date(format = ""),
        collect_end = col_date(format = ""),
        samp_size = col_double(),
        no_of_hours = col_double(),
        half_life = col_double(),
        mat_num = col_integer(),
        study_num = col_integer(),
        mdc = col_double(),
        result_date = col_date(format = "%d-%b-%y")
))


#' We look at the type of data available and the amount. 
#+ analyte-type
(analyte.ids <- data %>% select(analyte_id, analyte_name) %>% unique())
(analyte.freq <- table(data$analyte_id))

#' We select just analyte ids equal to beta. 
#+ select-beta
beta <- data %>% filter(analyte_id == "BETA") 

#' We drop non-detectable results, which make up `r round(100*table(beta$detectable)[[1]]/dim(beta)[[1]], 3)` % of total entries. 
#+ drop-nondetect
table(beta$detectable)

beta <- beta %>% filter(detectable == "Y")

#' ## Select Columns
#' What columns do we want to keep in our beta table? We need to see what is in each column. 
#' 
#' First we start by dropping the following: 
#' 
#' * Columns on the analysis and the sampling.
#' * All results in SI units 
#' * Project details (proj_name, proj_num, proj_id all walk 1-1-1 and do not have useful information)  
#' * Result date, which is after the sample is collected (OR MAYBE WE WANT TO KEEP THIS BECAUSE IT COULD INFLUENCE BETA VALUES???)
#' * Client ID, Event ID and study number - no useful information

#+ drop-columns1
# Examples for columns we are dropping 
beta %>% select(proj_num, proj_id, proj_name) %>% unique()
head(unique(beta$proj_name))

table(beta$event_id)
table(beta$study_num)

# Drop columns 
beta <- beta %>% 
        select(-ana_proc_num, -ana_size, -ana_unit, -ana_size_2, -ana_unit_2, -ana_proc_name) %>% 
        select(-samp_size, -samp_unit, -samp_desc, -samp_num, -samp_id, -no_of_hours) %>% 
        select(-result_in_si, -csu_in_si, -mdc_in_si, -si_unit) %>% 
        select(-proj_name, -proj_num, -proj_id) %>% 
        select(-client_id, -event_id, -study_num) #-result_date

#' We drop additional columns that it we do not need: 
#' 
#' * result_id, ana_type, proc_type_id, mat_num, mat_desc, detectable  
#' 
#' These columns only have one value each, and it is not useful. 

#+ drop-columns2
drop <- c("result_id", "ana_type", "proc_type_id", "mat_num", "mat_desc", "detectable", "half_life", "half_life_time_unit", "crs_id")
unique(beta[drop])

beta <- beta[!(names(beta) %in% drop)]


#' We add a city_state column and rename the state column. We also drop non-continental states.  

beta <- beta %>% 
        rename(state = state_abbr) %>% 
        mutate(city_state = paste0(city_name, ",", state)) %>% 
        filter(!(state %in% c("HI", "AK", "GU", "CNMI", "PR", "ON")))

#' We make a list of all final columns for future reference. 

#+ Summarize-final-variables 
unique.cols <- as.tibble(t(beta %>% summarise_all(funs(n_distinct(.)))), rownames = "variable") %>% 
        rename(unique_count = V1) 

vars.one.value <- (unique.cols %>% filter(unique_count == 1))[[1]]
vars.one.value <- as.tibble(t(beta %>% select(vars.one.value) %>% summarise_all(funs(unique(.)))), rownames = "variable") %>% 
        rename(unique_value = V1)

unique.cols <- unique.cols %>% 
        left_join(vars.one.value, by = "variable")

unique.cols
summary(beta)

#' ## Determine unique location identifiers 
#' We want to know which location identifiers are unique. We have the following count of unique entries for each location column: 
#' 
#' * `r length(unique(beta$city_state))` unique city_states 
#' * `r length(unique(beta$station))` unique station names
#' * `r length(unique(beta$loc_num))` unique location numbers
#' 
#' It appears as though some stations have multiple cities - but this does not make sense. On closer look, we see that 
#' some stations have the same name:
#' 
#'  * Augusta GA and Augusta ME; Portland OR and Portland ME; Richmond CA and Richmond VA; Wilmington DE and Wilmington NC  
#'  
#' We can use loc_num or city_name-station as unique identifiers. 

#+ unique-location-identifiers
location.list <- beta %>% group_by(city_state, station, loc_num) %>% count() %>% rename(n.obs = n)

# Cities with multiple monitors 
beta %>% select(city_state, station) %>% 
        unique() %>% 
        group_by(city_state) %>% 
        count() %>% 
        filter(n > 1) %>% 
        arrange(desc(n))

# Stations with multiple cities?? 
beta %>% select(city_state, station) %>% 
        unique() %>% 
        group_by(station) %>% 
        count() %>% 
        filter(n > 1) %>% 
        arrange(desc(n))

#' ## Cities with multiple monitors 
#' We need to identify cities with multiple monitors and decide what to do.
#' 
#' * For now, we drop duplicate monitors with less than 150 days of data

#+ identify-duplicate-cities
dup.cities <- location.list %>% group_by(city_state) %>% count() %>% filter(n > 1) %>% select(-n) %>% 
        inner_join(location.list, dup.cities, by = "city_state") %>%
        arrange(city_state) %>% 
        ungroup()

drop.loc.num <- dup.cities %>% filter(n.obs < 150) %>% 
        pull(loc_num)
drop.loc.num

dup.cities2 <- dup.cities %>% 
        filter(!loc_num %in% drop.loc.num) %>% 
        filter(city_state != "AUSTIN,TX") # drop Austin because now it only has one monitor

beta2 <- beta %>% filter(!loc_num %in% drop.loc.num)

#' ## Drop measurements before 1987
#' We drop all measurements taken at or before 1986, when Chernobyl happened. No major nuclear accidents have happened since them, except Fukushima

beta3 <- beta2 %>% filter(lubridate::year(collect_start) > 1986)

#' ## Create city list
#' We want a list of all the cities included in our dataset, their number of days of data and their lat/long.
#' We download lat-lon from Google. NOTE: this step used to use an API key, but Google has changed its requirements for API keys. 
#' This code should be re-written as a loop... I just haven't throught it through yet. It sometimes fails when you knit because the later lists have no content.  
#'
#+ city-list, message = FALSE
city.summary <- beta3 %>%
        group_by(city_state, station) %>%
        summarize(n_meas = sum(!is.na(result_amount)),
                  obs_start = min(collect_start),
                  obs_end =  max(collect_end)) 

#' For now, the lat-lon download does not work and is commented out. 
#+ get-lat-long, message = F, warning = F
# library(ggmap)
# latlon <- map(city.summary[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon) <- city.summary[["city_state"]]
# latlon <- bind_rows(latlon, .id = "city_state")
# 
# latlon_fail <- latlon %>% filter(is.na(lon))
# latlon2 <- map(latlon_fail[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon2) <- latlon_fail[["city_state"]]
# latlon2 <- bind_rows(latlon2, .id = "city_state")
# 
# latlon_fail <- latlon2 %>% filter(is.na(lon))
# latlon3 <- map(latlon_fail[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon3) <- latlon_fail[["city_state"]]
# latlon3 <- bind_rows(latlon3, .id = "city_state")
# 
# latlon_fail <- latlon3 %>% filter(is.na(lon))
# latlon4 <- map(latlon_fail[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon4) <- latlon_fail[["city_state"]]
# latlon4 <- bind_rows(latlon4, .id = "city_state")
# 
# latlon_fail <- latlon4 %>% filter(is.na(lon))
# latlon5 <- map(latlon_fail[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon5) <- latlon_fail[["city_state"]]
# latlon5 <- bind_rows(latlon5, .id = "city_state")
# 
# latlon_fail <- latlon5 %>% filter(is.na(lon))
# latlon6 <- map(latlon_fail[["city_state"]], geocode, output = c("latlon"), source = c("google"))
# names(latlon6) <- latlon_fail[["city_state"]]
# latlon5 <- bind_rows(latlon6, .id = "city_state")
# 
# latlon_final <- map(list(latlon, latlon2, latlon3, latlon4, latlon5, latlon6), ~ filter(.x, !is.na(lon))) %>% 
#         bind_rows()
# 
# city.summary2 <- full_join(city.summary, latlon_final, by = "city_state")

#' ## Save Files
#' We save the beta-data and the city-summary files.
#' Remember that these have duplicate days! 

#+ save-final-files
write_csv(beta3, path = here("data", "Clean-RadNet-Beta-byMeas.csv"))
write_csv(city.summary2, path = here("data", "Clean-RadNet-Beta-citylist.csv"))