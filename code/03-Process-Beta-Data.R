#' ---
#' title: "Process Beta data from Envirofacts (step 3)"
#' author: "Annelise Blomberg"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: html_document
#' ---

#' ## Introduction 
#' We have already downloaded all available air RadNet data from Envirofacts using the API. 
#' This data has been downloaded and saved as "RadNet-Air-Envirofacts.csv"
#' Our next step is to process Beta data. 

#' ## Load Data 

library(tidyverse)
library(here)

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
        mdc = col_double()
))

analyte.ids <- data %>% select(analyte_id, analyte_name) %>% unique()
analyte.freq <- table(data$analyte_id)

beta <- data %>% filter(analyte_id == "BETA") 

#` We drop non-detectable results, which make up `r round(100*table(beta$detectable)[[1]]/dim(beta)[[1]], 3)` % of total entries. 
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
#' * Result date, which is after the sample is collected
#' * Client ID, Event ID and study number - no useful information

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
        select(-result_date, -client_id, -event_id, -study_num)

#' We drop additional columns that it we do not need: 
#' 
#' * result_id, ana_type, proc_type_id, mat_num, mat_desc, detectable  
#'   
drop <- c("result_id", "ana_type", "proc_type_id", "mat_num", "mat_desc", "detectable", "half_life", "half_life_time_unit", "crs_id")
unique(beta[drop])

beta <- beta[!(names(beta) %in% drop)]



#' We add a city_state column and rename the state column. We also drop non-continental states.  

beta <- beta %>% 
        rename(state = state_abbr) %>% 
        mutate(city_state = paste0(city_name, ",", state)) %>% 
        filter(!(state %in% c("HI", "AK", "GU", "CNMI", "PR", "ON")))

#' We make a list of all final columns for future reference. 
#'  

#+ Summarize final variables 
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


#' ## Create all beta dates
#' We assign the beta measurement to all days that it was measured over (e.g., from collect_start to collect_end).
#' First we confirm that "ANA_NUM" is a unique identifier. Then we sequence along from start date to end date for each sample (ANA_NUM).
#' We are using data.table because it is by far the fastest option.

dim(beta)
length(unique(beta$ana_num))

library(data.table)
beta.alldates <- setDT(beta)[ , list(ana_num = ana_num, date = seq.Date(collect_start, collect_end, by = 1)), by = 1:nrow(beta)]
beta.alldates <- as.tibble(beta.alldates[ , c("ana_num", "date")])

#' We join the rest of the beta, drop unnecessary columns, and create a city_state column.
beta.alldates2 <- left_join(beta.alldates, beta, by = "ana_num")

#' Are there any duplicate beta entries when we do this?
#' Yes. Some cities have more duplicate dates than others. This is because the end-date of one sample matches the start-date of the next sample.
#' In other cases, we have multiple measures for the same day. 

dq1 <- beta.alldates2 %>%
        ungroup() %>%
        group_by(city_state, station, date) %>%
        count() %>%
        filter(n > 1)
dq2 <- dq1 %>% 
        select(-n) %>%
        ungroup() %>%
        group_by(city_state) %>%
        count() %>%
        arrange(desc(n))

head(beta %>% filter(city_state == "OAK RIDGE,TN") %>%
        select(city_state, loc_num, collect_start, collect_end, result_amount))

#' When there are days with duplicates, we take the mean of the two measures.
#' We drop ana_num, collect_start and collect_end because all of these are unique to entries with duplicates. 

beta.alldates3 <- beta.alldates2 %>%
        group_by(city_state, city_name, state, station, loc_num, 
                 analyte_id, mat_id, analyte_name, ana_proc_id, result_unit, date) %>% 
        summarize(result_amount = mean(result_amount, na.rm = T), 
                  csu = mean(csu, na.rm = T), 
                  mdc = mean(mdc, na.rm = T)) %>% 
        ungroup()

dim(beta.alldates2)
dim(beta.alldates3)

#' ## Cities with multiple monitors 
#' We need to identify cities with multiple monitors and decide what to do.
#' 
#' * We drop duplicate monitors with less than 150 days of data
#' * We plot cities with multiple monitors and check correlations
#' * In the end, we keep city monitors with correlation > 0.6. For monitors <0.6, we drop overlapping days (to keep the most data). 
#' 
#' In the final dataset, for cities with >1 monitor, we take the mean by day. 
#' 
#+ identify duplicate cities
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

#' We pull in daily data for these cities. 
#' We add an extra column labeling the location numbers as A, B, C etc. for conveninece. 

#+ Add location letters
RenameLocation <- function(df){
        n.unique <- length(unique(df$loc_num))
        loc_num_list <- unique(df$loc_num)
        new_names <- paste0("loc", LETTERS[1:n.unique])
        df2 <- df %>% mutate(loc_letter = plyr::mapvalues(df$loc_num, from = loc_num_list, to = new_names))
        return(df2)
}

dup.city.data <- beta.alldates3 %>%
        inner_join(dup.cities2) 

dup.city.data <- split(dup.city.data, dup.city.data$city_state) %>% 
        map(RenameLocation) %>% 
        bind_rows()

#' We plot the series for all cities with multiple monitors. 

#+ plot duplicate cities, fig.width = 10, fig.height = 10 
plot.dups <- dup.city.data %>%
        filter(date > "1996-01-01") %>%
        ggplot(aes(x = date, y = result_amount, color = as.factor(loc_letter))) +
        geom_point() +
        scale_y_continuous(limits = c(NA, 0.06)) + 
        facet_wrap(~ city_state, ncol = 1)

plot.dups
ggsave("duplicate-beta-monitors.jpg", plot = plot.dups, device = "jpeg", path = here("figures"), width = 10, height = 12)

#' We also plot and save the monitors for all individual cities
#+ plot duplicate cities2

PlotCityMonitors <- function(df, name.df) {
        plot <- df %>% 
                ggplot(aes(x = date, y = result_amount)) +
                geom_point() +
                scale_y_continuous(limits = c(NA, 0.06)) + 
                facet_wrap(~ loc_num, ncol = 1) + 
                title(name.df)
        
        ggsave(paste0("duplicate-monitors-", name.df, ".jpg"), device = "jpeg", path = here("figures"), width = 10, height = 12)
}

city.list <- split(dup.city.data, dup.city.data$city_state) 
walk2(city.list, names(city.list), PlotCityMonitors)


#' It looks like the cities with multiple monitors are generally in agreement. How do we decide what to keep and what to drop?
#' Maybe we check the correlations

#+ Check correlations, fig.width = 8, fig.height = 12
cor <- dup.city.data %>% 
        select(city_state, date, result_amount, loc_letter) %>% 
        spread(key = loc_letter, value = result_amount) 

cor1 <- split(cor, cor$city_state)

PlotCor <- function(df, name.df){
        df <- df %>% select(locA:locE)
        cor <- cor(df, use = "pairwise.complete.obs", method = "spearman")
        cor2 <- cor[!rowSums(cor, na.rm = T)==0,!colSums(cor, na.rm = T)==0]
        corrplot::corrplot.mixed(cor2, title = name.df, mar=c(0,0,2,0))
        return(cor2)
}

par(mfrow = c(3, 2))
cor2 <- map2(cor1, names(cor1), PlotCor)

#' We keep all monitors with correlations greater than 0.6
#' 
#' * All of Oak Ridge (cor > 0.8)
#' * Both of Jackson MS (cor = 0.68)
#' * Both of Las Vegas (cor = 0.71) BUT WE DROP THE ONE SPIKE 
#' * All of Welch MN (no time overlap) 
#' 
#' Montgomery and Phoenix are more tricky. We do the following: 
#' 
#' * Phoenix: drop monitor 4087 
#' * Montgomery, AL: Keep early dates for monitor 1; middle dates for monitor 3992; final dates for monitor 3997


beta.alldates.no.montgomery <- beta.alldates3 %>% 
        filter(!(city_state == "LAS VEGAS,NV" & loc_num == "43" & date > "2005-01-13")) %>% 
        filter(!(city_state == "PHOENIX,AZ" & loc_num == "4087")) %>% 
        filter(city_state != "MONTGOMERY,AL") 

beta.alldates.montgomery <- beta.alldates3 %>% 
        filter(city_state == "MONTGOMERY,AL") %>% 
        group_by(date) %>% 
        filter(rank(loc_num)==1) %>% 
        mutate(year = year(date)) %>% 
        filter(!(loc_num == 1 & year > 2010)) %>% 
        select(-year)

beta.alldates4 <- bind_rows(beta.alldates.no.montgomery, beta.alldates.montgomery) %>% 
        group_by(city_state, city_name, state, 
                 analyte_id, mat_id, analyte_name, ana_proc_id, result_unit, date) %>% 
        summarize(result_amount = mean(result_amount, na.rm = T), 
                  csu = mean(csu, na.rm = T), 
                  mdc = mean(mdc, na.rm = T)) %>% 
        ungroup()

#' ## Save File
#' We do some basic DQ checks and then we save. 

beta.alldates4 %>% group_by(city_state, date) %>% count() %>% arrange(desc(n))
write_csv(beta.alldates4, path = here("data", "Clean-RadNet-Beta.csv"))

