---
title: "Initial RadNet Envirofacts Download"
author: "Annelise Blomberg"
date: 'April 13, 2018'
output:
      html_document:
        keep_md: true
        toc: true
---



# Introduction 

We are working on downloading RadNET data from the Envirofacts API (instead of downloading by hand). The EPA has a REST API for interacting with Envirofacts: 
https://www.epa.gov/enviro/envirofacts-data-service-api

I access the data using the package "httr"

There are 11 RadNET tables available from Envirofacts. The following figure is from the EPA website (https://www.epa.gov/enviro/radnet-model), and shows how all the tables connect to each other. 

![RadNet Model](RADNET-model.png)

This markdown file is a quick review of what information is available in each table. 

# Download Table Lengths

We start by making a list of all files and downloading the total counts of entries for each one.  

```r
table.list <- as.list(c("ERM_ANALYTE", "ERM_RESULT", "ERM_ANALYSIS", "ERM_COUNT", "ERM_DET_TYPE", "ERM_ANA_PROC", "ERM_SAMPLE", "ERM_MATRIX", "ERM_LOCATION", "ERM_PROJECT", "ERM_STUDY")) 

GetTableLength <- function(table) {
        url <- "http://iaspub.epa.gov/enviro/efservice" 
        url2 <- paste(url, table, "JSON", "count", sep = "/")
        results <- GET(url2)
        TableLength <- flatten(content(results))[[1]]
}

# test <- GetTableLength("ERM_ANA_PROC")

table.lengths <- map(table.list, GetTableLength)
names(table.lengths) <- table.list
table.lengths2 <- t(bind_rows(table.lengths))
table.lengths3 <- as.tibble(table.lengths2, rownames = "Table") %>% 
        rename(Length = V1) %>% 
        arrange(Length)

library(kableExtra)
knitr::kable(table.lengths3, "html") %>% 
        kable_styling(full_width = F, position = 'left')
```

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Table </th>
   <th style="text-align:right;"> Length </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ERM_MATRIX </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_STUDY </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_ANALYTE </td>
   <td style="text-align:right;"> 65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_ANA_PROC </td>
   <td style="text-align:right;"> 172 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_LOCATION </td>
   <td style="text-align:right;"> 331 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_DET_TYPE </td>
   <td style="text-align:right;"> 424 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_PROJECT </td>
   <td style="text-align:right;"> 1094 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_SAMPLE </td>
   <td style="text-align:right;"> 277959 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_ANALYSIS </td>
   <td style="text-align:right;"> 324868 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_COUNT </td>
   <td style="text-align:right;"> 327312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERM_RESULT </td>
   <td style="text-align:right;"> 512077 </td>
  </tr>
</tbody>
</table>

# Download (7) smaller tables 

From the table lengths, we see that some of the tables are smaller, so must be crosswalks/metadata of a sort rather than sample information. We download these smaller tables. 

```r
small.table.list <- as.list(c("ERM_ANALYTE", "ERM_DET_TYPE", "ERM_ANA_PROC", "ERM_MATRIX", "ERM_LOCATION", "ERM_STUDY", "ERM_PROJECT"))

GetTableContent <- function(table){
        url <- "http://iaspub.epa.gov/enviro/efservice" 
        url2 <- paste(url, table, "CSV", sep = "/")
        results <- GET(url2)
        results.content <- content(results)
        
        # Rename table column names to drop the table prefix
        names.long <- names(results.content)
        names.short <- str_sub(names.long, start = str_length(table) + 2)
        names(results.content) <- names.short
        
        return(results.content)
}

table.content <- map(small.table.list, GetTableContent)
names(table.content) <- small.table.list
```

It looks like every table is loading an empty last-column. Is this the case? 


```r
CheckLastCol <- function(table){
        last.col <- dim(table)[2]
        unique(table[last.col])
}
test_lastcol <- map(table.content, CheckLastCol) 
# Print out first three
test_lastcol[1:3]
```

```
## $ERM_ANALYTE
## # A tibble: 1 x 1
##   ``   
##   <chr>
## 1 <NA> 
## 
## $ERM_DET_TYPE
## # A tibble: 1 x 1
##   ``   
##   <chr>
## 1 <NA> 
## 
## $ERM_ANA_PROC
## # A tibble: 1 x 1
##   ``   
##   <chr>
## 1 <NA>
```

We see that the last column is empty for all tables, so we drop it. 


```r
DropLastCol <- function(table){
        last.col <- dim(table)[2]
        table2 <- table[, -last.col]
}

table.content2 <- map(table.content, DropLastCol)
```

Save the tables into the global environment. 


```r
list2env(table.content2, envir=.GlobalEnv)
```

```
## <environment: R_GlobalEnv>
```

## Look at individual tables 

We have seven tables with crosswalk information. 

ERM_ANALYTE: gives ANALYTE_ID and ANALYTE_NAME for all 65 analyte types. 

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> ANALYTE_ID </th>
   <th style="text-align:left;"> ANA_TYPE </th>
   <th style="text-align:right;"> HALF_LIFE </th>
   <th style="text-align:left;"> HALF_LIFE_TIME_UNIT </th>
   <th style="text-align:left;"> ANALYTE_NAME </th>
   <th style="text-align:left;"> CRS_ID </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> AC228 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> 6.130 </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> Actinium-228 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ALPHA </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Gross Alpha </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AM241 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> 432.200 </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> Americium-241 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BA133 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> 10.500 </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> Barium-133 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BA140 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> 12.789 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Barium-140 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BE7 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:right;"> 53.440 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Beryllium-7 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

ERM_DET_TYPE: I have no idea what this is.

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> DET_NUM </th>
   <th style="text-align:left;"> DET_ID </th>
   <th style="text-align:left;"> DET_TYPE_ID </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> UNK </td>
   <td style="text-align:left;"> UNK </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> AS </td>
   <td style="text-align:left;"> AS </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> HPGE </td>
   <td style="text-align:left;"> HPGE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> GELI </td>
   <td style="text-align:left;"> GELI </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> LBAB </td>
   <td style="text-align:left;"> GPC </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> LBB </td>
   <td style="text-align:left;"> LBB </td>
  </tr>
</tbody>
</table>

ERM_ANA_PROC: this gives a procedure name and procedure ID>. Each procedure is connected to an analysis. 

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> ANA_PROC_NUM </th>
   <th style="text-align:left;"> ANA_PROC_ID </th>
   <th style="text-align:left;"> PROC_TYPE_ID </th>
   <th style="text-align:left;"> ANA_PROC_NAME </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> AIRBETA </td>
   <td style="text-align:left;"> AIRBETA </td>
   <td style="text-align:left;"> Gross beta in air </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> ALPBET </td>
   <td style="text-align:left;"> ALPBET </td>
   <td style="text-align:left;"> Gross alpha-beta </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> ALPHA </td>
   <td style="text-align:left;"> ALPHA </td>
   <td style="text-align:left;"> Gross alpha </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> ALPHACO </td>
   <td style="text-align:left;"> ALPHA </td>
   <td style="text-align:left;"> Gross alpha </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> AM </td>
   <td style="text-align:left;"> AM </td>
   <td style="text-align:left;"> Americium </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> BETA </td>
   <td style="text-align:left;"> BETA </td>
   <td style="text-align:left;"> Gross beta </td>
  </tr>
</tbody>
</table>

ERM_MATRIX: this is the matrix from which the sample was collected. There are only six matrix types (including air filters) 

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> MAT_NUM </th>
   <th style="text-align:left;"> MAT_ID </th>
   <th style="text-align:left;"> MAT_DESC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> PASTEURIZED MILK </td>
   <td style="text-align:left;"> Milk </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> PRECIPITATION </td>
   <td style="text-align:left;"> Precipitation </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:left;"> DRINKING WATER </td>
   <td style="text-align:left;"> Drinking water </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:left;"> SURFACE WATER </td>
   <td style="text-align:left;"> Surface water </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> AIR-CHARCOAL </td>
   <td style="text-align:left;"> Charcoal cartridge </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> AIR-FILTER </td>
   <td style="text-align:left;"> Air filter </td>
  </tr>
</tbody>
</table>

There are 331 ERM_LOCATIONs and 1094 ERM_PROJECTs. Some locations have multiple projects. 

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> LOC_NUM </th>
   <th style="text-align:left;"> STATE_ABBR </th>
   <th style="text-align:left;"> CITY_NAME </th>
   <th style="text-align:left;"> SURFACE_WATER_SOURCE </th>
   <th style="text-align:left;"> STATION </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> AL </td>
   <td style="text-align:left;"> MONTGOMERY </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> MONTGOMERY </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> AL </td>
   <td style="text-align:left;"> BIRMINGHAM </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> BIRMINGHAM </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> AL </td>
   <td style="text-align:left;"> MOBILE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> MOBILE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> AL </td>
   <td style="text-align:left;"> DOTHAN </td>
   <td style="text-align:left;"> Chattahoochee River </td>
   <td style="text-align:left;"> DOTHAN </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> FL </td>
   <td style="text-align:left;"> TALLAHASSEE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> TALLAHASSEE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> FL </td>
   <td style="text-align:left;"> MIAMI </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> MIAMI </td>
  </tr>
</tbody>
</table>

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> PROJ_NUM </th>
   <th style="text-align:right;"> STUDY_NUM </th>
   <th style="text-align:left;"> PROJ_ID </th>
   <th style="text-align:left;"> PROJ_NAME </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> PMN 1978-4 </td>
   <td style="text-align:left;"> PMN 1978-4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> DW 1978-4 </td>
   <td style="text-align:left;"> DW 1978-4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> SW 1978-4 </td>
   <td style="text-align:left;"> SW 1978-4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> TR 1978-3 </td>
   <td style="text-align:left;"> TR 1978-3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> PMNC 1978-4 </td>
   <td style="text-align:left;"> PMNC 1978-4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> SW 1978-3 </td>
   <td style="text-align:left;"> SW 1978-3 </td>
  </tr>
</tbody>
</table>

There 10 ERM_STUDYs. 

<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:right;"> STUDY_NUM </th>
   <th style="text-align:left;"> STUDY_ID </th>
   <th style="text-align:left;"> STUDY_NAME </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> PMN </td>
   <td style="text-align:left;"> RadNet Pasteurized Milk Network </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> RAN </td>
   <td style="text-align:left;"> RadNet Radiation Alert Network </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> TR </td>
   <td style="text-align:left;"> RadNet Precipitation Network </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> RadNet Drinking Water Network </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> SW </td>
   <td style="text-align:left;"> ERAMS Surface Water Network </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:left;"> KR </td>
   <td style="text-align:left;"> ERAMS Kr-85 in Air </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> TR - RAIN </td>
   <td style="text-align:left;"> RadNet TR - Individual rain samples </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:left;"> RAN - PUUA </td>
   <td style="text-align:left;"> RadNet RAN - Annual composites </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:left;"> DWCP </td>
   <td style="text-align:left;"> RadNet DW - Annual composites </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:left;"> FNI </td>
   <td style="text-align:left;"> Fukushima Nuclear Incident </td>
  </tr>
</tbody>
</table>



```r
SaveFiles <- function(table, table.name){
        save.name <- paste0(table.name, ".csv")
        print(save.name)
        write_csv(table, path = here::here("data", save.name))
}

walk2(.x = table.content2, .y = names(table.content2), SaveFiles)
```

```
## [1] "ERM_ANALYTE.csv"
## [1] "ERM_DET_TYPE.csv"
## [1] "ERM_ANA_PROC.csv"
## [1] "ERM_MATRIX.csv"
## [1] "ERM_LOCATION.csv"
## [1] "ERM_STUDY.csv"
## [1] "ERM_PROJECT.csv"
```

# Investigate (4) bigger tables

The largest tables in RadNet Envirofacts are: 

* ERM_RESULT   (512077)  
* ERM_ANALYSIS (324868)  
* ERM_COUNT    (327312)
* ERM_SAMPLE   (277959)

From this, we can see that every sample has at least one analysis, and every analysis has at least one result. 

From the metadata available online, we can see what is in each table without downloading. 

* ERM_RESULT    
    + ANALYTE_ID, ANA_NUM, CSU, MDC  
    + RESULT_AMOUNT, RESULT_DATE, RESULT_ID, RESULT_UNIT


* ERM_ANALYSIS  
    + ANA_COMMENT, ANA_NUM, ANA_PROC_NUM, ANA_SIZE, ANA_UNIT, SAMP_NUM


* ERM_SAMPLE  
    + CLIENT_ID, COLLECT_END, COLLECT_START, LOC_NUM, MAT_ID, PROJ_NUM
    + SAMP_COMMENT, SAMP_DESC, SAMP_ID, SAMP_NUM, SAMP_SIZE, SAMP_UNIT


It looks like most of this will be important. 
