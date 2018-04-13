---
title: "Download Envirofacts RadNet: step 2"
author: "Annelise Blomberg"
date: 'April 13, 2018'
output:
      html_document:
        keep_md: true
        toc: true
---



## Introduction 
We are downloading RadNet information from the Envirofacts API. The initial documentation has already been downloaded (crosswalk tables), and we have looked at the bigger tables to see what information is available. 
Big Tables

* ERM_RESULT
* ERM_ANALYSIS
* ERM_SAMPLE

Downloads have to be smaller than 10,000 entries long. How can we split up the downloads? You can combine up to three tables, and then download in chunks of 10,000. 

## Download ERM_RESULT, ERM_ANALYSIS and ERM_SAMPLE 

We could theoretically download all three together, because they have matching columns. 

First we check to make sure column length is similar. 


```r
GetTableLength <- function(tables) {
        tables = paste(tables, collapse = "/")
        url <- "http://iaspub.epa.gov/enviro/efservice" 
        url2 <- paste(url, tables, "JSON", "count", sep = "/")
        results <- GET(url2)
        TableLength <- flatten(content(results))[[1]]
}


print(GetTableLength(c("ERM_RESULT")))
```

```
## [1] 512077
```

```r
print(GetTableLength(c("ERM_ANALYSIS")))
```

```
## [1] 324868
```

```r
print(GetTableLength(c("ERM_SAMPLE")))
```

```
## [1] 277959
```

```r
print(GetTableLength(c("ERM_RESULT", "ERM_ANALYSIS")))
```

```
## [1] 512063
```

```r
print(GetTableLength(c("ERM_RESULT", "ERM_ANALYSIS", "ERM_SAMPLE")))
```

```
## [1] 512063
```

If we download all three tables at once, we only lose 14 number of entries. So we can download all three at once. 


```r
seqlast <- function(from, to, by){
        vec <- do.call(what = seq, args = list(from, to, by))
        
        if(tail(vec, 1) != to){
                return(c(vec, to))
        }else{
                return(vec)
        }
}

MakeUrls <- function(tables){
        TableLength <- GetTableLength(tables)
        
        # Format download lengths
        download.rows <- tibble(start = seqlast(0, TableLength, by = 10000)) %>% 
                mutate(end = lead(start),
                start = start + 1) %>% 
                filter(!is.na(end)) %>% 
                mutate(end = format(end, scientific = F, trim = T),
                       start = format(start, scientific = F, trim = T),
                       final = paste(start, end, sep = ":"))
        
        download.rows <- download.rows[["final"]]
        
        tables.paste <- paste(tables, collapse = "/")
        url <- "http://iaspub.epa.gov/enviro/efservice"
        url2 <- paste(url, tables.paste, "rows", download.rows, "CSV", sep = "/")
}

DownloadData <- function(url){
        results <- GET(url)
        print(results$status_code)
        content <- content(results)
        
        # Check if last column is empty
        last.col <- dim(content)[2]
        last.col.cont <- unique(content[last.col])
        
        if(is.na(last.col.cont)){
                content <- content[, -last.col]
        } else {
                content <- content
        }
        
        return(content)

}

ShortenNames <- function(content, tables) { 
        names.long <- names(content)
        names.short <- map(names.long, str_replace, pattern = array(paste0(tables, ".")), replacement = "") %>% 
                map( ~ .x[which.min(str_length(.x))]) %>% 
                unlist()
        names(content) <- names.short
        return(content)
}
```

We go ahead and download the three complete tables: ERM_RESULT, ERM_ANALYSIS and ERM_SAMPLE. (running time: ~ 20 minutes)


```r
# Create URLS
erm.result.tables <- c("ERM_RESULT", "ERM_ANALYSIS", "ERM_SAMPLE")
erm.result.urls <- MakeUrls(erm.result.tables)

# Download data 
erm.result <- map(erm.result.urls, possibly(DownloadData, NA, quiet = FALSE))
```

```
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 404
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
## [1] 200
```

```r
# Pull out any URLs that failed and try again
failed.urls <- erm.result.urls[which(is.na(erm.result))]
erm.result.try1 <- erm.result[which(!is.na(erm.result))]
erm.result.try2 <- map(failed.urls, possibly(DownloadData, NA, quiet = FALSE))
```

```
## [1] 200
```

```r
# Bind rows for the two attempts
erm.result.try1 <- data.table::rbindlist(erm.result.try1)
erm.result.try2 <- data.table::rbindlist(erm.result.try2)

erm.result.full <- data.table::rbindlist(list(erm.result.try1, erm.result.try2))
erm.result.full <- ShortenNames(erm.result.full, erm.result.tables)

print(dim(erm.result.full))
```

```
## [1] 512062     35
```

Notice that when you download two tables at the same time, Envirofacts gives you the column that they are matched on twice. We check that the column is a duplicate and then delete. 

In the case of ERM_RESULT and ERM_ANALYSIS, the joining column is "ANA_NUM".
In the case of ERM_ANALYSIS and ERM_SAMPLE, the joining column is "SAMP_NUM"


```r
colnames(erm.result.full)
```

```
##  [1] "ANA_NUM"       "RESULT_ID"     "ANALYTE_ID"    "RESULT_AMOUNT"
##  [5] "CSU"           "MDC"           "RESULT_UNIT"   "RESULT_DATE"  
##  [9] "RESULT_IN_SI"  "CSU_IN_SI"     "MDC_IN_SI"     "SI_UNIT"      
## [13] "DETECTABLE"    "ANA_NUM"       "SAMP_NUM"      "ANA_PROC_NUM" 
## [17] "ANA_SIZE"      "ANA_UNIT"      "ANA_SIZE_2"    "ANA_UNIT_2"   
## [21] "ANA_COMMENT"   "SAMP_NUM"      "PROJ_NUM"      "SAMP_ID"      
## [25] "CLIENT_ID"     "LOC_NUM"       "COLLECT_START" "COLLECT_END"  
## [29] "SAMP_SIZE"     "SAMP_UNIT"     "MAT_ID"        "SAMP_DESC"    
## [33] "SAMP_COMMENT"  "NO_OF_HOURS"   "EVENT_ID"
```

```r
names(erm.result.full)[1]
```

```
## [1] "ANA_NUM"
```

```r
names(erm.result.full)[15]
```

```
## [1] "SAMP_NUM"
```

```r
names(erm.result.full)[1] <- "ANA_NUM.2"
names(erm.result.full)[15] <- "SAMP_NUM.2"

# Check that two columns are identical
identical(erm.result.full$ANA_NUM, erm.result.full$ANA_NUM.2)
```

```
## [1] TRUE
```

```r
identical(erm.result.full$SAMP_NUM, erm.result.full$SAMP_NUM.2)
```

```
## [1] TRUE
```

```r
# Drop extra column
erm.result3 <- erm.result.full[, -c("ANA_NUM.2", "SAMP_NUM.2")]
```

## Join in Crosswalk Information 

We load the other crosswalk information and merge. 

First, we load the crosswalk tables we are interested in. Then we add the ERM_RESULT (and analysis and sample) tables to the list, putting them first to facilitate the merging order. 


```r
xw <- list("ERM_ANALYTE", "ERM_ANA_PROC", "ERM_MATRIX", "ERM_LOCATION", "ERM_PROJECT")
xw.files <- paste0(xw,".csv")

xw.files <- map(here::here("data", xw.files), read_csv)
names(xw.files) <- xw


xw.files$ERM_RESULT <- erm.result3
xw.files <- xw.files[c("ERM_RESULT", "ERM_ANALYTE", "ERM_ANA_PROC", "ERM_MATRIX", "ERM_LOCATION", "ERM_PROJECT")]

merged <- Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2), xw.files)
```

## Limit final table and save

We know that we are only interested in information collected from air, so we limit our final file to MAT_ID "AIR-CHARCOAL" and "AIR-FILTER" 


```r
merged <- merged %>% 
        filter(MAT_ID %in% c("AIR-CHARCOAL", "AIR-FILTER")) %>% 
        select(-SAMP_COMMENT, - ANA_COMMENT, -SURFACE_WATER_SOURCE)

write_csv(merged, here::here("data", "RadNet-Air-Envirofacts.csv"))
```

