#### Checking missing data part in detail ####

#### Housekeeping ####
rm(list=ls())
dev.off()
gc()
gc()
cat("\014")


#### Load libraries #### 
library(readr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(mice)


####  Specify directory #### 
dataDir <- "C:\\Users\\roman\\Documents\\MTData\\DB_uplift\\output_report_log_summary"


####  Read Data #### 
filesDatDir <- list.files(dataDir)
sumRepLog <- filesDatDir[str_detect(filesDatDir, 
                                    pattern = "summary")]
cat("Reading file ", sumRepLog, "as wd", "\n")
wd <- readr::read_csv(paste0(dataDir,
                             "\\",
                             sumRepLog))
# View(wd)

#### Visualize missing pattern #### 

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(wd)


#### Quantify missing pattern #### 
misPatWd <- mice::md.pattern(wd)
View(data.frame(misPatWd))


wd %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> misCountByVar


wd %>% 
  select_if(function(x) any(is.na(x))) %>% 
  apply(., 2, function(x)which(is.na(x))) -> misRowIdxByVar

#### Custom check of missing values #### 
# This initiaties are to possible filling of missing data which were not possible using 
# the initial approach of data reading from *Console.log.0.zip files
# or even if it was done previously in this script the readr::read_csv
# found that the data are not in legitimate format for further processing
# Read raw data to validate mssingness

valDat <- readr::read_csv(paste0(dataDir,
                                 "\\",
                                 filesDatDir[str_detect(filesDatDir, "raw")]))

tmp <- valDat[misRowIdxByVar$customerID,]
View(tmp)

unique(tmp$repName)

tmp %>%
  filter(repName == "Fatigue.FatigueSummaryReport") -> dataByRepName
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: values don't appear in expected position in initial data read
# Better to fix it here
custID <- as.numeric(unlist(str_match_all(word(dataByRepName$lineContent, -3), pattern = "[0-9]+")))
vehicID <- as.numeric(unlist(str_match_all(word(dataByRepName$lineContent, -2), pattern = "[0-9]+")))

tmp %>%
  filter(repName == "Logistics.MultiFleetTagLocationReport") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: values got unexpectd character with it(') 
# Better to fix it in the initial reading
# Be careful about the number s

tmp %>%
  filter(repName == "SystemReports.UnitAssetReport") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: unusual format 
# Better to leave until get clafrification about the numbers

tmp %>%
  filter(repName == "Maintenance.MaintenanceSummary") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: values got unexpectd character with it(') 
# Better to leave until get clafrification about the numbers

tmp %>%
  filter(repName == "TrackableAssets.TrackableAssetFleetSummary") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: no customerID or vehicleID
# Better to leave it

tmp %>%
  filter(repName == "Custom.Toll.TollSpeedExceptionReport") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: unusual format 
# Better to leave until get clafrification about the numbers
# Better to fix it here

tmp %>%
  filter(repName == "Logistics.JobDetailsByTemplate") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: unusual format 
# Better to leave until get clafrification about the numbers
# Better to fix it here

tmp %>%
  filter(repName == "Vehicle.OOHAndStationaryReport") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: values got unexpectd character with it(') 
# Better to leave until get clafrification about the numbers

tmp %>%
  filter(repName == "Logistics.JobEventReport") -> dataByRepName 
# View(dataByRepName)
head(dataByRepName$lineContent, 3)
tail(dataByRepName$lineContent, 3)
# Reason for missing: values got unexpectd character with it(') 
# Better to leave until get clafrification about the numbers

