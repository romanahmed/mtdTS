# Read log files and extract information from report broker

# Directory structure and related information
rm(list=ls())
gc()
cat("\014")
library(tidyverse)
library(stringr)
library(lubridate)
library(xml2)


baseDir <- "C:\\Users\\roman\\Documents\\MTData\\DB_uplift"
stackDir <- "Reports\\Reports"

dirNames <- list.files(paste0(baseDir, "\\", stackDir))
stackName <- stringr::str_replace(dirNames, pattern = "-PROD", replacement = "")
# mtStack <- str_detect(string = dirNames, pattern = "MT")

# Check existence of zip folder
dataDir <- "Reports\\Schedules\\TrackingClient"
dirExtCheck <- file.exists(paste0(baseDir, 
                                  "\\", 
                                  stackDir, 
                                  "\\", 
                                  dirNames, 
                                  "\\", 
                                  dataDir))
if(!all(dirExtCheck)){
  stop("The TrackingClient directory doesn't exists for all stacks")
} else{"The TrackingClient directory exists for all stacks"}

fileCount <- unlist(lapply(paste0(baseDir,
                                  "\\", 
                                  stackDir, 
                                  "\\", 
                                  dirNames, 
                                  "\\",
                                  dataDir), 
                           function(x)length(dir(x))))
if(!all(fileCount>0)){
  stop("The TrackingClient directory is not populated for all stacks")
} else{"The TrackingClient directory contains files in all stacks"}


fileNames <- lapply(paste0(baseDir,
                           "\\",
                           stackDir,
                           "\\",
                           dirNames,
                           "\\",
                           dataDir),
                    function(x)dir(x))

usefulFiles <- ".dat"
ufIDx <- lapply(fileNames, function(x){stringr::str_detect(x, pattern = usefulFiles)})
srLogs <- mapply("[", fileNames, ufIDx, SIMPLIFY = F)
names(srLogs) <- dirNames
srLogsCount <- unlist(lapply(srLogs, length))

stackReportInfo <- data.frame(directory = dirNames,
                              stack = stackName,
                              dirFileCount = fileCount,
                              srFileCount = srLogsCount)

stackReportInfo

files2Read <- srLogs


dirStr <- list(stackInfo = stackReportInfo,
               files2Read = files2Read)


fList <- dirStr$files2Read
length(fList)

outputLoc <- paste0(baseDir,
                    "\\",
                    "output_report")


extXml <- function(d = data){
  # User ID
  data %>% 
    xml_find_all("//UserID") %>% 
    xml_text() -> uID
  
  
  # extData <- tibble::tibble(
  #   uID = xml2::xml_text(xml2::xml_find_first(data, "//UserID")),
  #   appName = xml2::xml_text(xml2::xml_find_first(data, "//ApplicationName"))
  # )
  
  if(length(uID) == 1) next
  
  # App Name
  data %>% 
    xml_find_all("//ApplicationName") %>% 
    xml_text() -> appName
  
  # Schedule report start date
  data %>% 
    xml_find_all("//StartDate") %>% 
    xml_text() -> srStDate
  
  
  # Schedule report end date
  data %>% 
    xml_find_all("//EndDate") %>% 
    xml_text() -> srEndDate
  
  
  
  # Schedule report last run date
  data %>% 
    xml_find_all("//LastRun") %>% 
    xml_text() -> srLRDate
  
 
  # Schedule report next run date
  data %>% 
    xml_find_all("//NextSchedule") %>% 
    xml_text() -> srNRDate
  
  
  # Schedule report frequency
  data %>% 
    xml_find_all("//PeriodType") %>% 
    xml_text() -> srRepFreq
  
  
  # Schedule report time interval
  data %>% 
    xml_find_all("//PeriodLength") %>% 
    xml_text() -> srRepTimeInt
  
  
  
  # Schedule report source name
  data %>% 
    xml_find_all("//SourceName") %>% 
    xml_text() -> srSourceName
  
  
  # Schedule report name
  data %>% 
    xml_find_all("//ReportName") %>% 
    xml_text() -> srReportName
  
  
  # Schedule report submit time
  data %>% 
    xml_find_all("//SubmitTime") %>% 
    xml_text() -> srSubmitTime
  
  
  data %>% 
    xml_find_all("//RequestData") %>% 
    xml_text() -> tmpDate
  
  datePat <- "([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})[ ]([0-9]{2})[[:punct:]]([0-9]{2})[[:punct:]]([0-9]{2})"
  stDate <- unlist(lapply(lapply(lapply(tmpDate, 
                                        function(x)str_match_all(x, pattern = datePat)), "[[", 1), "[", 1))
  
  endDate <- unlist(lapply(lapply(lapply(tmpDate, 
                                         function(x)str_match_all(x, pattern = datePat)), "[[", 1), "[", 2))
  
  if(length(uID) == length(appName)){uId <- uID}else uID <- uID[-1]
  if(is_empty(srStDate)) srStDate <- rep(NA, length(uID))
  if(is_empty(srEndDate)) srEndDate <- rep(NA, length(uID))
  if(is_empty(srLRDate)) srLRDate <- rep(NA, length(uID))
  if(is_empty(srLRDate)) srLRDate <- rep(NA, length(uID))
  if(is_empty(srRepFreq)) srRepFreq <- rep(NA, length(uID))
  if(is_empty(srRepTimeInt)) srRepTimeInt <- rep(NA, length(uID))
  if(is_empty(srSourceName)) srSourceName <- rep(NA, length(uID))
  
  if(is_empty(srReportName)) srReportName <- rep(NA, length(uID))
  if(is_empty(srSubmitTime)) srSubmitTime <- rep(NA, length(uID))
  if(is_empty(stDate)) stDate <- rep(NA, length(uID))
  if(is_empty(endDate)) endDate <- rep(NA, length(uID))
  
  xmlRead <- data.frame(applicationName = appName,
                        userID = uID,
                        reportStartdate = srStDate,
                        reportEndDate = srEndDate,
                        reportLastRan = srLRDate,
                        reportNextRun = srNRDate,
                        reportRunFreq = srRepFreq,
                        reportTimeSpan = srRepTimeInt,
                        reportSourceName = srSourceName,
                        reportName = srReportName,
                        reportSubmitTime = srSubmitTime,
                        startSearchDate = stDate,
                        endSearchDate = endDate)
  return(xmlRead)
}


for(stk in 1:length(fList)){
  nameRef <- names(fList)[stk]
  files <- fList[[stk]]
  # for(fl in 1:3){
  for(fl in 1:length(files)){
    workFile <- paste0(paste0(baseDir,
                              "\\",
                              stackDir,
                              "\\",
                              dirNames,
                              "\\",
                              dataDir)[stk],
                       "\\",
                       files[fl])
    cat("working with file", workFile, "\n")
    

    data <- read_xml(workFile)
    
   
    readXMLData <- try(extXml(data))
    
    if(class(readXMLData) == "try-error") next
    
    xmlRead <- data.frame(readXMLData,
                          stackID = nameRef,
                          fileRef = files[fl])
    
    oututFileName <- paste0("xmlExtractedFeatures",
                             ".csv")
    
    if(file.exists(paste0(outputLoc, "\\", oututFileName))){
      write_csv(x = xmlRead, path = paste0(outputLoc, "\\", oututFileName), append = TRUE)
    }else write_csv(x = xmlRead, path = paste0(outputLoc, "\\", oututFileName))
  }
}



