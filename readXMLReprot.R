rm(list=ls())

fNameSchRep <- paste0("C:\\Users\\roman\\Documents",
                      "\\MTData\\DB_uplift\\Reports\\Reports",
                      "\\MTD-TOL-PROD\\Reports\\Schedules\\TrackingClient",
                      "\\User_00000165.dat")


library(xml2)
data <- read_xml(fNameSchRep)
# Point locations
ReportSchedule <- data %>% xml_find_all("//ReportSchedule")
# App Name
data %>% 
  xml_find_all("//ApplicationName") %>% 
  xml_text() -> appName

# User ID
data %>% 
  xml_find_all("//UserID") %>% 
  xml_text() -> uID

# Schedule report start date
data %>% 
  xml_find_all("//CronReportSchedule//StartDate") %>% 
  xml_text() -> srStDate

# Schedule report end date
data %>% 
  xml_find_all("//CronReportSchedule//EndDate") %>% 
  xml_text() -> srEndDate


# Schedule report last run date
data %>% 
  xml_find_all("//CronReportSchedule//LastRun") %>% 
  xml_text() -> srLRDate


# Schedule report next run date
data %>% 
  xml_find_all("//CronReportSchedule//NextSchedule") %>% 
  xml_text() -> srNRDate

# Schedule report frequency
data %>% 
  xml_find_all("//ReportDateRange//PeriodType") %>% 
  xml_text() -> srRepFreq

# Schedule report time interval
data %>% 
  xml_find_all("//ReportDateRange//PeriodLength") %>% 
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


stringr::str_detect(data, "StartSearchDate")

data %>% 
  xml_find_all("//RequestData") %>% 
  xml_text() -> jnk

datePat <- "([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})[ ]([0-9]{2})[[:punct:]]([0-9]{2})[[:punct:]]([0-9]{2})"

lapply(lapply(lapply(jnk, function(x)str_match_all(x, pattern = datePat)), "[[", 1), "[", 1)
stDate <- unlist(lapply(lapply(lapply(jnk, function(x)str_match_all(x, pattern = datePat)), "[[", 1), "[", 1))
endDate <- unlist(lapply(lapply(lapply(jnk, function(x)str_match_all(x, pattern = datePat)), "[[", 1), "[", 2))


xlmRead <- data.frame(applicationName = appName,
                      userID = uID[-1],
                      reportStartdate = srStDate,
                      reportEndDate = srEndDate,
                      reportLastRan = srLRDate,
                      reportNextRun = srNRDate,
                      reportRunFreq = srRepFreq,
                      reportTimeSpan = srRepTimeInt,
                      reportSourceName = srSourceName,
                      reportName = srReportName,
                      reportSubmitTime = srSubmitTime,
                      StartSearchDate = stDate,
                      EndSearchDate = endDate)

xlmRead

