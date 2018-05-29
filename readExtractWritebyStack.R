# Read log files and extract information from report broker

source('C:/Users/roman/Documents/MTData/DB_uplift/dirStructure.R')

fList <- dirStr$files2Read
length(fList)

outputLoc <- paste0(baseDir,
                   "\\",
                   "output_report_log_summary")

# for(stk in 1:1){
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
    rbf <- read_lines(file = workFile)
    
    repoExeLines <- rbf[stringr::str_detect(string = rbf, pattern = "SQL : EXEC ")]
    
    # patRepGenDateNTime <- ".*(?=(?:([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})))"
    patRepGenDateNTime <- "([0-9]{4})[-]([0-9]{2})[-]([0-9]{2})[ ]([0-9]{2})[[:punct:]]([0-9]{2})[[:punct:]]([0-9]{2})"
    lineExtract <- str_split(unlist(str_extract(repoExeLines, patRepGenDateNTime)), pattern = ",")
    repDate <- stringr::str_trim(stringr::word(unlist(sapply(lineExtract, "[", 1)), 1))
    repTime <- stringr::str_trim(stringr::word(unlist(sapply(lineExtract, "[", 1)), 2))
    
    # patRepExtDateNTime <- ".*(?=(?:([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})))"
    infoExtract <- str_split(repoExeLines, pattern = ",")
    numOneInRep <- as.numeric(stringr::str_trim(stringr::word(unlist(sapply(infoExtract, "[", 2)), 1)))
    numLastInRep <- as.numeric(stringr::str_trim(stringr::word(unlist(sapply(infoExtract, "[", 2)), -1)))
    numLast <- as.numeric(stringr::str_trim(unlist(sapply(infoExtract, "[", 3))))
    
    extDatePat <- "([0-9]{2})[/]([0-9]{2})[/]([0-9]{4})[ ]([0-9]{2})[[:punct:]]([0-9]{2})[[:punct:]]([0-9]{2})"
    tmp <- str_match_all(repoExeLines, extDatePat)
    emptyTmp <- unlist(lapply(tmp, is_empty))
    tmp[emptyTmp][[1]] <- matrix(NA, nr = 2, nc = 7)
    tmp[which(unlist(lapply(tmp, nrow)) < 2)][[1]] <- matrix(NA, nr = 2, nc = 7)
    extRepPeriodDate <- sapply(tmp, "[", 1)
    extRepPeriodTime <- sapply(tmp, "[", 2)
    extRepStDate <- sapply(extRepPeriodDate, function(x)word(x,1))
    extRepStTime <- sapply(extRepPeriodDate, function(x)word(x,2))
    extRepEndDate <- sapply(extRepPeriodTime, function(x)word(x,1))
    extRepEndTime <- sapply(extRepPeriodTime, function(x)word(x,2))
    
    repTypePat <- "INFO (.+) SQL"
    repText <- str_extract(repoExeLines, repTypePat)
    repName <- gsub(pattern = " - SQL", replacement = "", 
                    gsub(pattern = "INFO  MTData.Transport.Service.Reports.TrackingReports.",
                         replacement = "",
                         x = repText))
    
    workDF <- data.frame(repDate = repDate,
                         repGenTime = repTime,
                         repExecuteTime = numOneInRep,
                         repName = repName,
                         customerID = numLastInRep,
                         vehicleID = numLast,
                         repPeriodStartDate = extRepStDate,
                         repPeriodStartTime = extRepStTime,
                         repPeriodEndDate = extRepEndDate,
                         repPeriodEndTime = extRepEndTime,
                         stackName = stringr::word(nameRef, 1))
    if(str_detect(nameRef, pattern = "MT")){
      workDF$tenancy = "MT"
    } else workDF$tenancy = "ST"
    # View(na.omit(workDF))
    # workDF <- na.omit(workDF)
    
    linesRead <- data.frame(lineContent = repoExeLines,
                            repDate = repDate,
                            repGenTime = repTime,
                            repExecuteTime = numOneInRep,
                            repName = repName,
                            customerID = numLastInRep,
                            vehicleID = numLast,
                            stackName = stringr::word(nameRef, 1))
    
    sumOutFileName <- paste0("summaryRepLog_", 
                          stringr::word(files[1],1),
                          "_to_",
                          stringr::word(files[length(files)],1),
                          ".csv")
    
    rawOutFileName <- paste0("rawRepLogLinesExtracted_", 
                             stringr::word(files[1],1),
                             "_to_",
                             stringr::word(files[length(files)],1),
                             ".csv")
    
    if(file.exists(paste0(outputLoc, "\\", sumOutFileName))){
      write_csv(x = workDF, path = paste0(outputLoc, "\\", sumOutFileName), append = TRUE)
    }else write_csv(x = workDF, path = paste0(outputLoc, "\\", sumOutFileName))
    
    if(file.exists(paste0(outputLoc, "\\", rawOutFileName))){
      write_csv(x = linesRead, path = paste0(outputLoc, "\\", rawOutFileName), append = TRUE)
    }else write_csv(x = linesRead, path = paste0(outputLoc, "\\", rawOutFileName))
  }
}