## Report log start 2 completion summary ----

# Read log files and extract information from report broker
# source('C:/Users/roman/Documents/MTData/DB_uplift/dirStructure.R')


fList <- dirStr$files2Read
length(fList)

outputLoc <- paste0(baseDir,
                    "\\",
                    "output_report_log_summary")

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
    # cat("working with file", workFile, "\n")
    rbf <- read_lines(file = workFile)
    
    # Mapping
    
    stringsForLineExtract <- c("Starting task",
                               "Starting Excel",
                               "report complete",
                               "TrackingAPI",
                               "GetReportOutput")

    # Summarize the Hawk-Eye reports
    # Hawk-eye report start and end time can be found from follwoing line sets
    reportStartLines <- rbf[stringr::str_detect(string = rbf, 
                                                pattern = "Starting task")]

    stTasks <- stringr::str_extract(reportStartLines, "\\[[^()]+\\]") %>%
      substring(., 2, nchar(.)-1)
    repIDStartLine <- as.numeric(gsub(",", "", word(sub(".*Starting task for report *(.*?) *,*", "\\1", 
                                                        reportStartLines), 1)))
    
    stRepName <- str_trim(unlist(lapply(str_split(string = sub(".*Starting task for report *(.*?) *for.*", 
                                                               "\\1", 
                                                               reportStartLines),
                                                  pattern = ",") , "[", 2)))
    timeStTask <- format(as.POSIXct(paste0(word(reportStartLines, 1),
                                           " ",
                                           gsub(word(reportStartLines, 2),
                                                pattern = ",",
                                                replacement = "."))), 
                         ("%Y-%m-%d %H:%M:%OS3"))
    userIdStTask <- as.numeric(gsub(".* user (.*) userName.*", "\\1", reportStartLines))
  
    startingTasks <- tbl_df(data.frame(taskID = stTasks,
                                       taskStart = timeStTask,
                                       task4user = userIdStTask,
                                       repID = repIDStartLine,
                                       repNames = stRepName,
                                       stackName = stringr::word(nameRef, 1))) %>% 
      group_by(repID, repNames, stackName) %>% 
      filter(n() == 1) %>%
      ungroup()
    

    excelStartLines <- rbf[stringr::str_detect(string = rbf, 
                                                pattern = "Starting Excel")]
    
    compTasks <- stringr::str_extract(excelStartLines, "\\[[^()]+\\]") %>%
      substring(., 2, nchar(.)-1)
    
    repIDdBquery <- as.numeric(gsub(",", "", word(sub(".*Excel task for report *(.*?) *,*", "\\1", 
                                                      excelStartLines), 1)))
    
    
    dbQyeryEndRepName <- str_trim(unlist(lapply(str_split(string = sub(".*Excel task for report *(.*?) *for.*", 
                                                                       "\\1", 
                                                                       excelStartLines), 
                                                          pattern = ",") , "[", 2)))
    
    timeCpTask <- format(as.POSIXct(paste0(word(excelStartLines, 1),
                                           " ",
                                           gsub(word(excelStartLines, 2),
                                                pattern = ",",
                                                replacement = "."))), 
                         ("%Y-%m-%d %H:%M:%OS3")) 
  
    
    completionTasks <- tbl_df(data.frame(taskID = compTasks,
                                         taskEnd = timeCpTask,
                                         repID = repIDdBquery,
                                         repNames = dbQyeryEndRepName,
                                         stackName = stringr::word(nameRef, 1)))%>% 
      group_by(repID, repNames, stackName) %>% 
      filter(n() == 1) %>%
      ungroup()

    HErepExecTime <- inner_join(completionTasks%>%
                                   select(-taskID),
                                 startingTasks,
                                 by = c("repID", "repNames", "stackName")) %>% # Completed Hawk-Eye task Info
      mutate(process = "HawkEye")
    

    compAPITaskInfo <- anti_join(startingTasks, completionTasks,
                                 by = c("repID", "repNames", "stackName")) # Incomplete Hawk-Eye (i.e., API) task Info

    
    # Find start and end time for API reports
    # API reports start and end time can be found from follwoing line sets
    getRepOutLines <- rbf[stringr::str_detect(string = rbf, 
                                                   pattern = "GetReportOutput")]
    
    if(length(getRepOutLines) > 1){
      groTimeRef <- paste0(word(getRepOutLines, 1),
                           " ",
                           gsub(word(getRepOutLines, 2),
                                pattern = ",",
                                replacement = "."))
      groTimeRef[which(nchar(groTimeRef) != 23)] <- NA
      timeGROTask <- format(as.POSIXct(groTimeRef), 
                            ("%Y-%m-%d %H:%M:%OS3"))
      
      repGROID <- as.numeric(gsub(";", "", 
                                  sub(".*GetReportOutput : ID *(.*?) * AppName.*", 
                                      "\\1", getRepOutLines)))
      
      uidGRO <- as.numeric(gsub(";", "", 
                                sub(".*UserID *(.*?) * Source.*", 
                                    "\\1", getRepOutLines)))
      
      repGROName <-  sub(".*Report  *(.*?) * *", 
                         "\\1", getRepOutLines)
      
      repEndAPISummary <- tbl_df(data.frame(repID = repGROID,
                                            task4user = uidGRO,
                                            taskEnd = timeGROTask,
                                            repNames = repGROName,
                                            stackName = stringr::word(nameRef, 1),
                                            process = "API")) %>% 
        group_by(task4user, repID, repNames, stackName) %>% 
        filter(n() == 1) %>%
        ungroup()
      apiRepExeTime <- left_join(compAPITaskInfo,repEndAPISummary,
                                 by = c("task4user", "repID", 
                                        "repNames", "stackName"))
      allRepGenTime <- bind_rows(HErepExecTime, apiRepExeTime)
    }else allRepGenTime <- HErepExecTime
    
   stEndOutFileName <- paste0("repStartEndTimeLog_", 
                               stringr::word(files[1],1),
                               "_to_",
                               stringr::word(files[length(files)],1),
                               ".csv")
    
    if(file.exists(paste0(outputLoc, "\\", stEndOutFileName))){
      write_csv(x = allRepGenTime, path = paste0(outputLoc, "\\", stEndOutFileName), append = TRUE)
    }else write_csv(x = allRepGenTime, path = paste0(outputLoc, "\\", stEndOutFileName))

  }
}

cat("The output file ", stEndOutFileName, "can be found in ", outputLoc, "\n")



#