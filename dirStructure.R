# Directory structure and related information
rm(list=ls())
gc()
cat("\014")
# library(tidyverse)
# library(stringr)
# library(lubridate)


baseDir <- "C:\\Users\\roman\\Documents\\MTData\\DB_uplift"
stackDir <- "Report Logs"

dirNames <- list.files(paste0(baseDir, "\\", stackDir))
stackName <- stringr::word(dirNames, 1)
mtStack <- str_detect(string = dirNames, pattern = "MT")

# Check existence of zip folder
dataDir <- "zip"
dirExtCheck <- file.exists(paste0(baseDir, 
                                  "\\", 
                                  stackDir, 
                                  "\\", 
                                  dirNames, 
                                  "\\", 
                                  dataDir))
if(!all(dirExtCheck)){
  stop("The zip directory doesn't exists for all stacks")
} else{"The zip directory exists for all stacks"}

fileCount <- unlist(lapply(paste0(baseDir,
                                  "\\", 
                                  stackDir, 
                                  "\\", 
                                  dirNames, 
                                  "\\",
                                  dataDir), 
                           function(x)length(dir(x))))
if(!all(fileCount>0)){
  stop("The zip directory is not populated for all stacks")
} else{"The zip directory contains files in all stacks"}


fileNames <- lapply(paste0(baseDir,
                           "\\",
                           stackDir,
                           "\\",
                           dirNames,
                           "\\",
                           dataDir),
                    function(x)dir(x))

usefulFiles <- "Console.log.0.zip"
ufIDx <- lapply(fileNames, function(x){stringr::str_detect(x, pattern = usefulFiles)})
rbLogs <- mapply("[", fileNames, ufIDx, SIMPLIFY = F)
names(rbLogs) <- dirNames
rbLogsCount <- unlist(lapply(rbLogs, length))

dataDaySpan <- lapply(rbLogs, function(x)lubridate::date(stringr::word(rbLogs[[1]], 1)))
dataStarts <- as.Date(unlist(lapply(dataDaySpan, min)), origin = "1970-01-01") # lubridate origin
dataEnds <- as.Date(unlist(lapply(dataDaySpan, max)), origin = "1970-01-01")

stackInfo <- data.frame(directory = dirNames,
                        stack = stackName,
                        MT = mtStack,
                        dirFileCount = fileCount,
                        rbLogFileCount = rbLogsCount,
                        oldestFileDate = dataStarts,
                        newestFileDate = dataEnds)
stackInfo

files2Read <- rbLogs


dirStr <- list(stackInfo = stackInfo,
               files2Read = files2Read)







