#### Housekeeping ####
rm(list=ls())
dev.off()
gc()
gc()
cat("\014")


#### Load libraries #### 
library(ggplot2)
library(lubridate)
library(forecast)
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
## View(wd)

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

# ggplot_missing(wd)


#### Quantify missing pattern #### 
misPatWd <- mice::md.pattern(wd)
# View(data.frame(misPatWd))

#### Remove duplicates #### 
uWd <- unique(wd)
dim(uWd)
cat("Unique data rows accounts ", round((nrow(uWd)/nrow(wd))*100, 3), "%", "\n")
## Unless mentioned all analysis will be on dedup data with missing for listwise deletion


#### Remove NA's (missing data) ####
noUwd <- na.omit(uWd)
noUwd(noUwd)
cat("Data rows with no missing accounts ", round((nrow(noUwd)/nrow(uWd))*100, 3), "%", "\n")
cat("Removing duplicates and missing data retains  ", round((nrow(noUwd)/nrow(wd))*100, 3), "% data", "\n")


#### Create new metrics ####

class(uWd)
uWd %>%
  mutate(repGenDay = lubridate::wday(repDate, label = T),
         repGenHour = lubridate::hour(repGenTime)) -> nwd
# View(head(nwd))


#### Visualization ####

# Time view of number of reports by stack
repCountDay <- uWd %>%
  group_by(repDate, stackName) %>%
  tally() 

qp1 <- ggplot(data = repCountDay, mapping = aes(x= repDate, y = n)) +
  geom_line() +
  facet_wrap(~stackName, 
             scales = "free_y") +
  theme_bw() +
  labs(title = "Number of reports generated daily") +
  xlab("Date") + 
  ylab("Number of reports")
qp1
ggsave(qp1, 
       width = 11.69, 
       height = 8.27, 
       filename = "repGenByStackOverTime.pdf")

# repCountStack <- uWd %>%
#   group_by(stackName, repName) %>%
#   tally() %>%
#   group_by(repName)

#### Cumulative Report geneartion summary ####

cumRepGen <- uWd %>%
  group_by(repName) %>% 
  tally() %>%
  arrange(desc(n)) %>%
  mutate(cumulative = cumsum(n),
         prop = (n/ sum(n))*100,
         cumProp = cumsum(prop)) %>%
  rename("Report Name" = repName,
         "Count" = n,
         "Cumulative count" = cumulative,
         "Proportion" = prop,
         "Cumulative proportion" = cumProp)

write_csv(cumRepGen, 
          "reportPopularity.csv")


#### Top x number of reports to achieve certain threshold ####

topXrep <- function(x, topPct = 95){
  x %>%
    group_by(repName) %>% 
    tally() %>%
    arrange(desc(n)) %>%
    mutate(cumulative = cumsum(n),
           prop = (n/ sum(n))*100,
           cumProp = cumsum(prop)) -> x
  retX <- x[-which(x$cumProp > topPct),]
  if(last(retX$cumProp) != topPct*0.95){
    cat("############################################", "\n")
    cat("#### Are you happy with ", last(retX$cumProp), "% ? ####", "\n")
    cat("############################################", "\n")}
  return(retX) 
}
topXrep(uWd, topPct = 90)

#### check report by clock hour ####

uWd %>%
  mutate(hour = lubridate::hour(repGenTime)) %>%
  group_by(hour) %>%
  tally() %>%
  ggplot(mapping = aes(x = hour, y = n)) +
  # geom_line(size = 2) +
  geom_bar(stat = "identity", position = "dodge") +
  # coord_polar() +
  theme_bw() +
  labs(title = "Number of reports generated by hour") +
  xlab("Hour") + 
  ylab("Number of reports")


#### check report by clock hour ####

uWd %>%
  mutate(wkDay = lubridate::wday(repDate, label = TRUE)) %>%
  group_by(wkDay) %>%
  tally() %>%
  ggplot(mapping = aes(x = wkDay, y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  # coord_polar() +
  theme_bw() +
  labs(title = "Number of reports generated by day of the week") +
  xlab("Weekday") + 
  ylab("Number of reports")


#### Check which reports are hot vs warm vs cold DB query #### 

uWd %>%
  mutate(repDateTime = lubridate:: ymd_hms(paste0(repDate, 
                                                  " ", 
                                                  repGenTime)),
         repEndDateTime = lubridate:: mdy_hms(paste0(repPeriodEndDate, 
                                                     " ", 
                                                     repPeriodEndTime)),
         repGenDelay = interval(repDateTime, repEndDateTime)/minutes(1),
         absRepGenDelay = abs(repGenDelay)) -> hwcPD

hwcPlot <- ggplot(data = hwcPD, mapping = aes(x = repName, 
                                            y = absRepGenDelay+1)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10", 
                     breaks = scales::trans_breaks("log10", 
                                                   function(x) 10^x),
                     labels = scales::trans_format("log10", 
                                                   scales::math_format(10^.x))) +
  theme_bw() + 
  theme(axis.text.x=element_text(size=10, hjust = 1, angle=90)) +
  labs(title = "Differnce (time delay?) in report generation from event time") +
  xlab("Report") + 
  ylab("Time delay on a normalized scale")
hwcPlot

#### Highlight top 99% reports in the hwc plot ####

top99PtcRepList <- topXrep(uWd, topPct = 99) %>%
  mutate(top99PctRep = "Yes") %>%
  select(one_of(c("repName", "top99PctRep")))

uWd %>%
  left_join(., top99PtcRepList) %>% 
  replace_na(list(top99PctRep = "No")) %>%
  mutate(repDateTime = lubridate:: ymd_hms(paste0(repDate, 
                                                  " ", 
                                                  repGenTime)),
         repEndDateTime = lubridate:: mdy_hms(paste0(repPeriodEndDate, 
                                                     " ", 
                                                     repPeriodEndTime)),
         repGenDelay = interval(repDateTime, repEndDateTime)/minutes(1),
         absRepGenDelay = abs(repGenDelay)) -> hwcPlotTop99Dat

hwcPlotTop99 <- hwcPlotTop99Dat %>%
  ggplot(aes(x = repName, y = absRepGenDelay+1)) +
  geom_boxplot(aes(fill = top99PctRep)) +
  scale_fill_brewer(palette = "Accent") + 
  scale_y_continuous(trans = "log10", 
                     breaks = scales::trans_breaks("log10", 
                                                   function(x) 10^x),
                     labels = scales::trans_format("log10", 
                                                   scales::math_format(10^.x))) +
  theme_bw() + 
  theme(axis.text.x=element_text(size=10, hjust = 1, angle=90)) +
  labs(title = "Differnce (time delay?) in report generation from event time") +
  xlab("Report") + 
  ylab("Time delay on a normalized scale")
hwcPlotTop99

##########################
# set.seed(1234567890)
wd %>%
  # sample_frac(size = 0.01, replace = FALSE) %>%
  mutate(rawRowNum = 1:n(),
         repDateTime = lubridate:: ymd_hms(paste0(repDate, 
                                                  " ", 
                                                  repGenTime)),
         repEndDateTime = lubridate:: mdy_hms(paste0(repPeriodEndDate, 
                                                     " ", 
                                                     repPeriodEndTime)),
         repGenDelay = repDateTime - repEndDateTime) -> sampUwd
  # na.omit() %>%
  # arrange(repGenDelay) -> sampUwd
# View(sampUwd)
##########################

### Identify which customer are standing out in stack
uWd %>%
  select(-one_of(c("repPeriodStartDate",
                   "repPeriodStartTime",
                   "repPeriodEndDate",
                   "repPeriodEndTime",
                   "tenancy"))) %>%
  filter(vehicleID != -1) %>%
  group_by(stackName, customerID, repDate) %>%
  mutate(nVehicle = n(),
         repExTimeAllVehc = sum(repExecuteTime)) %>%
  select(one_of(c("customerID", "nVehicle", "repExTimeAllVehc"))) %>%
  distinct() %>%
  select(-nVehicle) -> repExTimeXCustomer


unique(uWd$stackName)

plotStackQT <- function(dat = repExTimeXCustomer, sNm = "Kestrel"){
  gpgExTime <- ggplot(data = dat %>%
                        filter(stackName == sNm), 
                      mapping = aes(x = repDate, 
                                    y = repExTimeAllVehc, 
                                    colour = factor(customerID))) + 
    geom_line() +
    facet_wrap(~stackName) +
    scale_y_continuous(trans = "log10", 
                       breaks = scales::trans_breaks("log10", 
                                                     function(x) 10^x),
                       labels = scales::trans_format("log10", 
                                                     scales::math_format(10^.x))) +
    # annotation_logticks(side = "l") + # Not implemented yet in plotly
    theme_bw() +
    labs(title = "Time took for generating report for all vehicles (aggregated) by day") +
    xlab("Date") + 
    ylab("Time in milliseconds") +
    guides(color=guide_legend("Customer ID"))
  
  print(gpgExTime)
  return(gpgExTime)
  # library(plotly)
  # ggplotly(gpgExTime)
}
plotStackQT(sNm = "Kestrel")
# ggplotly(plt <- plotStackQT(sNm = "Kestrel"))
plotStackQT(sNm = "MDC02")
plotStackQT(sNm = "MDC04")
plotStackQT(sNm = "MDC07")
plotStackQT(sNm = "Penguin")
plotStackQT(sNm = "Qube")
plotStackQT(sNm = "Toll")


# hist(uWd %>% filter(vehicleID != -1) %>% pull(repExecuteTime), 
#      main = "Distribution of report execution time", 
#      xlab = "Time in millieseconds")


#### Recreate report execute date + time + milliesecond ####

tmp <- uWd %>%
  mutate(repTime = format(as.POSIXct(paste0(repDate,
                                            " ",
                                            repGenTime,
                                            ".",
                                            repExecuteTime)), 
                          ("%Y-%m-%d %H:%M:%OS5")))
