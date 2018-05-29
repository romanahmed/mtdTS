##################
rm(list=ls())
gc()
cat("\014")

# library(readr)
# library(tidyverse)
# library(lubridate)
repStartEndTimeLog <- read_csv("output_report_log_summary/repStartEndTimeLog_2018-04-08_to_2018-05-06.csv")
View(repStartEndTimeLog)

wd <- repStartEndTimeLog

wd %>%
  mutate(repGenTime = difftime(taskEnd, taskStart, units="mins"),
         date = date(taskStart)) %>%
  filter(repGenTime > 0) %>%
  group_by(process, stackName, repNames, date) %>%
  mutate(comTime = sum(repGenTime)) %>%
  ungroup() -> wd

# View(wd)

# plotData <- wd %>%
#   select(one_of(c("repNames",
#                   "stackName",
#                   "process",
#                   "date",
#                   "comTime"))) %>%
#   ggplot(mapping = aes(x = date, 
#                        y = as.numeric(comTime),
#                        color = repNames)) +
#   geom_line() +
#   facet_wrap(stackName~process, 
#              scales = "free_y")

# library(plotly)




plotStackRepTime <- function(dat = wd, sNm = "Kestrel"){
  plotData <- dat %>%
    filter(stackName == sNm)
  gpgExTime <- ggplot(plotData, 
                      mapping = aes(x = date, 
                                    y = as.numeric(comTime), 
                                    colour = factor(repNames))) + 
    geom_line() +
    facet_wrap(~stackName) +
    scale_y_continuous(trans = "log10", 
                       breaks = scales::trans_breaks("log10", 
                                                     function(x) 10^x),
                       labels = scales::trans_format("log10", 
                                                     scales::math_format(10^.x))) +
    # annotation_logticks(side = "l") + # Not implemented yet in plotly
    theme_bw() +
    labs(title = "Report generation time") +
    xlab("Report") + 
    ylab("Time in mimutes") +
    guides(color=guide_legend("Report name"))
  
  # print(gpgExTime)
  return(gpgExTime)
  # library(plotly)
  # ggplotly(gpgExTime)
}
plotStackRepTime(sNm = "Qube")
# ggplotly()



# Fast, medium and slow report

#### Top x number of reports to achieve certain threshold ####

topUsedReps <- function(x, topPct = 90){
  x %>%
    group_by(repNames) %>% 
    tally() %>%
    arrange(desc(n)) %>%
    mutate(cumulative = cumsum(n),
           prop = (n/ sum(n))*100,
           cumProp = cumsum(prop)) -> x2
  retX <- x2[-which(x2$cumProp > topPct),]
  if(last(retX$cumProp) != topPct*0.95){
    cat("############################################", "\n")
    cat("#### Are you happy with ", last(retX$cumProp), "% ? ####", "\n")
    cat("############################################", "\n")}
  return(retX) 
}
topUsedReps(wd, topPct = 90)


#### Highlight top 99% reports in the hwc plot ####

top90UsedReps <- topUsedReps(wd, topPct = 90) %>%
  mutate(top90PctRep = "Yes") %>%
  select(one_of(c("repNames", "top90PctRep")))

wd %>%
  left_join(., top90UsedReps) %>% 
  replace_na(list(top90PctRep = "No")) %>%
  mutate(hour = hour(wd$taskStart)) -> fmsPlotTop90Dat

fmsPlotTop90 <- fmsPlotTop90Dat %>%
  ggplot(aes(x = repNames, y = as.numeric(repGenTime))) +
  geom_boxplot(aes(fill = top90PctRep)) +
  scale_fill_brewer(palette = "Accent") + 
  scale_y_continuous(trans = "log10", 
                     breaks = scales::trans_breaks("log10", 
                                                   function(x) 10^x),
                     labels = scales::trans_format("log10", 
                                                   scales::math_format(10^.x))) +
  theme_bw() + 
  theme(#axis.text.x=element_text(size=10, hjust = 1, angle=90),
        legend.position="bottom") +
  labs(title = "Report generation time") +
  xlab("Report type") + 
  ylab("Time in mimutes") +
  coord_flip() +
  geom_hline(yintercept=c(10^(-3), 10^(-2), 10^(-1), 10^(0), 10^(1), 10^(2)), 
             linetype="dashed", color = "grey") +
  guides(color=guide_legend("Top 90"))  #+
  # facet_grid(hour~.)
  # facet_grid(top90PctRep~.)
  # facet_grid(stackName~.)
  # facet_grid(process~.)
fmsPlotTop90





### A attribution model for report generation time ###

wd %>%
  mutate(hour = hour(wd$taskStart)) %>%
  select(-taskEnd, -repID, -taskStart, - taskID, -comTime) %>%
  left_join(., top90UsedReps) %>% 
  replace_na(list(top90PctRep = "No")) %>%
  mutate(weekDay = wday(date, label = TRUE)) %>%
  mutate(repNames = factor(repNames),
         stackName = factor(stackName),
         task4user = factor(task4user),
         process = factor(process),
         repGenTime = log(as.numeric(repGenTime)),
         top90PctRep = factor(top90PctRep),
         weekDay = as.factor(as.character(weekDay)),
         hour = factor(hour)) -> modWd


qp1 <- ggplot(modWd, aes(x = process, y =repGenTime))
qp1 + geom_boxplot()
modWd %>% group_by(process) %>% tally()

library(randomForest)

# model <- formula(repGenTime ~ hour + top90PctRep + weekDay + repNames + stackName)
model <- formula(repGenTime ~ hour + top90PctRep + weekDay + stackName + process)
dmModel <- model.matrix(object = model, data = modWd)

y <- modWd %>% pull(repGenTime) %>% as.numeric()
X <- dmModel

fitRf2 <- randomForest(X, y, importance = TRUE)


# library(doMC)
# registerDoMC(cores = 5)




# library(mlbench)
# library(caret)
# library(parallel)
# library(doParallel)
# cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
# registerDoParallel(cluster)
# # fitControl <- trainControl(method = "cv",
# #                            number = 5,
# #                            allowParallel = TRUE)
# set.seed(95014)
# fit <- train(X, y, 
#              method="rf",
#              importance = TRUE)
# stopCluster(cluster)
# registerDoSEQ()
# fit
# fit$resample
# confusionMatrix.train(fit)
# 
# 
# 
# 
