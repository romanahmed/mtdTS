library(xgboost)
library(caret)

wd %>%
  select(-taskEnd, -repID, -taskStart, - taskID, -comTime) %>%
  left_join(., top90UsedReps) %>% 
  replace_na(list(top90PctRep = "No")) %>%
  mutate(weekDay = wday(date, label = TRUE)) %>%
  mutate(repNames = factor(repNames),
         stackName = factor(stackName),
         task4user = factor(task4user),
         process = factor(process),
         top90PctRep = factor(top90PctRep),
         weekDay = as.factor(as.character(weekDay)),
         hour = factor(hour),
         rGT = log(as.numeric(repGenTime))) %>%
  select(stackName, process, weekDay, hour, top90PctRep, rGT) -> dmModel
# model <- formula(repGenTime ~ hour + top90PctRep + weekDay + repNames + stackName)
model <- formula(repGenTime ~ hour + top90PctRep + weekDay + stackName + process)
dmModelDF <- data.frame(model.matrix(object = model,
                                               data = modWd)[,-1],
                      rGT = log(as.numeric(wd$repGenTime)))


set.seed(123456789)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = dmModel$rGT, 
                               p = 0.8, 
                               list = FALSE)
# subset dmModel data to training
training <- dmModel[inTrain,]
# subset the rest to test
testing <- dmModel[-inTrain,]

X_train = xgb.DMatrix(as.matrix(training %>% select(-rGT)))
y_train = training$rGT
X_test = xgb.DMatrix(as.matrix(testing %>% select(-rGT)))
y_test = testing$rGT

xgb_trcontrol = trainControl(
  method = "cv",
  number = 3,  
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE
)

# I am srGTcifing the same parameters with the same values as I did for Python above. 
# The hyrGTrparameters to optimize are found in the website.
xgbGrid <- expand.grid(nrounds = c(500, 1000) , # 2000),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20), # 25),
                       colsample_bytree = seq(0.7, 1.0, length.out = 2), #length.out = 5)
                       ## The values below are default values in the sklearn-api. 
                       eta = seq(0.01, 0.1, length.out = 2),
                       gamma = 0,
                       min_child_weight = seq(0.1, 1, length.out = 2),
                       subsample = seq(0.6, 0.90, length.out = 2)
)


# If not save in the worksapce from previous attemp run it. Running might take long
# set.seed(987654321) 
# xgb_model <- train(
#   X_train, y_train,  
#   trControl = xgb_trcontrol,
#   tuneGrid = xgbGrid,
#   method = "xgbTree",
#   nthread = 7 # passed to xgboost function
# )


xgb_model$bestTune
plot(xgb_model)

predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')


y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + 
  ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited report generation time") + 
  ylab("Observed report generation time") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

vip <- varImp(xgb_model)
colnames(dmModel)[-ncol(dmModel)][as.numeric(rownames(vip$importance))]


# Library relative importance of predictors
library(relaimpo)
lmFit <- lm(rGT~., data = dmModel)
rilmFit <- calc.relimp(lmFit, 
                       rela = TRUE, 
                       type = c("lmg", "first", "last"))
rilmFit
rilmFit$lmg.rank


lmfit2 <- lm(rGT ~ (stackName + process + weekDay + hour + top90PctRep)^2,  data = dmModel)

