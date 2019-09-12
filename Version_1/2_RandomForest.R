main_df = read.csv("loans_df.csv", stringsAsFactors = F)
str(main_df)

# The following variable selections have been noted.

predictors = c("creditpolicy", "term", "intrate",
               "installment", "grade", "emplength", "dti",
               "inqlast6mths", "openacc", "revolutil",
               "totalacc", "emp10years", "delin2years",
               "homeowner", "annualinc_bin", "revolbal_bin",
               "verified", "purpose_mod")
predictors_df = main_df[,predictors]
str(predictors_df)

target = c("targetloanstatus")
targetloanstatus = main_df[,target]
str(target_df)

# "loanamnt" and "installment" are highly correlated.
ignored_var = c("loanamnt")


df = cbind(targetloanstatus, predictors_df)
str(df)

# Create the training and test datasets
set.seed(123)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$targetloanstatus, p=0.7, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df[-trainRowNumbers,]

dim(trainData) ; dim(testData)

#------------------------------------------------------------------------------#

# For random forests, 
# tuning parameter is mtry which is the number of variables randomly sampled as 
# candidates at each split. For computationally reasons, we limit 
# the number of trees via caret::trainControl(ntree)), 
# the number of folds via caret::trainControl(number)),
# the resampling iterations via caret::trainControl(repeats)),
# address the imbalance target variable with sampling = "down"

library(randomForest)

ctrl = trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 1,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE,
               sampling = "down")

model_rf1 = train(targetloanstatus ~ ., data = trainData,
                  method = 'rf',
                  ntree = 10,
                  importance = TRUE,
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  trControl = ctrl)

model_rf1
plot(model_rf1$finalModel)
# Note: The error would most likely go further down if we had allowed more trees.

#Variable Importance
var_rf1 = varImp(object=model_rf1)
var_rf1
#Plotting Variable importance
plot(var_rf1, main=paste(model_rf1$modelInfo[1], "Variable Importance"))
nrow(varImp(model_rf1)$importance)

# Variables Importance in ggplot ---------------------------------------------#
varImp(model_rf1)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Default) %>%
  mutate(Features = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = Features, y = Default))+
  coord_flip()+
  theme_bw()+
  labs(title = "Features Importance")

#-----------------------------------------------------------------------------#

varImp(model_rf1)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(Default))

# Identifying the Top 10 features
predictor10 = c("term", "inqlast6mths", "intrate", "annualinc_bin","revolutil", 
               "grade", "totalacc", "homeowner", "creditpolicy", "delin2years")

f <- as.formula(paste("targetloanstatus ~", paste(predictor10, collapse = " + ")))
f

# Training the Random Forest with lesser features
ctrl = trainControl(method = "repeatedcv", 
                    number = 10, # increase from 5 to 10 folds
                    repeats = 1,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary,
                    verboseIter = FALSE,
                    allowParallel = TRUE,
                    sampling = "up")

model_rf2 = train(f, data = trainData,
                  method = 'rf',
                  ntree = 10,
                  importance = TRUE,
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  trControl = ctrl)

model_rf2
plot(model_rf2$finalModel)

#Variable Importance
var_rf2 = varImp(object=model_rf2)
var_rf2
#Plotting Variable importance
plot(var_rf2, main=paste(model_rf2$modelInfo[1], "Variable Importance"))

model_rf_pred = predict(model_rf2, newdata = testData, type = "prob")
  
caret::confusionMatrix(
  data = factor(ifelse(model_rf_pred[, "Default"] > 0.5, "Default", "No_Default")), 
  reference = testData$targetloanstatus, positive = "Default")

# Compute ROC.
pROC::roc(response = testData$targetloanstatus, 
          predictor = model_rf_pred[, "Default"], ci = T)



# Generate an ROC Curve.
library(ROCR)
au = pROC::ci.auc(response = testData$targetloanstatus, 
                  predictor = model_rf_pred[, "Default"])[2]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))

p <- ggplot()+ geom_line(data=pd, aes(x=fpr, y=tpr), colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=4,
                  label=paste("Random Forest AUC =", round(au, 2)))
print(p)







