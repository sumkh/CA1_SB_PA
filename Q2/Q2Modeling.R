pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr, DescTools, ROCR)

#set wd to this R file's current folder.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read csv file (to remove after merging file)
loans_df = read.csv("loansformodelling.csv",stringsAsFactors = TRUE)
tofactor = c("targetloanstatus","creditpolicy","term")
loans_df[,tofactor] = lapply(loans_df[,tofactor], as.factor)

# Sampling
##########
#Create our training set using stratified sampling.
#set initial seed for reproducibility
set.seed(123)

# partition dataset into training and test dataset in ratio 70:30
inds = createDataPartition(1:nrow(loans_df), p=0.7, list=FALSE,times=1)

loans_dftrain = loans_df[inds,]
nrow(loans_dftrain)/nrow(loans_df)
dim(loans_dftrain)

loans_dftest = loans_df[-inds,]
nrow(loans_dftest)/nrow(loans_df)
dim(loans_dftest)

#some exploration
loans_dftrain %>%
  group_by(targetloanstatus) %>%
  summarise(avgint = mean(intrate), avgloanamnt = mean(loanamnt))

loans_dftrain %>%
  group_by(targetloanstatus) %>%
  count() %>%
  mutate(perc = n/nrow(loans_dftrain))

# use caret to upsample the train dataset
loans_dftrainUP = upSample(loans_dftrain, y = as.factor(loans_dftrain$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainUP)

# use caret to downsample the train dataset
loans_dftrainDN = downSample(loans_dftrain, y = as.factor(loans_dftrain$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainDN)
########

# Create evaluation Metric
########

loans_pnl = read.csv("loansfortest.csv")
loans_testpnl = loans_pnl[-inds,]

baserevenue = (loans_testpnl %>%
                summarize(total = sum(profit)))[1,1]
baseloss = (loans_testpnl %>%
              summarize(total = sum(loss)))[1,1]
baseprofit = baserevenue - baseloss

# to determine optimum threshold point
pnl = function(predict, reference, loans_testpnl) {
  #profits -> predict no-default correctly (true-negative)
  #lost profits -> predict default incorrectly (false-positive)
  #losses -> predict no-default incorrectly (false-negative)
  
  thres = seq(0,1,0.01)
  mydf = data.frame(Threshold = numeric(),
                    Profits = numeric(),
                    Missed_Profits = numeric(),
                    Losses = numeric(),
                    Combined = numeric(),
                    Precision = numeric(),
                    Recall = numeric(),
                    F1 = numeric(),
                    fpr = numeric(),
                    newapp = numeric(),
                    existing = numeric())
  for (i in thres) {
    cm = confusionMatrix(data = as.factor(as.numeric(predict>i)), reference = reference)
    prediction = cbind(loans_testpnl,predict = as.numeric(predict>i))
    profits = (prediction %>% filter(predict == 0) %>% filter(targetloanstatus == 0) %>% summarize(sum(profit)))[1,1]
    lost_prof = (prediction %>% filter(predict == 1) %>% filter(targetloanstatus == 0) %>% summarize(sum(profit)))[1,1]
    losses = (prediction %>% filter(predict == 0) %>% filter(targetloanstatus == 1) %>% summarize(sum(loss)))[1,1]
    predictedloss = (prediction %>% filter(predict == 1) %>% filter(targetloanstatus == 1) %>% summarize(sum(loss)))[1,1]
    total = profits - lost_prof - losses
    
    predictpositivecost = (prediction %>% filter(predict == 1) %>% summarize(total = n()))[1,1] * 50

    pot_defaulter = losses + 0.9*predictedloss + predictpositivecost
    precision = cm[["byClass"]][["Precision"]]
    recall = cm[["byClass"]][["Recall"]]
    f1 = cm[["byClass"]][["F1"]]
    fpr = 1- cm[["byClass"]][["Specificity"]]
    mydf[nrow(mydf) + 1,] = list(i,profits,lost_prof,losses,total, precision, recall, f1, fpr, total, pot_defaulter)
  }
  return(mydf)
}

plotlift = function(predict, reference) {
  caseload = seq(0.01,1,0.01)
  a = data.frame(prob = predict, default = reference)
  b = cbind(arrange(a, desc(predict)), random = sample(reference))
  mydf = data.frame(caseload = numeric(),
                    lift = numeric())
  for (i in caseload) {
    predictdefault = (b %>% top_n(i*nrow(b), wt = prob) %>% count(default))[2,2]
    randomdefault = (b %>% top_n(i*nrow(b), wt = prob) %>% count(random))[2,2]
    lift = as.numeric(predictdefault/randomdefault)
    mydf[nrow(mydf) + 1,] = list(i, lift)
  }
  return(mydf)
}

###################

# DEVELOP MODEL
###################

# Logistic Regression
########
loans_dfglm <- glm(formula = targetloanstatus ~ .,
                   family=binomial,  data=loans_dftrain)
summary(loans_dfglm)

# check for multi-collinearity
vif(loans_dfglm)
# vif >10  for intrate indicating presence of multi-collinearity (grade is a categorical variable and vif is not applicable).  
# from earlier box plot, we observed a relationship between intrate and grade. we remove grade from the model and rerun the model

loans_dfglm <- glm(formula = update.formula(loans_dfglm, ~ . -grade),
                   family=binomial,  data=loans_dftrain)
summary(loans_dfglm)
vif(loans_dfglm)
# vif < 3 for all variables

# Refine model/Use step function, step function tries to optimize a glm model by automatically adding/dropping relevant independent variables.
loans_dfglm2 = step(loans_dfglm, trace = F)
summary(loans_dfglm2)
vif(loans_dfglm2)
# vif < 3 for all variables

attach(loans_dfglm2)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
anova
formula # check current formula after step function.
detach(loans_dfglm2)
# using anova to check remaining variables.
anova(loans_dfglm2, test="Chisq")
#identifies emplength as insignificant variables.retrain model by removing emplength 

loans_dfglm3 <- glm(formula = update.formula(loans_dfglm2, ~ . - emplength),
                    family=binomial,  data=loans_dftrain)
summary(loans_dfglm3)
vif(loans_dfglm3)
# vif < 3 for all variables

# test model on trainset and check accuracy with confusion matrix.
pdataglm_train <- predict(loans_dfglm3, newdata = loans_dftrain, type = "response")

#confusionmatrix (syntax: predicted result, actual results)
confusionMatrix(data = as.factor(as.numeric(pdataglm_train>0.5)), reference = loans_dftrain$targetloanstatus)
# accuracy of trainset is 84.8%

# show variable importance
VarImp_glm = as.data.frame(varImp(loans_dfglm3))
VarImp_glm =  data.frame(
  names   = rownames(VarImp_glm), overall = VarImp_glm$Overall)
VarImp_glm$names <- factor(VarImp_glm$names, levels = VarImp_glm$names[order(VarImp_glm$overall)])

VarImp_glm %>% 
  ggplot(aes(x = names, y = overall))+ geom_bar(stat ='identity') + coord_flip() + labs(title = "Relative Importance of Variables", x = 'Variable', y = 'Relative Importance')

# Perform prediction on testset and look at confusion matrix.
pdataglm_test <- predict(loans_dfglm3, newdata = loans_dftest, type = "response")

# confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)
confusionMatrix(data = as.factor(as.numeric(pdataglm_test>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy of test set is 84.9% which is comparable to our training set

# Try bagging glm model.
source("glmbagging.R")
loans_dfglmbag = bagglm(loans_dfglm3, agg = 10)

# test model on trainset and check accuracy with confusion matrix.
pdataglmbag_train = predictbag(loans_dfglmbag,loans_dftest, method = "max")
confusionMatrix(data = as.factor(as.numeric(pdataglmbag_train>0.5)), reference = loans_dftest$targetloanstatus)
# Accuracy on test set is 84.9%

# Perform prediction on testset and look at confusion matrix.
pdataglmbag_test = predictbag(loans_dfglmbag,loans_dftest, method = "max")
confusionMatrix(data = as.factor(as.numeric(pdataglmbag_test>0.5)), reference = loans_dftest$targetloanstatus)
# Accuracy on test set is 84.9%

library(pROC)
# roc syntax: (actual results, predicted probabilities)
roc_glm_test = roc(as.numeric(loans_dftest$targetloanstatus),pdataglm_test)
roc_glmbag_test = roc(as.numeric(loans_dftest$targetloanstatus),pdataglmbag_test)
plot(roc_glm_test, print.auc = TRUE,print.auc.y = 0.4, col = "green")
plot(roc_glmbag_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.3, col = "red")
legend(0.1,0.4, legend = c("Test","Test-bag"),col=c("green","red"), lty=1, cex=0.8)

prroc_glm = pnl(pdataglm_test, loans_dftest$targetloanstatus, loans_testpnl)

# plot PR curve
prroc_glm %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_glm$Recall, prroc_glm$Precision, method = "spline"),3)))

# plot ROC curve
prroc_glm %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_glm$fpr, prroc_glm$Recall, method = "spline"),3)))

# plot baseline curve
prroc_glm %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_glm %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_glm %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

#lift chart
lift_glm = plotlift(pdataglm_test, loans_dftest$targetloanstatus)
lift_glm %>%
  ggplot(aes(x = caseload, y = lift)) + 
  geom_line()
########

# Decision Tree model (rpart)
########
library(rpart)
library(rattle)

# model using unbalanced dataset
loans_dfrpart <- rpart(formula = targetloanstatus ~ .,
                       data=loans_dftrain,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=20,
                                              minbucket=7,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)

# Generate a textual view of the Decision Tree model.
loans_dfrpart
rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)

# rpart does not give good information gain on the unbalanced data set. 
# Try balancing the data before modelling.
# Perform modelling on downsampled dataset
loans_dfrpart <- rpart(formula = targetloanstatus ~ .,
                       data=loans_dftrainDN,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=20,
                                              minbucket=7,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)
# Generate a textual view of the Decision Tree model.
rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)

# Grade is the only selection factor. Try rerun model without using variable, grade.
loans_dfrpart <- rpart(formula = update.formula(loans_dfrpart, ~ . -grade),
                       data=loans_dftrainDN,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=20,
                                              minbucket=7,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)

rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)
#see variable importance
loans_dfrpart[["variable.importance"]]

# test model on trainset and check accuracy with confusion matrix.
pdata_traintree = predict(loans_dfrpart, loans_dftrainDN, type = "class")
confusionMatrix(pdata_traintree, reference = loans_dftrainDN$targetloanstatus)
# accuracy of trainset is 62.6%

# Perform prediction on testset and look at confusion matrix.
pdata_tree = predict(loans_dfrpart, loans_dftest, type = "class")
confusionMatrix(pdata_tree, reference = loans_dftest$targetloanstatus)
# accuracy of trainset is 61.8% which is comparable to our training set

# get probabilities for ROC curve
pdata_tree = predict(loans_dfrpart, loans_dftest, type = "prob")
roc_tree_test = roc(as.numeric(loans_dftest$targetloanstatus),pdata_tree[,2])
plot(roc_tree_test, print.auc = TRUE)

prroc_rpart = pnl(pdata_tree[,2], loans_dftest$targetloanstatus, loans_testpnl)

#plot PR curve
prroc_rpart %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_rpart$Recall, prroc_rpart$Precision, method = "spline"),3)))

#plot ROC curve
prroc_rpart %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_rpart$fpr, prroc_rpart$Recall, method = "spline"),3)))

#plot baseline curve
prroc_rpart %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_rpart %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_rpart %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

########

# Random Forest
########
library(randomForest)
st = Sys.time() 
rf_dn <- randomForest(targetloanstatus~., loans_dftrainDN,
                      ntree = 400,
                      mtry = 2,
                      importance = TRUE,
                      cutoff=c(0.5,1-0.5),
                      na.action=na.exclude)
Sys.time()-st #11secs
plot(rf_dn)
st=Sys.time()
t <- tuneRF(loans_dftrainDN[,-7], loans_dftrainDN[,7],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.05)
# mtry 2 has optimum 
Sys.time()-st

# Test model on trainset and check accuracy with confusion matrix.
pdatarf_train_cm <- predict(rf_dn, newdata = loans_dftrainDN, type = "response")
confusionMatrix(data = pdatarf_train_cm, reference = loans_dftrainDN$targetloanstatus)
# accuracy of training set is 99.9%

# Perform prediction on testset and look at confusion matrix.
pdatarf_test_cm <- predict(rf_dn, newdata = loans_dftest, type = "response")
confusionMatrix(data = pdatarf_test_cm, reference = loans_dftest$targetloanstatus)
# accuracy of test set is 65.0%

# Get probabilities for ROC curve
pdatarf_test_roc = predict(rf_dn, newdata = loans_dftest, type = "prob")
roc_rf_test = roc(loans_dftest$targetloanstatus,pdatarf_test_roc[,2])
plot(roc_rf_test, print.auc = TRUE, print.auc.y = 0.4, col = "green")
legend(0,0.4, legend = c("Test"),col=c("green"), lty=1, cex=0.8)
# AUC = 0.698

prroc_rf = pnl(pdatarf_test_roc[,2], loans_dftest$targetloanstatus, loans_testpnl)
#plot PR curve
prroc_rf %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_rf$Recall, prroc_rf$Precision, method = "spline"),3)))

#plot ROC curve
prroc_rf %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_rf$fpr, prroc_rf$Recall, method = "spline"),3)))

#plot baseline curve
prroc_rf %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_rf %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_rf %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

#lift chart
lift_rf = plotlift(pdatarf_test_roc[,2], loans_dftest$targetloanstatus)
cbind(glm = lift_glm, rf = lift_rf) %>%
  ggplot(aes(x = glm.caseload)) + 
  geom_line(aes(y = glm.lift), color = "red") +
  geom_line(aes(y = rf.lift), color = "blue")

lift_random = plotlift(runif(nrow(loans_dftest),0.01,1),loans_dftest$targetloanstatus)
cbind(glm = lift_glm, rf = lift_rf, random = lift_random) %>%
  ggplot(aes(x = glm.caseload, y = lift)) + 
  geom_line(aes(y = glm.lift), color = "red") +
  geom_line(aes(y = rf.lift), color = "blue") +
  geom_line(aes(y = random.lift), color = "green")
########

# Boosting
########

library(xgboost)
train_x = data.matrix(loans_dftrainDN[,-7])
train_y = loans_dftrainDN[,7]
train_y = ifelse(train_y=="1","1","0")
test_x = data.matrix(loans_dftest[,-7])
test_y = loans_dftest[,7]
test_y = ifelse(test_y=="1","1","0")

xgb_train = xgb.DMatrix(data=train_x, label=train_y)
xgb_test = xgb.DMatrix(data=test_x, label=test_y)

params_tree <- list(booster = "gbtree", 
                    eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1,
                    objective = "binary:logistic")

params_linear = list(booster = "gblinear",
                     feature_selector = "cyclic", lambda = 0, alpha = 0,
                     objective = "binary:logistic")

# try xgboost cross validation
xgbcv_tree = xgb.cv(data = xgb_train, 
                    params = params_tree, nrounds = 100, nfold = 5, 
                    showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgbcv_linear = xgb.cv(data = xgb_train, 
                      params = params_linear, nrounds = 100, nfold = 5, 
                      showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

# identify iteration with lowest error for xgb tree
which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]]))
which.min((xgbcv_tree[["evaluation_log"]][["test_error_mean"]]))

# identify iteration with lowest error for xgb linear
which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]]))
which.min((xgbcv_linear[["evaluation_log"]][["test_error_mean"]]))

xgbc_tree <- xgb.train(data = xgb_train, 
                       params = params_tree, nfold = 5, nrounds = which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]])), 
                       verbose = FALSE, eval_metric = 'auc')

mat_tree = xgb.importance(model=xgbc_tree)

xgb.plot.importance(importance_matrix = mat_tree[1:20]) 

# test on trainset and check confusion matrix
x2_dn_traintree = predict(xgbc_tree, xgb_train, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_traintree>0.5)), reference = loans_dftrainDN$targetloanstatus)
# accuracy = 92.1% for training set

# Perform prediction on testset and look at confusion matrix.
x2_dn_tree = predict(xgbc_tree, xgb_test, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_tree>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy = 61.8% for test set

prroc_xgbtree = pnl(x2_dn_tree, loans_dftest$targetloanstatus, loans_testpnl)

#plot PR curve
prroc_xgbtree %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_xgbtree$Recall, prroc_xgbtree$Precision, method = "spline"),3)))

#plot ROC curve
prroc_xgbtree %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_xgbtree$fpr, prroc_xgbtree$Recall, method = "spline"),3)))

#plot baseline curve
prroc_xgbtree %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_xgbtree %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_xgbtree %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

#roc(loans_dftrain$targetloanstatus,predict(xgbc_tree, xgb_train,type="prob"),print.auc=TRUE,print.auc.y=0.4,plot=TRUE)
#plot.roc(loans_dftest$targetloanstatus,predict(xgbc_tree, xgb_test,type="prob"),print.auc=TRUE,print.auc.y=0.3,add=TRUE, col="blue")
#AUC: 0.69

xgbc_linear <- xgb.train(data = xgb_train, 
                         params = params_linear, nfold = 5, nrounds = which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]])), 
                         verbose = FALSE, eval_metric = 'auc')

# test on trainset and check confusion matrix
x2_dn_trainlinear = predict(xgbc_linear, xgb_train, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_trainlinear>0.5)), reference = loans_dftrainDN$targetloanstatus)
# accuracy = 62.8% for training set

# Perform prediction on testset and look at confusion matrix.
x2_dn_linear = predict(xgbc_linear, xgb_test, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_linear>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy = 65.6% for test set

mat_linear = xgb.importance(model=xgbc_linear)
xgb.plot.importance(importance_matrix = mat_linear[1:20])

prroc_xgblinear = pnl(x2_dn_linear, loans_dftest$targetloanstatus, loans_testpnl)

#plot PR curve
prroc_xgblinear %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_xgblinear$Recall, prroc_xgblinear$Precision, method = "spline"),3)))

#plot ROC curve
prroc_xgblinear %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_xgblinear$fpr, prroc_xgblinear$Recall, method = "spline"),3)))

#plot baseline curve
prroc_xgblinear %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_xgblinear %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_xgblinear %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

########

# PCA
########
#create one hot encoding model.
OHEmodel <- dummyVars(~ . , select(loans_df, -targetloanstatus))
loans_dftrain_dummy = predict(OHEmodel, newdata = select(loans_df, -targetloanstatus))[inds,]
loans_dftest_dummy = predict(OHEmodel, newdata = select(loans_df, -targetloanstatus))[-inds,]

nrow(loans_dftrain_dummy)

#principal component analysis
prin_comp <- prcomp(loans_dftrain_dummy, scale. = T)
names(prin_comp)

prin_comp$rotation
dim(prin_comp$x)

# plot the resultant principal components.
biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
# plot(prop_varex, xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components, we only use the first 30 as 50% of variance is explained.

PCAtraindata = data.frame(targetloanstatus = loans_dftrain$targetloanstatus, prin_comp$x[,1:30])
PCAtestdata = data.frame(targetloanstatus = loans_dftest$targetloanstatus, predict(prin_comp, newdata = loans_dftest_dummy)[,1:30])

PCAmodel_glm = glm(targetloanstatus ~.,
                   family=binomial, data = PCAtraindata)

# test on training set
pdataPCA_trainglm = predict(PCAmodel_glm, newdata = PCAtraindata, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdataPCA_trainglm>0.5)), reference = loans_dftrain$targetloanstatus)
# accuracy of 84.8% for training set

# Perform prediction on testset and look at confusion matrix.
pdataPCA_glm = predict(PCAmodel_glm, newdata = PCAtestdata, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdataPCA_glm>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy of 84.9% for test set which is comparable to accuracy for training set

prroc_PCAglm = pnl(pdatarf_test_roc[,2], loans_dftest$targetloanstatus, loans_testpnl)
#plot PR curve
prroc_PCAglm %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_PCAglm$Recall, prroc_PCAglm$Precision, method = "spline"),3)))

#plot ROC curve
prroc_PCAglm %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_PCAglm$fpr, prroc_PCAglm$Recall, method = "spline"),3)))

#plot baseline curve
prroc_PCAglm %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_PCAglm %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_PCAglm %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)
# Build a neural network model using the neuralnet package.
########

library(neuralnet)

# Build the model.

summary(loans_dftrain)

# data preparation for train dataset
tempdata1 <- model.matrix(~creditpolicy-1, subset(loans_dftrain, select = creditpolicy))
tempdata2 <- model.matrix(~term-1, subset(loans_dftrain, select = term))
tempdata3 <- model.matrix(~grade-1, subset(loans_dftrain, select = grade))
tempdata4 <- model.matrix(~delin2years-1, subset(loans_dftrain, select = delin2years))
tempdata5 <- model.matrix(~homeowner-1, subset(loans_dftrain, select = homeowner))
tempdata6 <- model.matrix(~verified-1, subset(loans_dftrain, select = verified))
tempdata7 <- model.matrix(~purpose_mod-1, subset(loans_dftrain, select = purpose_mod))

loans_dftrainNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                              tempdata7, subset(loans_dftrain, select=c(loanamnt, intrate, emplength, dti, inqlast6mths,logrevolbal, revolutil, totalacc, logannualinc, ratioacc, targetloanstatus)))

loans_dftrain$loanamnt <- scale(loans_dftrain$loanamnt)
loans_dftrain$intrate <- scale(loans_dftrain$intrate)
loans_dftrain$emplength <- scale(loans_dftrain$emplength)
loans_dftrain$dti <- scale(loans_dftrain$dti)
loans_dftrain$inqlast6mths <- scale(loans_dftrain$inqlast6mths)
loans_dftrain$revolbal <- scale(loans_dftrain$logrevolbal)
loans_dftrain$revolutil <- scale(loans_dftrain$revolutil)
loans_dftrain$totalacc<- scale(loans_dftrain$totalacc)
loans_dftrain$logannualinc<- scale(loans_dftrain$logannualinc)
loans_dftrain$ratioacc<- scale(loans_dftrain$ratioacc)

# # use caret to downsample the train dataset
loans_dftrainNNDN = downSample(loans_dftrainNN, y = as.factor(loans_dftrainNN$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainNNDN)

# define neural network parameter

# combine the attributes name for convenience.
names <- colnames(loans_dftrainNNDN)
f <- as.formula(paste("targetloanstatus ~", paste(names[!names %in% "targetloanstatus"], collapse = " + ")))

# train model

require(nnet)
require(caret)
# nnmodel  <- nnet(f, data=loans_dftrainNNDN,
#              size=2, decay=1.0e-5, maxit=50)
st = Sys.time() 
nnmodel <- train(f, loans_dftrainNNDN, method='nnet', trace = FALSE,
                 #Grid of tuning parameters to try:
                 tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1))) 
Sys.time() - st
#a 34-7-1 network with 253 weights

# show neural network result
nnmodel[["finalModel"]]
plot(nnmodel)

# save model in rds format (to save time rerunning model)
saveRDS(nnmodel, file = "neuralnetmodel.rds")

# read model
nnmodel <- readRDS("neuralnetmodel.rds") 

# test model on training set 
my_datatrain <- subset(loans_dftrainNNDN, select = -c(targetloanstatus)) 
predictNN_train <- predict(nnmodel, my_datatrain, type = "raw")

confusionMatrix(data = predictNN_train, reference = loans_dftrainNNDN$targetloanstatus)
# Accuracy = 64.2%

# use confusion matrix to evaluate model performance on test data.

# data preparation
tempdata1 <- model.matrix(~creditpolicy-1, subset(loans_dftest, select = creditpolicy))
tempdata2 <- model.matrix(~term-1, subset(loans_dftest, select = term))
tempdata3 <- model.matrix(~grade-1, subset(loans_dftest, select = grade))
tempdata4 <- model.matrix(~delin2years-1, subset(loans_dftest, select = delin2years))
tempdata5 <- model.matrix(~homeowner-1, subset(loans_dftest, select = homeowner))
tempdata6 <- model.matrix(~verified-1, subset(loans_dftest, select = verified))
tempdata7 <- model.matrix(~purpose_mod-1, subset(loans_dftest, select = purpose_mod))

loans_dftestNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                             tempdata7, subset(loans_dftest, select=c(loanamnt, intrate, emplength, dti, inqlast6mths,logrevolbal, revolutil, totalacc, logannualinc, ratioacc, targetloanstatus)))

loans_dftest$loanamnt <- scale(loans_dftest$loanamnt)
loans_dftest$intrate <- scale(loans_dftest$intrate)
loans_dftest$emplength <- scale(loans_dftest$emplength)
loans_dftest$dti <- scale(loans_dftest$dti)
loans_dftest$inqlast6mths <- scale(loans_dftest$inqlast6mths)
loans_dftest$revolbal <- scale(loans_dftest$logrevolbal)
loans_dftest$revolutil <- scale(loans_dftest$revolutil)
loans_dftest$totalacc<- scale(loans_dftest$totalacc)
loans_dftest$logannualinc<- scale(loans_dftest$logannualinc)
loans_dftest$ratioacc<- scale(loans_dftest$ratioacc)

my_data <- subset(loans_dftestNN, select = -c(targetloanstatus)) 
predictNN_test <- predict(nnmodel, my_data, type = "raw")

# predictNN_test = factor(predictNN_test, levels = c(1,0), labels = c("Default", "No Default"))
predictNN_test = factor(predictNN_test)

confusionMatrix(data = predictNN_test, reference = loans_dftestNN$targetloanstatus)
# Accuracy = 61.4% which is comparable to the accuracy for training set

# show relative importance
VarImp_nn = varImp(nnmodel)
VarImp_nn %>% 
  ggplot(aes(x = names, y = overall))+ geom_bar(stat ='identity') + coord_flip() + labs(title = "Relative Importance of Variables", x = 'Variable', y = 'Relative Importance')

# plot roc for test set
library(pROC)
predictNN_test_roc <- predict(nnmodel, my_data, type = "prob")
head(predictNN_test_roc)[,1]
length(predictNN_test_roc[,1])
ROC_NNtest = roc(loans_dftestNN$targetloanstatus,predictNN_test_roc[,1]) 
plot(ROC_NNtest,print.auc = TRUE, print.auc.y = 0.3, col = "red")

#prroc_nn = pnl(predictNN_test_roc[,2], loans_dftestNN$targetloanstatus, loans_testpnl, baseprofit)
prroc_nn = pnl(predictNN_test_roc[,2], loans_dftestNN$targetloanstatus, loans_testpnl)
#plot PR curve
prroc_nn %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_nn$Recall, prroc_nn$Precision, method = "spline"),3)))

#plot ROC curve
prroc_nn %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_nn$fpr, prroc_nn$Recall, method = "spline"),3)))

prroc_nn %>%
  select(Threshold, newapp, existing) %>%
  gather(key = variable, value = value, -Threshold) %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable)) + facet_wrap(~variable, scales = "free")

prroc_nn %>% select(Threshold, existing) %>% top_n(-1, wt = existing)
prroc_nn %>% select(Threshold, newapp) %>% top_n(1, wt = newapp)

# prroc_nn %>%
#   ggplot(aes(x = Threshold, y = baseline)) +
#   geom_line() + ylim(-500000,500000)

# Evaluation
############

#Compute Test set's money
loans_pnl = read.csv("loansfortest.csv")
loans_testpnl = loans_pnl[-inds,]

pdataglm_test = predict(loans_dfglm3, newdata = loans_dftest, type = "response")
#optimum threshold based on median profit/loss
# pdataglm_test = as.numeric(pdataglm_test>0.8)
# loans_pnlglm = cbind(loans_pnl[-inds,],pdataglm_test)
# 
# a = (loans_pnlglm %>% filter(pdataglm_test == 0) %>% summarize(total = sum(profit) - sum(loss)))[1,1]
# b = (loans_pnlglm %>% filter(pdataglm_test == 1) %>% summarize(total = sum(profit)))[1,1]
# 
# glmearn = a-b -baseprofit

prroc_glm = pnl(pdataglm_test, loans_dftest$targetloanstatus, loans_testpnl, baseprofit)
prroc_glm %>% 
  select(-Precision, -Recall, -F1, -fpr, -Combined) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level") + facet_wrap(~variable, scales = "free_y")

prroc_glm %>%
  ggplot(aes(x = Threshold, y = baseline)) +
  geom_line() + ylim(-500000,500000)

#plot PR curve
prroc_glm %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_glm$Recall, prroc_glm$Precision, method = "spline"),3)))

#plot ROC curve
prroc_glm %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_glm$fpr, prroc_glm$Recall, method = "spline"),3)))
