pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr, DescTools, dummies)

#setwd("C:/Users/nelso/Documents/Github/CA1_SB_PA/Q2")
#setwd("C:/Users/andy/Desktop/NUS EBAC/EBA5002 Predictive Analytics/CA")
setwd("~/WorkDirectory")

loans_df = read.csv("loansformodelling.csv",stringsAsFactors = TRUE)
tofactor = c("targetloanstatus","creditpolicy","term")
loans_df[,tofactor] = lapply(loans_df[,tofactor], as.factor)

# Modelling
# Sampling
##########
#Create our training set using stratified sampling.
#set initial seed for reproducibility
set.seed(123)

# collect the data indices returned in a list
inds = createDataPartition(1:nrow(loans_df), p=0.7, list=FALSE,times=1)

loans_dftrain = loans_df[inds,]
nrow(loans_dftrain)/nrow(loans_df)
dim(loans_dftrain)

loans_dftest = loans_df[-inds,]
nrow(loans_dftest)/nrow(loans_df)

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

# Evaluation Metric
########

loans_pnl = read.csv("loansfortest.csv")
loans_testpnl = loans_pnl[-inds,]

baseprofit = (loans_pnl[-inds,] %>%
                summarize(total = sum(profit) - sum(loss)))[1,1]

# to determine optimum threshold point
pnl = function(predict, reference, loans_testpnl, baseprofit) {
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
                    baseline = numeric())
  for (i in thres) {
    cm = confusionMatrix(data = as.factor(as.numeric(predict>i)), reference = reference)
    prediction = cbind(loans_testpnl,predict = as.numeric(predict>i))
    profits = (prediction %>% filter(predict == 0) %>% filter(targetloanstatus == 0) %>% summarize(sum(profit)))[1,1]
    lost_prof = (prediction %>% filter(predict == 1) %>% filter(targetloanstatus == 0) %>% summarize(sum(profit)))[1,1]
    losses = (prediction %>% filter(predict == 0) %>% filter(targetloanstatus == 1) %>% summarize(sum(loss)))[1,1]
    total = profits - lost_prof - losses
    baseline = total - baseprofit
    precision = cm[["byClass"]][["Precision"]]
    recall = cm[["byClass"]][["Recall"]]
    f1 = cm[["byClass"]][["F1"]]
    fpr = 1- cm[["byClass"]][["Specificity"]]
    mydf[nrow(mydf) + 1,] = list(i,profits,lost_prof,losses,total, precision, recall, f1, fpr, baseline)
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
vif(loans_dfglm)
# vif >10  for intrate and grade. Remove grade from domain knowledge.

loans_dfglm <- glm(formula = update.formula(loans_dfglm, ~ . -grade),
                   family=binomial,  data=loans_dftrain)
summary(loans_dfglm)
vif(loans_dfglm)

# try using step function, step function tries to optimize a lm/glm model by automatically add/dropping relevant indep variables.
loans_dfglm2 = step(loans_dfglm, trace = F)
summary(loans_dfglm2)
vif(loans_dfglm2)

attach(loans_dfglm2)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
anova
formula # check current formula after step function.
detach(loans_dfglm2)
# using anova to check remaining variables.
anova(loans_dfglm2, test="Chisq")
#identifies revolbal and emplength as insignificant variables.

loans_dfglm3 <- glm(formula = update.formula(loans_dfglm2, ~ . -revolbal - emplength),
                    family=binomial,  data=loans_dftrain)
summary(loans_dfglm3)
vif(loans_dfglm3)

#Try bagging glm model.
source("glmbagging.R")
loans_dfglmbag = bagglm(loans_dfglm3, agg = 10)

# Perform prediction on trainset and look at confusion matrix.
pdataglm_train <- predict(loans_dfglm3, newdata = loans_dftrain, type = "response")
pdataglm_test <- predict(loans_dfglm3, newdata = loans_dftest, type = "response")
pdataglmbag_test = predictbag(loans_dfglmbag,loans_dftest, method = "max")
#confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)

confusionMatrix(data = as.factor(as.numeric(pdataglm_train>0.5)), reference = loans_dftrain$targetloanstatus)
confusionMatrix(data = as.factor(as.numeric(pdataglm_test>0.5)), reference = loans_dftest$targetloanstatus)

library(pROC)
#roc syntax: (actual results, predicted probabilities)
roc_glm_train = roc(as.numeric(loans_dftrain$targetloanstatus),pdataglm_train)
roc_glm_test = roc(as.numeric(loans_dftest$targetloanstatus),pdataglm_test)
roc_glmbag_test = roc(as.numeric(loans_dftest$targetloanstatus),pdataglmbag_test)
plot(roc_glm_train, print.auc = TRUE)
plot(roc_glm_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.4, col = "green")
plot(roc_glmbag_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.3, col = "red")
legend(0.1,0.4, legend = c("Train","Test","Test-bag"),col=c("black", "green","red"), lty=1, cex=0.8)

prroc_glm = pnl(pdataglm_test, loans_dftest$targetloanstatus, fp, fn, tn)
prroc_glm %>% 
  select(-Precision, -Recall, -F1, -fpr) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level")

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

########

# Decision Tree model (rpart)
########
library(rpart)
library(rattle)

#unbalanced dataset
loans_dfrpart <- rpart(formula = targetloanstatus ~ .,
                       data=loans_dftrain,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=5,
                                              minbucket=2,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)

# Generate a textual view of the Decision Tree model.
loans_dfrpart
rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)

#rpart does not see good information gain on the unbalanced data set. For a good decision tree model, balancing of data is required.

loans_dfrpart <- rpart(formula = targetloanstatus ~ .,
                       data=loans_dftrainDN,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=5,
                                              minbucket=2,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)
# Generate a textual view of the Decision Tree model.
rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)

#grade is the only selection factor?? rerun without grade.
loans_dfrpart <- rpart(formula = update.formula(loans_dfrpart, ~ . -grade),
                       data=loans_dftrainDN,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=5,
                                              minbucket=2,
                                              usesurrogate=0, 
                                              maxsurrogate=0),
                       model=TRUE)

rattle::fancyRpartPlot(loans_dfrpart)
summary(loans_dfrpart)
#see variable importance
loans_dfrpart[["variable.importance"]]

pdata_tree = predict(loans_dfrpart, loans_dftest, type = "class")
confusionMatrix(pdata_tree, reference = loans_dftest$targetloanstatus)
#get probabilities for ROC curve

pdata_tree = predict(loans_dfrpart, loans_dftest, type = "prob")
roc_tree_test = roc(as.numeric(loans_dftest$targetloanstatus),pdata_tree[,2])
plot(roc_tree_test, print.auc = TRUE)

prroc_rpart = pnl(pdata_tree[,2], loans_dftest$targetloanstatus, fp, fn, tn)
prroc_rpart %>% 
  select(-Precision, -Recall, -F1, -fpr) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level")

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


########

# Random Forest
########
library(randomForest)
st = Sys.time() 
rf_dn <- randomForest(targetloanstatus~., loans_dftrainUP,
                      ntree = 400,
                      mtry = 2,
                      importance = TRUE,
                      cutoff=c(0.5,1-0.5),
                      na.action=na.exclude)
Sys.time()-st #11secs
plot(rf_dn)
st=Sys.time()
t <- tuneRF(loans_dftrainUP[,-7], loans_dftrainUP[,7],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.05)
# mtry 2 has optimum 
Sys.time()-st

# Perform prediction on trainset and look at confusion matrix.
pdatarf_train_cm <- predict(rf_dn, newdata = loans_dftrainUP, type = "response")
pdatarf_test_cm <- predict(rf_dn, newdata = loans_dftest, type = "response")

#confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)

confusionMatrix(data = pdatarf_train_cm, reference = loans_dftrainUP$targetloanstatus)
confusionMatrix(data = pdatarf_test_cm, reference = loans_dftest$targetloanstatus)

pdatarf_train_roc <- predict(rf_dn, newdata = loans_dftrainUP, type = "prob")
pdatarf_test_roc <- predict(rf_dn, newdata = loans_dftest, type = "prob")

#roc syntax: (actual results, predicted probabilities)
roc_rf_train = roc(loans_dftrainUP$targetloanstatus,pdatarf_train_roc[,2])
roc_rf_test = roc(loans_dftest$targetloanstatus,pdatarf_test_roc[,2])
plot(roc_rf_train, print.auc = TRUE)
plot(roc_rf_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.4, col = "green")
legend(0,0.4, legend = c("Train","Test"),col=c("black", "green"), lty=1, cex=0.8)
# AUC = 0.696

prroc_rf = pnl(pdatarf_test_roc[,2], loans_dftest$targetloanstatus, loans_testpnl, baseprofit)
prroc_rf %>% 
  select(-Precision, -Recall, -F1, -fpr, -baseline) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level")

prroc_rf %>%
  ggplot(aes(x = Threshold, y = baseline)) +
  geom_line() + ylim(-500000,500000)

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

########

# boosting
########
library(xgboost)
train_x = data.matrix(loans_dftrain[,-7])
train_y = loans_dftrain[,7]
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

#try xgboost cross validation
xgbcv_tree = xgb.cv(data = xgb_train, 
               params = params_tree, nrounds = 100, nfold = 5, 
               showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgbcv_linear = xgb.cv(data = xgb_train, 
                    params = params_linear, nrounds = 100, nfold = 5, 
                    showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]]))
which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]]))
#8th iteration gives lowest test_error_mean

xgbc_tree <- xgb.train(data = xgb_train, 
                  params = params_tree, nfold = 5, nrounds = which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]])), 
                  verbose = FALSE, eval_metric = 'auc')
xgbc_linear <- xgb.train(data = xgb_train, 
                       params = params_linear, nfold = 5, nrounds = which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]])), 
                       verbose = FALSE, eval_metric = 'auc')

x1_dn_tree = predict(xgbc_tree, xgb_train, type="prob")
x2_dn_tree = predict(xgbc_tree, xgb_test, type="prob")

roc(loans_dftrain$targetloanstatus,predict(xgbc_tree, xgb_train,type="prob"),print.auc=TRUE,print.auc.y=0.4,plot=TRUE)
plot.roc(loans_dftest$targetloanstatus,predict(xgbc_tree, xgb_test,type="prob"),print.auc=TRUE,print.auc.y=0.3,add=TRUE, col="blue")
#AUC: 0.69

mat_tree = xgb.importance(model=xgbc_tree)
xgb.plot.importance(importance_matrix = mat_tree[1:20]) 

x1_dn_linear = predict(xgbc_linear, xgb_train, type="prob")
x2_dn_linear = predict(xgbc_linear, xgb_test, type="prob")

x1_dn_linear = as.factor(ifelse(x1_dn_linear>0.5,"1","0"))
x2_dn_linear = as.factor(ifelse(x2_dn_linear>0.5,"1","0"))

confusionMatrix(x1_dn_linear,loans_dftrain$targetloanstatus)
confusionMatrix(x2_dn_linear,loans_dftest$targetloanstatus)

roc(loans_dftrain$targetloanstatus,predict(xgbc_linear, xgb_train,type="prob"),print.auc=TRUE,print.auc.y=0.4,plot=TRUE)
plot.roc(loans_dftest$targetloanstatus,predict(xgbc_linear, xgb_test,type="prob"),print.auc=TRUE,print.auc.y=0.3,add=TRUE, col="blue")

mat_linear = xgb.importance(model=xgbc_linear)
xgb.plot.importance(importance_matrix = mat_linear[1:20])

prroc_xgbtree = pnl(x2_dn_tree, loans_dftest$targetloanstatus, fp, fn, tn)
prroc_xgbtree %>% 
  select(-Precision, -Recall, -F1, -fpr) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level")

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

########

# Adaboost
########
# Build an adaboost model.
library(adabag)
library(plyr)
library(dplyr)
#grid is the tuning parameters

loan_dftrainDNada = loan_dftrainDN  %>% 
  mutate(targetloanstatus = factor(targetloanstatus, 
                                   labels = make.names(levels(targetloanstatus))))

grid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3),
                    coeflearn = c("Breiman", "Freund", "Zhu"))
seeds <- vector(mode = "list", length = nrow(loan_dftrainDN) + 1)
seeds <- lapply(seeds, function(x) 1:20)
#train control is the kfolds resampling methods.
cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)

model3 <- train(targetloanstatus ~ ., data = loan_dftrainDNada, 
                method = "AdaBoost.M1",
                tuneGrid = grid, 
                trControl = cctrl1,
                metric = "ROC", 
                preProc = c("center", "scale"))
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
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components, we only use the first 10 as 50% of variance is explained.

PCAtraindata <- data.frame(targetloanstatus = loans_dftrain$targetloanstatus, prin_comp$x[,1:30])
PCAtestdata = data.frame(targetloanstatus = loans_dftest$targetloanstatus, predict(prin_comp, newdata = loans_dftest_dummy)[,1:30])

PCAmodel_glm = glm(targetloanstatus ~.,
                   family=binomial, data = PCAtraindata)
pdataPCA_glm = predict(PCAmodel_glm, newdata = PCAtestdata, type = "response")

prroc_PCAglm = pnl(pdataPCA_glm, loans_dftest$targetloanstatus, fp, fn, tn)
prroc_PCAglm %>% 
  select(-Precision, -Recall, -F1, -fpr) %>%
  melt(id.vars = "Threshold") %>%
  ggplot(aes(x = Threshold, y = value)) +
  geom_line(aes(color = variable, size = variable)) + scale_size_manual(values = c(0.75,0.75,0.75,1.5)) + scale_color_manual(values = c("green","red4","red","gold2")) +
  labs(title = "Combined Profits of Lending Club vs Threshold Level")

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

# Build a neural network model using the neuralnet package.
########

library(neuralnet)

# Build the model.

summary(loans_df)

# data preparation
tempdata1 <- model.matrix(~creditpolicy-1, subset(loans_df, select = creditpolicy))
tempdata2 <- model.matrix(~term-1, subset(loans_df, select = term))
tempdata3 <- model.matrix(~grade-1, subset(loans_df, select = grade))
tempdata4 <- model.matrix(~delin2years-1, subset(loans_df, select = delin2years))
tempdata5 <- model.matrix(~homeowner-1, subset(loans_df, select = homeowner))
tempdata6 <- model.matrix(~verified-1, subset(loans_df, select = verified))
tempdata7 <- model.matrix(~purpose_mod-1, subset(loans_df, select = purpose_mod))

loans_dfNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                         tempdata7, subset(loans_df, select=c(loanamnt, intrate, emplength, dti, inqlast6mths,revolbal, revolutil, totalacc, logannualinc, ratioacc, targetloanstatus)))

loans_df$loanamnt <- scale(loans_df$loanamnt)
loans_df$intrate <- scale(loans_df$intrate)
loans_df$emplength <- scale(loans_df$emplength)
loans_df$dti <- scale(loans_df$dti)
loans_df$inqlast6mths <- scale(loans_df$inqlast6mths)
loans_df$revolbal <- scale(loans_df$revolbal)
loans_df$revolutil <- scale(loans_df$revolutil)
loans_df$totalacc<- scale(loans_df$totalacc)
loans_df$logannualinc<- scale(loans_df$logannualinc)
loans_df$ratioacc<- scale(loans_df$ratioacc)

# split into train and test set
# collect the data indices returned in a list
inds = createDataPartition(1:nrow(loans_df), p=0.7, list=FALSE,times=1)

loans_dftrainNN = loans_dfNN[inds,]
nrow(loans_dftrainNN)/nrow(loans_dftrainNN)
dim(loans_dftrainNN)

loans_dftestNN = loans_dfNN[-inds,]
nrow(loans_dftestNN)/nrow(loans_dftestNN)

# use caret to downsample the train dataset
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
                 tuneGrid=expand.grid(.size=seq(1, 10,  by= 2),.decay=c(0,0.001,0.1))) 
Sys.time() - st
#a 41-5-1 network with 216 weights


# show neural network result
nnmodel[["finalModel"]]
plot(nnmodel)

# save model in rds format (to save time rerunning model)
saveRDS(nnmodel, file = "neuralnetmodel.rds")

# read model
nnmodel <- readRDS("neuralnetmodel.rds") 

# use confusion matrix to evaluate model performance on test data.

my_data <- subset(loans_dftestNN, select = -c(targetloanstatus)) 
predictNN_test <- predict(nnmodel, my_data, type = "raw")

# predictNN_test = factor(predictNN_test, levels = c(1,0), labels = c("Default", "No Default"))
predictNN_test = factor(predictNN_test)

head(predictNN_test)
length(predictNN_test)

head(loans_dftestNN$targetloanstatus)
length(loans_dftestNN$targetloanstatus)

confusionMatrix(data = predictNN_test, reference = loans_dftestNN$targetloanstatus)

# plot roc for test set
library(pROC)
predictNN_test_roc <- predict(nnmodel, my_data, type = "prob")
head(predictNN_test_roc)[,1]
length(predictNN_test_roc[,1])
ROC_NNtest = roc(loans_dftestNN$targetloanstatus,predictNN_test_roc[,1]) 
plot(ROC_NNtest,print.auc = TRUE, print.auc.y = 0.3, col = "red")

prroc_nn = pnl(predictNN_test_roc[,2], loans_dftestNN$targetloanstatus, loans_testpnl, baseprofit)

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
  ggplot(aes(x = Threshold, y = baseline)) +
  geom_line() + ylim(-500000,500000)

#plot F1 curve
# threshold_test %>%
#   ggplot(aes(x = Threshold, y = F1)) +
#   geom_line()

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
