pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr,xgboost)

#setwd("C:/Users/nelso/Documents/Github/CA1_SB_PA/Q2")
setwd("C:/Users/andy/Desktop/NUS EBAC/EBA5002 Predictive Analytics/CA")
#setwd("~/WorkDirectory")

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
##########

# Develop model
##########
# Logistic Regression model

loans_dfglm <- glm(formula = targetloanstatus ~ .,
                   family=binomial,  data=loans_dftrainDN)
summary(loans_dfglm)
vif(loans_dfglm)
# vif >10  for intrate and grade. Remove grade from domain knowledge.

loans_dfglm <- glm(formula = targetloanstatus ~ . -grade,
                   family=binomial,  data=loans_dftrainDN)
summary(loans_dfglm)
vif(loans_dfglm)

# try using step function, step function tries to optimize a lm/glm model by automatically add/dropping relevant indep variables.
loans_dfglm2 = step(loans_dfglm, trace = F)
summary(loans_dfglm2)
vif(loans_dfglm2)

attach(loans_dfglm2)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
anova
detach(loans_dfglm2)
# using anova to check remaining variables.
anova(loans_dfglm2, test="Chisq")
#identifies revolbal and emplength as insignificant variables.

loans_dfglm3 <- glm(formula = targetloanstatus ~ creditpolicy + loanamnt + term + intrate + 
                      inqlast6mths + revolutil + logannualinc + purpose_mod,
                    family=binomial,  data=loans_dftrainDN)
summary(loans_dfglm3)
vif(loans_dfglm3)

# Perform prediction on trainset and look at confusion matrix.
pdataglm_train <- predict(loans_dfglm3, newdata = loans_dftrainDN, type = "response")
pdataglm_test <- predict(loans_dfglm3, newdata = loans_dftest, type = "response")
#confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)

confusionMatrix(data = as.factor(as.numeric(pdataglm_train>0.5)), reference = loans_dftrainDN$targetloanstatus)
confusionMatrix(data = as.factor(as.numeric(pdataglm_test>0.5)), reference = loans_dftest$targetloanstatus)

#roc syntax: (actual results, predicted probabilities)
roc_glm_train = roc(as.numeric(loans_dftrainDN$targetloanstatus),pdataglm_train)
roc_glm_test = roc(as.numeric(loans_dftest$targetloanstatus),pdataglm_test)
plot(roc_glm_train, print.auc = TRUE)
plot(roc_glm_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.4, col = "green")
legend(0.1,0.4, legend = c("Train","Test"),col=c("black", "green"), lty=1, cex=0.8)
# AUC = 0.700

#Random Forest

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

# Perform prediction on trainset and look at confusion matrix.
pdatarf_train_cm <- predict(rf_dn, newdata = loans_dftrainDN, type = "response")
pdatarf_test_cm <- predict(rf_dn, newdata = loans_dftest, type = "response")

#confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)

confusionMatrix(data = pdatarf_train_cm, reference = loans_dftrainDN$targetloanstatus)
confusionMatrix(data = pdatarf_test_cm, reference = loans_dftest$targetloanstatus)


pdatarf_train_roc <- predict(rf_dn, newdata = loans_dftrainDN, type = "prob")
pdatarf_test_roc <- predict(rf_dn, newdata = loans_dftest, type = "prob")

#roc syntax: (actual results, predicted probabilities)
roc_rf_train = roc(loans_dftrainDN$targetloanstatus,pdatarf_train_roc[,1])
roc_rf_test = roc(loans_dftest$targetloanstatus,pdatarf_test_roc[,1])
plot(roc_rf_train, print.auc = TRUE)
plot(roc_rf_test, print.auc = TRUE, add = TRUE, print.auc.y = 0.4, col = "green")
legend(0,0.4, legend = c("Train","Test"),col=c("black", "green"), lty=1, cex=0.8)
# AUC = 0.696

# boosting
train_x = data.matrix(loans_dftrainDN[,-7])
train_y = loans_dftrainDN[,7]
train_y = ifelse(train_y=="1","1","0")
test_x = data.matrix(loans_dftest[,-7])
test_y = loans_dftest[,7]
test_y = ifelse(test_y=="1","1","0")

xgb_train = xgb.DMatrix(data=train_x, label=train_y)
xgb_test = xgb.DMatrix(data=test_x, label=test_y)

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)
#try xgboost cross validation
xgbcv = xgb.cv(params = params, data = xgb_train, nrounds = 100, nfold = 5, 
               showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

which.min((xgbcv[["evaluation_log"]][["test_error_mean"]]))
#8th iteration gives lowest test_error_mean

xgbc <- xgb.train(params=params,data = xgb_train, nfold = 5, nrounds = 8, verbose = FALSE,
                  eval_metric = 'auc')

x1_dn = predict(xgbc, xgb_train,type="prob")
x2_dn = predict(xgbc, xgb_test,type="prob")

x1_dn = as.factor(ifelse(x1_dn>0.5,"1","0"))
x2_dn = as.factor(ifelse(x2_dn>0.5,"1","0"))

confusionMatrix(x1_dn,loans_dftrainDN$targetloanstatus)
confusionMatrix(x2_dn,loans_dftest$targetloanstatus)

roc(loans_dftrainDN$targetloanstatus,predict(xgbc, xgb_train,type="prob"),print.auc=TRUE,print.auc.y=0.4,plot=TRUE)
plot.roc(loans_dftest$targetloanstatus,predict(xgbc, xgb_test,type="prob"),print.auc=TRUE,print.auc.y=0.3,add=TRUE,col="blue")
#AUC: 0.69
mat = xgb.importance(model=xgbc)
xgb.plot.importance (importance_matrix = mat[1:20]) 

library(pROC)

# Build a Random Forest model. This takes a while.
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model2 = train(targetloanstatus ~. , data = loan_dftrainDN, model = "rf", metric = "Accuracy", trControl = control)

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

# Generate textual output of the 'Random Forest' model.

loan_dfrf
pred2 = predict(model2, loan_dftest, type = "prob")
roc_rf = roc(as.numeric(loan_dftest$targetloanstatus),pred2$"1")
plot(roc_glm, print.auc = TRUE)
plot(roc_rf, print.auc = TRUE, add = TRUE, print.auc.y = 0.4, col = "green")

# Calculate the Area Under the Curve (AUC).

pred3 = predict(model3,loan_dftest,type = "prob")
roc_ada = roc(as.numeric(loan_dftest$targetloanstatus),pred3$"X1")
plot(roc_ada, print.auc = TRUE, add = TRUE, print.auc.y = 0.3, col = "red")
legend(0.2,0.2, legend = c("GLM","RF","ada"),col=c("black", "green","red"), lty=1, cex=0.8)
# AUC = 0.5066

# Calculate the AUC Confidence Interval.

pROC::ci.auc(loan_dfrf$y, as.numeric(loan_dfrf$predicted))

# 95% CI: 0.5046-0.5086

# List the importance of the variables.

rn <- round(randomForest::importance(loan_dfrf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# perform validation on test set

predictloan_dfrf <- predict(loan_dfrf, newdata=loan_dftest,
                            type="class")

matrix_table2 = table(loan_dftest$targetloanstatus,predictloan_dfrf)
accuracyrf = sum(diag(matrix_table2))/sum(matrix_table2)
round(accuracyrf, 3)

# accuracy = 0.85

# adaboost (WIP)
#############################################################

# Build a Decision Tree model (WIP)

library(rpart)
loan_dfrpart <- rpart(targetloanstatus ~ .,
                      data=loan_dftrain,
                      method="class",
                      parms=list(split="information"),
                      control=rpart.control(minsplit=5,
                                            minbucket=2,
                                            usesurrogate=0, 
                                            maxsurrogate=0),
                      model=TRUE)

# Generate a textual view of the Decision Tree model.

loan_dfrpart

# Build a neural network model using the neuralnet package.

library(neuralnet)

# Build the model.

summary(loan_dftrain)

# data preparation
tempdata1 <- model.matrix(~creditpolicy-1, subset(loan_dftrain, select = creditpolicy))
tempdata2 <- model.matrix(~emp10years-1, subset(loan_dftrain, select = emp10years))
tempdata3 <- model.matrix(~delin2years-1, subset(loan_dftrain, select = delin2years))
tempdata4 <- model.matrix(~homeowner-1, subset(loan_dftrain, select = homeowner))
tempdata5 <- model.matrix(~grade-1, subset(loan_dftrain, select = grade))
tempdata6 <- model.matrix(~annualinc_bin-1, subset(loan_dftrain, select = annualinc_bin))
tempdata7 <- model.matrix(~revolbal_bin-1, subset(loan_dftrain, select = revolbal_bin))
tempdata8 <- model.matrix(~verified-1, subset(loan_dftrain, select = verified))
tempdata9 <- model.matrix(~purpose_mod-1, subset(loan_dftrain, select = purpose_mod))

loan_dftrainNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                             tempdata7, tempdata8, tempdata9, subset(loan_dftrain, select=c(loanamnt, term, intrate, installment, emplength, dti, inqlast6mths,openacc, revolutil, totalacc, targetloanstatus)))

loan_dftrain$loanamnt <- scale(loan_dftrain$loanamnt)
loan_dftrain$term <- scale(loan_dftrain$term)
loan_dftrain$intrate <- scale(loan_dftrain$intrate)
loan_dftrain$installment <- scale(loan_dftrain$installment)
loan_dftrain$emplength <- scale(loan_dftrain$emplength)
loan_dftrain$dti <- scale(loan_dftrain$dti)
loan_dftrain$inqlast6mths <- scale(loan_dftrain$inqlast6mths)
loan_dftrain$openacc <- scale(loan_dftrain$openacc)
loan_dftrain$revolutil <- scale(loan_dftrain$revolutil)
loan_dftrain$totalacc<- scale(loan_dftrain$totalacc)

param_nodes_hidden_layer <- c(5,3,3) # No. of nodes at each hidden layer
param_max_iteration <- 5e4 # No. of iterations in training
param_learning_rate <- 0.1 # the learning rate during back propagation

# combine the attributes name for the convenience.
names <- colnames(loan_dftrainNN)
f <- as.formula(paste("targetloanstatus ~", paste(names[!names %in% "targetloanstatus"], collapse = " + ")))

# train model
# this takes a long duration
nnmodel <- neuralnet(f, data = loan_dftrainNN, hidden=param_nodes_hidden_layer, stepmax=param_max_iteration, learningrate = param_learning_rate, linear.output=FALSE)  

# use confusion matrix to evaluate model performance on test data.

mypredict <- compute(nnmodel, loan_dftest[,-8])$net.result
mypredict <- sapply(mypredict, round, digits=0)
results = data.frame(actual = loan_dftest$targetloanstatus, prediction = mypredict)

matrix_table3 = table(results)

accuracyNN = sum(diag(matrix_table3))/sum(matrix_table3)
round(accuracyNN, 3)