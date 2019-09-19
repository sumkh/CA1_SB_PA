pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr, DescTools, ROCR,xgboost,rpart,rattle,nnet,randomForest)

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

# use caret to upsample the train dataset
loans_dftrainUP = upSample(loans_dftrain, y = as.factor(loans_dftrain$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainUP)

# use caret to downsample the train dataset
loans_dftrainDN = downSample(loans_dftrain, y = as.factor(loans_dftrain$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainDN)

############

# establish business metric for evaluation
loans_pnl = read.csv("loansfortest.csv")
loans_pnltest = loans_pnl[-inds,]

########

# DEVELOP MODEL
###################

# Logistic Regression
########
loans_dfglm <- glm(formula = targetloanstatus ~ .,
                   family=binomial,  data=loans_dftrain)
summary(loans_dfglm)

# check for multi-collinearity

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
pdataglmbag_train = predictbag(loans_dfglmbag,loans_dftrain, method = "mean")
confusionMatrix(data = as.factor(as.numeric(pdataglmbag_train>0.5)), reference = loans_dftrain$targetloanstatus)
# Accuracy on test set is 84.9%

# Perform prediction on testset and look at confusion matrix.
pdataglmbag_test = predictbag(loans_dfglmbag,loans_dftest, method = "mean")
confusionMatrix(data = as.factor(as.numeric(pdataglmbag_test>0.5)), reference = loans_dftest$targetloanstatus)
# Accuracy on test set is 84.9%

########

# Decision Tree model (rpart)
########

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

# error is due to rpart not giving good information gain to split the trees on the unbalanced data set. 
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

#see variable importance
loans_dfrpart[["variable.importance"]]

# test model on trainset and check accuracy with confusion matrix.
pdata_traintree = predict(loans_dfrpart, loans_dftrainDN, type = "class")
confusionMatrix(pdata_traintree, reference = loans_dftrainDN$targetloanstatus)
# accuracy of trainset is 63.8%

# Perform prediction on testset and look at confusion matrix.
pdata_tree = predict(loans_dfrpart, loans_dftest, type = "class")
confusionMatrix(pdata_tree, reference = loans_dftest$targetloanstatus)
# accuracy of trainset is 61.82% which is comparable to our training set

# get probabilities for ROC curve
pdata_tree = predict(loans_dfrpart, loans_dftest, type = "prob")

########
# Random Forest
########
#build a forest with 1000 trees, mtry is recommended as sqrt of variables. Hence mtry=4 
st = Sys.time() 
set.seed(123)
rf_dn <- randomForest(targetloanstatus~., loans_dftrainDN,
                      ntree = 1000,
                      mtry = 4,
                      importance = TRUE,
                      cutoff=c(0.5,1-0.5),
                      na.action=na.exclude)
Sys.time()-st #35secs
plot(rf_dn)
#error stabilies at ntree = 400
st=Sys.time()
t <- tuneRF(loans_dftrainDN[,-6], loans_dftrainDN[,6],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.05)
Sys.time()-st #18 secs
# optimum mtry=2

st = Sys.time() 
set.seed(123)
rf_dn <- randomForest(targetloanstatus~., loans_dftrainDN,
                      ntree = 400,
                      mtry = 2,
                      importance = TRUE,
                      cutoff=c(0.5,1-0.5),
                      na.action=na.exclude)
Sys.time()-st #10secs

# Test model on trainset and check accuracy with confusion matrix.
pdatarf_train_cm <- predict(rf_dn, newdata = loans_dftrainDN, type = "response")
confusionMatrix(data = pdatarf_train_cm, reference = loans_dftrainDN$targetloanstatus)
# accuracy of training set is 99.9%

# Perform prediction on testset and look at confusion matrix.
pdatarf_test_cm <- predict(rf_dn, newdata = loans_dftest, type = "response")
confusionMatrix(data = pdatarf_test_cm, reference = loans_dftest$targetloanstatus)
# accuracy of test set is 64.87%
pdatarf_test= predict(rf_dn, newdata = loans_dftest, type = "prob")

varimp_rf = as.data.frame(varImp(rf_dn))
colnames(varimp_rf) = c("importance","importance2")
varimp_rf %>%
  select(-importance2)
varimp_rf = data.frame(names=rownames(varimp_rf),importance=varimp_rf$importance)
varimp_rf$names <- factor(varimp_rf$names, levels = varimp_rf$names[order(varimp_rf$importance)])
varimp_rf %>%
  ggplot(aes(x = names, y = importance))+ geom_bar(stat ='identity') + coord_flip() + labs(title = "Relative Importance of Variables", x = 'Variable', y = 'Relative Importance')

########
# Tree Extreme Gradient Boosting
########

train_x = data.matrix(loans_dftrainDN[,-6])
train_y = loans_dftrainDN[,6]
train_y = ifelse(train_y=="1","1","0")
test_x = data.matrix(loans_dftest[,-6])
test_y = loans_dftest[,6]
test_y = ifelse(test_y=="1","1","0")

xgb_train = xgb.DMatrix(data=train_x, label=train_y)
xgb_test = xgb.DMatrix(data=test_x, label=test_y)

params_tree <- list(booster = "gbtree", 
                    eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1,
                    objective = "binary:logistic")

# try xgboost cross validation
xgbcv_tree = xgb.cv(data = xgb_train, 
                    params = params_tree, nrounds = 100, nfold = 5, 
                    showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

# identify iteration with lowest error for xgb tree
which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]]))
which.min((xgbcv_tree[["evaluation_log"]][["test_error_mean"]]))
set.seed(123)
xgbc_tree <- xgb.train(data = xgb_train, 
                       params = params_tree, nfold = 5, nrounds = which.min((xgbcv_tree[["evaluation_log"]][["train_error_mean"]])), 
                       verbose = FALSE, eval_metric = 'auc')

mat_tree = xgb.importance(model=xgbc_tree)

xgb.plot.importance(importance_matrix = mat_tree[1:20]) 

# test on trainset and check confusion matrix
x2_dn_traintree = predict(xgbc_tree, xgb_train, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_traintree>0.5)), reference = loans_dftrainDN$targetloanstatus)
# accuracy = 90.4% for training set

# Perform prediction on testset and look at confusion matrix.
x2_dn_tree = predict(xgbc_tree, xgb_test, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_tree>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy = 62.43% for test set

########
# Linear Extreme Gradient Boosting
########

params_linear = list(booster = "gblinear",
                     feature_selector = "cyclic", lambda = 0, alpha = 0,
                     objective = "binary:logistic")

# try xgboost cross validation
xgbcv_linear = xgb.cv(data = xgb_train, 
                      params = params_linear, nrounds = 100, nfold = 5, 
                      showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

# identify iteration with lowest error for xgb linear
which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]]))
which.min((xgbcv_linear[["evaluation_log"]][["test_error_mean"]]))

set.seed(123)
xgbc_linear <- xgb.train(data = xgb_train, 
                         params = params_linear, nfold = 5, nrounds = which.min((xgbcv_linear[["evaluation_log"]][["train_error_mean"]])), 
                         verbose = FALSE, eval_metric = 'auc')

# test on trainset and check confusion matrix
x2_dn_trainlinear = predict(xgbc_linear, xgb_train, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_trainlinear>0.5)), reference = loans_dftrainDN$targetloanstatus)
# accuracy = 62.49% for training set

# Perform prediction on testset and look at confusion matrix.
x2_dn_linear = predict(xgbc_linear, xgb_test, type="prob")
confusionMatrix(data = as.factor(as.numeric(x2_dn_linear>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy = 65.22% for test set

mat_linear = xgb.importance(model=xgbc_linear)
xgb.plot.importance(importance_matrix = mat_linear[1:20]) #think need to change lambda and alpha(line 274) which are for normalizing

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
prop_varex[1:10]

#scree plot
# plot(prop_varex, xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components, we only use the first 15 Principal Component as 80% of variance is explained.

PCAtraindata = data.frame(targetloanstatus = loans_dftrain$targetloanstatus, prin_comp$x[,1:15])
PCAtestdata = data.frame(targetloanstatus = loans_dftest$targetloanstatus, predict(prin_comp, newdata = loans_dftest_dummy)[,1:15])
set.seed(123)
PCAmodel_glm = glm(targetloanstatus ~.,
                   family=binomial, data = PCAtraindata)

# test on training set
pdataPCA_trainglm = predict(PCAmodel_glm, newdata = PCAtraindata, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdataPCA_trainglm>0.5)), reference = loans_dftrain$targetloanstatus)
# accuracy of 84.82% for training set

# Perform prediction on testset and look at confusion matrix.
pdataPCA_glm = predict(PCAmodel_glm, newdata = PCAtestdata, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdataPCA_glm>0.5)), reference = loans_dftest$targetloanstatus)
# accuracy of 84.93% for test set which is comparable to accuracy for training set


# Build a neural network model using the caret and nnet package.
########

#Build the model.
# foreval = read.csv("foreval.csv",as.is = TRUE) #if need to tune NN parameters
# foreval = foreval %>% #if need to tune NN parameters
# select(-pvalue_NN) #if need to tune NN parameters

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

# data preparation for train dataset
tempdata1 <- model.matrix(~creditpolicy-1, subset(loans_dftrain, select = creditpolicy))
tempdata2 <- model.matrix(~term-1, subset(loans_dftrain, select = term))
tempdata3 <- model.matrix(~delin2years-1, subset(loans_dftrain, select = delin2years))
tempdata4 <- model.matrix(~homeowner-1, subset(loans_dftrain, select = homeowner))
tempdata5 <- model.matrix(~verified-1, subset(loans_dftrain, select = verified))
tempdata6 <- model.matrix(~purpose_mod-1, subset(loans_dftrain, select = purpose_mod))

loans_dftrainNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                              subset(loans_dftrain, select=c(loanamnt, intrate, emplength, dti, inqlast6mths,logrevolbal, revolutil, totalacc, logannualinc, ratioacc, targetloanstatus)))

loans_dftrain$loanamnt <- normalize(loans_dftrain$loanamnt)
loans_dftrain$intrate <- normalize(loans_dftrain$intrate)
loans_dftrain$emplength <- normalize(loans_dftrain$emplength)
loans_dftrain$dti <- normalize(loans_dftrain$dti)
loans_dftrain$inqlast6mths <- normalize(loans_dftrain$inqlast6mths)
loans_dftrain$logrevolbal <- normalize(loans_dftrain$logrevolbal)
loans_dftrain$revolutil <- normalize(loans_dftrain$revolutil)
loans_dftrain$totalacc<- normalize(loans_dftrain$totalacc)
loans_dftrain$logannualinc<- normalize(loans_dftrain$logannualinc)
loans_dftrain$ratioacc<- normalize(loans_dftrain$ratioacc)

# # use caret to downsample the train dataset
loans_dftrainNNDN = downSample(loans_dftrainNN, y = as.factor(loans_dftrainNN$targetloanstatus), list = TRUE)[[1]]
glimpse(loans_dftrainNNDN)

# define neural network parameter

# combine the attributes name for convenience.
names <- colnames(loans_dftrainNNDN)
f <- as.formula(paste("targetloanstatus ~", paste(names[!names %in% "targetloanstatus"], collapse = " + ")))

# train model
set.seed(123)
st = Sys.time() 
nnmodel <- train(f, loans_dftrainNNDN, method='nnet', trace = FALSE,
                 #Grid of tuning parameters to try:
                 tuneGrid=expand.grid(.size=seq(1, 11, by = 2),.decay=c(0,0.001,0.1))) 
Sys.time() - st
#a 27-7-1 network with 204 weights

# show neural network result
nnmodel[["finalModel"]]
plot(nnmodel)

# save model in rds format (to save time rerunning model)
saveRDS(nnmodel, file = "neuralnetmodel.rds")

# read model
nnmodel <- readRDS("neuralnetmodel.rds") 

# test model on training set 
my_datatrain <- subset(loans_dftrainNNDN, select = -c(targetloanstatus)) 
predictNN_train_cm <- predict(nnmodel, my_datatrain, type = "raw")

confusionMatrix(data = predictNN_train_cm, reference = loans_dftrainNNDN$targetloanstatus)
# Accuracy = 64.47%

# use confusion matrix to evaluate model performance on test data.

# data preparation
tempdata1 <- model.matrix(~creditpolicy-1, subset(loans_dftest, select = creditpolicy))
tempdata2 <- model.matrix(~term-1, subset(loans_dftest, select = term))
tempdata3 <- model.matrix(~delin2years-1, subset(loans_dftest, select = delin2years))
tempdata4 <- model.matrix(~homeowner-1, subset(loans_dftest, select = homeowner))
tempdata5 <- model.matrix(~verified-1, subset(loans_dftest, select = verified))
tempdata6 <- model.matrix(~purpose_mod-1, subset(loans_dftest, select = purpose_mod))

loans_dftestNN <- data.frame(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6,
                              subset(loans_dftest, select=c(loanamnt, intrate, emplength, dti, inqlast6mths,logrevolbal, revolutil, totalacc, logannualinc, ratioacc, targetloanstatus)))

loans_dftest$loanamnt <- normalize(loans_dftest$loanamnt)
loans_dftest$intrate <- normalize(loans_dftest$intrate)
loans_dftest$emplength <- normalize(loans_dftest$emplength)
loans_dftest$dti <- normalize(loans_dftest$dti)
loans_dftest$inqlast6mths <- normalize(loans_dftest$inqlast6mths)
loans_dftest$logrevolbal <- normalize(loans_dftest$logrevolbal)
loans_dftest$revolutil <- normalize(loans_dftest$revolutil)
loans_dftest$totalacc<- normalize(loans_dftest$totalacc)
loans_dftest$logannualinc<- normalize(loans_dftest$logannualinc)
loans_dftest$ratioacc<- normalize(loans_dftest$ratioacc)

my_data <- subset(loans_dftestNN, select = -c(targetloanstatus)) 
predictNN_test_cm <- predict(nnmodel, my_data, type = "raw")
predictNN_test <- predict(nnmodel, my_data, type = "prob")

predictNN_test = factor(predictNN_test)
confusionMatrix(data = predictNN_test_cm, reference = loans_dftestNN$targetloanstatus)
# Accuracy = 61.95% which is comparable to the accuracy for training set

# show relative importance
VarImp_nn = varImp(nnmodel)
VarImp_nn %>% 
  ggplot(aes(x = names, y = overall))+ geom_bar(stat ='identity') + coord_flip() + labs(title = "Relative Importance of Variables", x = 'Variable', y = 'Relative Importance')

#foreval = cbind(foreval,pvalue_NN = predictNN_test[,2]) #if need to tune parameters

foreval = cbind(loans_dftest,
                select(loans_testpnl, -targetloanstatus),
                pvalue_glm = pdataglm_test,
                pvalue_bag = pdataglmbag_test,
                pvalue_tree=pdata_tree[,2], 
                pvalue_forest=pdatarf_test[,2],
                pvalue_boosttree=x2_dn_tree,
                pvalue_boostlinear=x2_dn_linear,
                pvalue_pca=pdataPCA_glm,
                pvalue_NN=predictNN_test[,2])
write.csv(foreval, "foreval.csv", row.names = F)