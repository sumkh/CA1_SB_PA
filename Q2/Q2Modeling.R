pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr)

setwd("C:/Users/nelso/Documents/Masters/EBA5002/CA Doc/data")
#setwd("~/WorkDirectory")

loans_df = read.csv("cleanedloans.csv",stringsAsFactors = TRUE)
loans_df$targetloanstatus = as.factor(loans_df$targetloanstatus)

loans_df = loans_df %>%
  filter(grade == "B") %>%
  select(-profit, -loss, -grade)
# Modelling ------------------------------------------------------------------#

#Create our training set using stratified sampling.
#set initial seed for reproducibility
set.seed(123)
# collect the data indices returned in a list
inds = createDataPartition(1:nrow(loans_df), p=0.7, list=FALSE,times=1) 

loan_dftrain = loans_df[inds,]
nrow(loan_dftrain)/nrow(loans_df)
dim(loan_dftrain)

loan_dftest = loans_df[-inds,]
nrow(loan_dftest)/nrow(loans_df)

#some exploration
loan_dftrain %>%
  group_by(targetloanstatus) %>%
  summarise(avgint = mean(intrate), avginsta = mean(installment))

loan_dftrain %>%
  group_by(targetloanstatus) %>%
  count() %>%
  mutate(perc = n/nrow(loan_dftrain))

# use caret to upsample the train dataset ------------------------------------#
loan_dftrainUP = upSample(loan_dftrain, y = as.factor(loan_dftrain$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dftrainUP)
table(loan_dftrainUP$loanstatus, loan_dftrainUP$targetloanstatus)
loan_dftrainUP = select(loan_dftrainUP, -"loanstatus") # remove redundent variable loanstatus

# write.csv(loan_dftrainUP, "loan_dftrainUP.csv", row.names = F)
# use caret to downsample the train dataset ----------------------------------#
loan_dftrainDN = downSample(loan_dftrain, y = as.factor(loan_dftrain$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dftrainDN)
table(loan_dftrainDN$loanstatus, loan_dftrainDN$targetloanstatus)
loan_dftrainDN = select(loan_dftrainDN, -"loanstatus") # remove redundent variable loanstatus

# write.csv(loan_dftrainDN, "loan_dftrainDN.csv", row.names = F)
#-----------------------------------------------------------------------------#

# Develop model

# Logistic Regression model

loan_dfglm <- glm(formula = targetloanstatus ~ .,
                  family=binomial,  data=loan_dftrainDN)
summary(loan_dfglm)
vif(loan_dfglm)

# vif >10  for loanamnt, intrate, installment indicating high multicollinearity

# try using step function, step function tries to optimize a lm/glm model by automatically add/dropping relevant indep variables.
loan_dfglm2 = step(loan_dfglm, trace = F)
summary(loan_dfglm2)

vif(loan_dfglm2)

# vif >10  for intrate indicating high multicollinearity

attach(loan_dfglm2)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
anova
detach(loan_dfglm2)
# p value = 0 - correct??
# use anova instead
anova(loan_dfglm2, test="Chisq")

# verify on test set
pdataglm <- predict(loan_dfglm2, newdata = loan_dftest, type = "response")
#confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)
confusionMatrix(data = as.factor(as.numeric(pdataglm>0.7)), reference = loan_dftest$targetloanstatus)

library(pROC)
#roc syntax: (actual results, predicted probabilities)
roc_glm = roc(as.numeric(loan_dftest$targetloanstatus),pdataglm)
# accuracy = 0.851

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