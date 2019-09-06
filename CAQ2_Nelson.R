
pacman::p_load(dplyr, tidyverse, ggplot2, lubridate, reshape2, stringr, car, caret, ggpubr)

#setwd("C:/Users/nelso/Documents/Masters/EBA5002/CA Doc/data")
#setwd("~/WorkDirectory")

loans_all = read.csv("loans.csv", as.is = TRUE)
summary(loans_all)

# data exploration
# check if all lines are filled
# remove data with missing values since instances of NA is not significant
loans = loans_all[complete.cases(loans_all),]
glimpse(loans)
summary(loans)

# Inspecting the Variables
# install.packages("inspectdf")
library("inspectdf")
inspect_cat(loans) %>% show_plot()
inspect_num(loans_all) %>% show_plot()

# code dependent variable target loan status as factor and plot graph
loans$targetloanstatus <- factor(loans$targetloanstatus, levels = c("0", "1"),
                                labels = c("No Default", "Default"))

# check for the 0 and 1 using contrasts
contrasts(loans$targetloanstatus)

loans %>%
  group_by(targetloanstatus)%>%
  summarise(per = n()/nrow(loans))%>%
  ggplot(aes(x=targetloanstatus, y=per, fill = targetloanstatus)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label = round(per, 2)), vjust = 2) + labs(fill = "Loan Status", title = "Summary of Current Loan Status", x = "Loan Status", y = "Percentage")

# 15% of borrowers defaulted on loan

# Plot loan status against term

loans %>%
  group_by(term, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loans)) %>%
  ggplot(aes(x = term, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(title="Current Loan Status by Term", x = "Term", y = "Percentage", fill = "Loan Status") 

# higher chance of default for long term (60 months) loan


# plot of loan amount against grade and loan status

loans %>%
  ggplot(aes(grade, loanamnt)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  scale_y_continuous() +
  facet_wrap(~ targetloanstatus) +
  labs(title="Loan Amount by Grade", x = "Grade", y = "Loan Amount")

# Typically higher loan amount at lower grade
# Outliers are common for grades A - D
# the loan amount spread (IQR) is higher for lower grade loans

# Plot loan status against grade

loans %>%
  group_by(grade, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loans)) %>%
  ggplot(aes(x = grade, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(title="Current Loan Status by Grade", x = "Grade", y = "Percentage", fill = "Loan Status") 


#-----------------------------------------------------------------------------#
#assume that targetloanstatus defines defaults -> defaulted loans: targetloanstatus == 1

loans %>%
  group_by(targetloanstatus) %>%
  summarise(ratio = n()/nrow(loans), count = n())

#Some data visualization

loans %>%
  group_by(targetloanstatus) %>%
  count(creditpolicy) %>%
  mutate(perc = n/sum(n))

#this is to compute percentage default against ordinal variables.
p1 = loans %>%
  group_by(grade) %>%
  count(targetloanstatus) %>%
  mutate(percentage = n/sum(n)) %>%
  ggplot(aes(x = grade, y = percentage)) +
  geom_col(aes(fill = targetloanstatus)) + 
  labs(title = "percentage of defaulters") + ylab("default %")

p2 = loans %>%
  ggplot(aes(x = targetloanstatus)) +
  geom_boxplot(aes(y=intrate)) + facet_grid(~grade)

p3 = loans %>%
  ggplot(aes(x = targetloanstatus)) +
  geom_boxplot(aes(y=annualinc), outlier.shape = NA) + facet_grid(~grade) + 
  scale_y_continuous(limits = c(0,150000))

#ggdensity plot for continuous variables
p4 = loans %>%
  ggdensity(x = "revolutil",
            add = "median",
            color = "targetloanstatus", fill = "targetloanstatus",
            palette = c("blue","red"))

#geom_area also can be used.
p5 = loans %>%
  ggplot(aes(x= revolutil)) +
  geom_area(aes(fill = targetloanstatus), color = "white", 
            stat ="density") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Density distibution of revolutil")

ggarrange(p1,p2,p3, p4, nrow = 2, ncol = 2)

#-----------------------------------------------------------------------------#

# clean emplength and term lines
table(loans$term, useNA = "always")
loans$term %>%
  str_extract("\\d{1,2}") -> loans$term

table(loans$emplength, useNA = "always")
loans$emplength %>%
  str_replace("< 1", "0") %>%
  str_extract("\\d{1,2}") -> loans$emplength
loans[,c("term","emplength")] = 
  lapply(loans[,c("term","emplength")], as.integer)
loans = na.omit(loans)

# Create 2 new variables for emp10years and delinq2ears
loans = loans %>%
  mutate(emp10years = as.factor(case_when(emplength > 9 ~ "Y",
                                          emplength <= 9 ~ "N")),
         delin2years = factor(case_when(delinq2yrs > 0 ~ "Y",
                                        delinq2yrs == 0 ~ "N")),
         homeowner = factor(ifelse(homeownership == "MORTGAGE"|homeownership == "OWN",
                                   "Y", "N")))

table(loans$emp10years, loans$emplength)
table(loans$delin2years, loans$delinq2yrs)
table(loans$homeowner, loans$homeownership)


# Binning annualinc
# install.packages("dlookr")
# library(dlookr)
loans$annualinc_bin = dlookr::binning(loans$annualinc, nbins = 5, type = "quantile", 
                                      ordered = T, labels = paste0("Gp",seq(1:5)))
table(loans$annualinc_bin, useNA = "always")
class(loans$annualinc_bin)
str(loans$annualinc_bin)

loans %>% 
  group_by(annualinc_bin) %>%
  summarise(min(annualinc), max(annualinc))

# Binning the revolbal
loans$revolbal_bin = dlookr::binning(loans$revolbal, nbins = 5, type = "quantile", 
                                     ordered = T, labels = paste0("Gp",seq(1:5)))

table(loans$revolbal_bin, useNA = "always")
class(loans$revolbal_bin)
str(loans$revolbal_bin)

loans %>% 
  group_by(revolbal_bin) %>%
  summarise(min(revolbal), max(revolbal))


# Recoding verification status
table(loans$verificationstatus, useNA = "always")
loans$verified = factor(ifelse(loans$verificationstatus == "Not Verified", "N","Y"))
table(loans$verified, loans$verificationstatus)

# Recoding purpose into 5 main categories
# create new variable purpose_mod

loans = loans%>%
  mutate(purpose_mod = recode_factor(purpose,"home_improvement" = "living_expenses", 
                                     "educational" = "living_expenses", "home_improvement" = "living_expenses", "house" = "living_expenses"
                                     , "major_purchase" = "luxury", "medical" = "living_expenses", "moving" = "living_expenses",
                                     "renewable_energy" = "business", "small_business" = "business", "vacation" = "luxury", "wedding" = "living_expenses")) 

unique(loans$purpose_mod)
str(loans$purpose_mod)

# plot purpose with modified category
loans %>%
  group_by(purpose_mod, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loans)) %>%
  ggplot(aes(x = purpose_mod, y = count_level, fill = targetloanstatus)) +
  geom_bar(stat = 'identity') +
  labs(title="Purpose", x = "Purpose", y = "Number", fill = "Loan Status")


# Factorise catagorical variables
loans[,c("creditpolicy", "grade", "homeownership", "verificationstatus","targetloanstatus","purpose_mod")] = 
  lapply(loans[,c("creditpolicy", "grade", "homeownership", "verificationstatus","targetloanstatus","purpose")], as.factor)

# Selecting Features for Modelling
loans_df = select(loans, -c("id","homeownership","annualinc", "revolbal","verificationstatus","delinq2yrs", "purpose"))
glimpse(loans_df)

# Visualisation of Correlation of Numerical Variables
corrplot::corrplot(cor(loans_df[, sapply(loans_df, is.numeric)],
                       use="complete.obs"), method = "number", type='lower')


inspect_cat(loans_df) %>% show_plot()
inspect_num(loans_df) %>% show_plot()

write.csv(loans_df, "loans_df.csv", row.names = F)

# Generating Reports with dlookr packages
#loans_df %>% diagnose_report(output_format = "html", output_file = "Diagn.html", output_dir = ".")

#loans_df %>% eda_report(target = targetloanstatus, output_format = "html", output_file = "EDA.html", output_dir = ".")

# Modelling ------------------------------------------------------------------#

#Create our training set using stratified sampling.
#set initial seed for reproducibility
set.seed(123)
# collect the data indices returned in a list
inds = createDataPartition(loans_df$targetloanstatus, p=0.7, list=FALSE,times=1) 

# Updated upstream
train_set = loans_df[inds,]
nrow(train_set)/nrow(loans_df)
dim(train_set)

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
  count()

# use caret to upsample the dataset ------------------------------------------#
loan_dfUP = upSample(loans_df, y = as.factor(loans_df$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dfUP)
table(loan_dfUP$loanstatus)
table(loan_dfUP$targetloanstatus)

write.csv(loan_dfUP, "loans_dfUP.csv", row.names = F)


set.seed(123)
# collect the data indices returned in a list
indsUP = createDataPartition(loan_dfUP$loanstatus, p=0.7, list=FALSE,times=1) 

loan_dftrainUP = loan_dfUP[indsUP,]
nrow(loan_dftrainUP)/nrow(loan_dfUP)
dim(loan_dftrainUP)

loan_dftestUP = loan_dfUP[-indsUP,]
nrow(loan_dftestUP)/nrow(loan_dfUP)



# use caret to downsample the dataset ------------------------------------------#
loan_dfDN = downSample(loans_df, y = as.factor(loans_df$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dfDN)
table(loan_dfDN$loanstatus)
table(loan_dfDN$targetloanstatus)

write.csv(loan_dfDN, "loan_dfDN.csv", row.names = F)


set.seed(123)
# collect the data indices returned in a list
indsDN = createDataPartition(loan_dfDN$loanstatus, p=0.7, list=FALSE,times=1) 

loan_dftrainDN = loan_dfDN[indsDN,]
nrow(loan_dftrainDN)/nrow(loan_dfDN)
dim(loan_dftrainDN)

loan_dftestDN = loan_dfDN[-indsDN,]
nrow(loan_dftestDN)/nrow(loan_dfDN)

# Develop model

# Logistic Regression model

loan_dfglm <- glm(formula = targetloanstatus ~ .,
                  family=binomial,  data=loan_dftrain)
summary(loan_dfglm)
vif(loan_dfglm)

# vif >10  for loanamnt, intrate, installment indicating high multicollinearity

# try using step function 
loan_dfglm2 = step(loan_dfglm, trace = F)
summary(loan_dfglm2)

vif(loan_dfglm2)

# vif >10  for intrate indicating high multicollinearity

attach(loan_dfglm2)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
detach(loan_dfglm2)
# p value = 0 - correct??
# use anova instead
anova(loan_dfglm2, test="Chisq")

# verify on test set
pdataglm <- predict(loan_dfglm2, newdata = loan_dftest, type = "response")
p_class = ifelse(pdataglm > 0.5, 1,0)
matrix_table = table(loan_dftest$targetloanstatus, p_class)
matrix_table 

accuracyglm = sum(diag(matrix_table))/sum(matrix_table)
round(accuracyglm, 3)

# accuracy = 0.851

# Build a Random Forest model 

library(randomForest)
library(pROC)
loan_dfrf <- randomForest::randomForest(targetloanstatus ~ .,
                                        data=loan_dftrain, 
                                        ntree=500,
                                        mtry=4,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of the 'Random Forest' model.

loan_dfrf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(loan_dfrf$y, as.numeric(loan_dfrf$predicted))

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


# use caret to upsample the dataset ------------------------------------------#
loan_dfUP = upSample(loans_df, y = as.factor(loans_df$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dfUP)
table(loan_dfUP$loanstatus)
table(loan_dfUP$targetloanstatus)

write.csv(loan_dfUP, "loans_dfUP.csv", row.names = F)


set.seed(123)
# collect the data indices returned in a list
indsUP = createDataPartition(loan_dfUP$loanstatus, p=0.7, list=FALSE,times=1) 

loan_dftrainUP = loan_dfUP[indsUP,]
nrow(loan_dftrainUP)/nrow(loan_dfUP)
dim(loan_dftrainUP)

loan_dftestUP = loan_dfUP[-indsUP,]
nrow(loan_dftestUP)/nrow(loan_dfUP)



# use caret to downsample the dataset ------------------------------------------#
loan_dfDN = downSample(loans_df, y = as.factor(loans_df$targetloanstatus), list = FALSE, yname = "loanstatus")
glimpse(loan_dfDN)
table(loan_dfDN$loanstatus)
table(loan_dfDN$targetloanstatus)

write.csv(loan_dfDN, "loan_dfDN.csv", row.names = F)


set.seed(123)
# collect the data indices returned in a list
indsDN = createDataPartition(loan_dfDN$loanstatus, p=0.7, list=FALSE,times=1) 

loan_dftrainDN = loan_dfDN[indsDN,]
nrow(loan_dftrainDN)/nrow(loan_dfDN)
dim(loan_dftrainDN)

loan_dftestDN = loan_dfDN[-indsDN,]
nrow(loan_dftestDN)/nrow(loan_dfDN)

# ------------------------------------------------------------------#


# Modelling ------------------------------------------------------------------#

set.seed(123)

#using sample_n to downsample.
#trainset = loans %>%
#  group_by(targetloanstatus) %>%
#  sample_n(3000)


# create dummy variables
dummies_train <- dummyVars("~. -id -targetloanstatus", data = loan_dftrain, 
                           fullRank = FALSE)

train_down_dummy <-
  loan_dftrain %>%
  select(-grade) %>%
  cbind(predict(dummies_train, newdata = loan_dftrain))

#glm model
model1 = loan_dftrain %>%
  glm(default ~ revolutil + grade, data = ., family = binomial)
summary(model1)

