pacman ::p_load(dplyr,corrplot,psych,stringr,ROSE,GGally,caret,plotROC,ggpubr,dlookr,inspectdf,randomForest,tictoc,pROC,car,xgboost)
setwd("C:/Users/andy/Desktop/NUS EBAC/EBA5002 Predictive Analytics/CA/data")
loans = read.csv("loans.csv",na.strings="n/a")
write.csv(trainset_caret,"loans_df3.csv")

str(loans)
summary(loans)
#unique id
anyDuplicated(loans$id) # no duplicate IDs
#any 0 or more (*) spaces (\\s) between beginning (^) and end ($) of string

any(grepl("^\\s*$",loans))
#empty cells
any(is.na(loans)) 
colSums(is.na(loans))
loans = loans[complete.cases(loans),]
loans$emplength=as.character(loans$emplength)
loans$emplength[is.na(loans$emplength)]=0
###########################################################################Deriving Variables
#remove "term" from term column
loans$term_num=str_extract_all(loans$term, pattern="\\d\\d" )
loans$term_num = as.numeric(loans$term_num)

loans$revoldebt=loans$revolbal*loans$revolutil
loans$targetloanstatus=as.factor(loans$targetloanstatus)

loans$creditpolicy=as.factor(loans$creditpolicy)
loans$grade = factor(loans$grade,ordered=TRUE)
loans_cont = select_if(loans,is.numeric)
corr.test(loans_cont) #high correlation between loanamnt and installment
plot(loans$loanamnt, loans$intrate)
#ggpairs(loans_cont)

g1 = loans %>%
  ggplot(aes(x=targetloanstatus))+
  geom_bar(stat="count") +
  facet_wrap(~homeownership)

g2 = loans %>%
  ggplot(aes(x=targetloanstatus))+
  geom_bar(stat="count") +
  facet_wrap(~grade)
###########Binning
loans$revolbal_bin = dlookr::binning(loans$revolbal, nbins = 5, type = "quantile", 
                                     ordered = T)

loans %>% 
  group_by(revolbal_bin) %>%
  summarise(min(revolbal), max(revolbal))

table(loans$revolbal_bin, useNA = "always")
class(loans$revolbal_bin)
str(loans$revolbal_bin)

loans$annualinc_bin = dlookr::binning(loans$annualinc, nbins = 5, type = "quantile", 
                                      ordered = T)
table(loans$annualinc_bin, useNA = "always")
class(loans$annualinc_bin)
str(loans$annualinc_bin)

loans %>% 
  group_by(annualinc_bin) %>%
  summarise(min(annualinc), max(annualinc))

table(loans$verificationstatus, useNA = "always")
loans$verified = factor(ifelse(loans$verificationstatus == "Not Verified", "N","Y"))
table(loans$verified, loans$verificationstatus)

loans = loans %>%
  mutate(emp10years = as.factor(case_when(emplength > 9 ~ "Y",
                                          emplength <= 9 ~ "N")),
         delin2years = factor(case_when(delinq2yrs > 0 ~ "Y",
                                        delinq2yrs == 0 ~ "N")),
         homeowner = factor(ifelse(homeownership == "MORTGAGE"|homeownership == "OWN",
                                   "Y", "N")))

loans = loans%>%
  mutate(purpose_mod = recode_factor(purpose,"home_improvement" = "living_expenses", 
                                     "educational" = "living_expenses", "home_improvement" = "living_expenses", "house" = "living_expenses"
                                     , "major_purchase" = "luxury", "medical" = "living_expenses", "moving" = "living_expenses",
                                     "renewable_energy" = "business", "small_business" = "business", "vacation" = "luxury", "wedding" = "living_expenses")) 

#underloans = ovun.sample(targetloanstatus~. ,data=loans,method="under")$data
trainset_caret = caret::upSample(loans, y = as.factor(loans$targetloanstatus), list = FALSE)
summary(trainset_caret)
str(trainset_caret)

trainset_caret = trainset_caret %>%
  group_by(targetloanstatus) %>%
  sample_n(5000)

#loan_fit = glm(default ~. -id -loanamnt - term_num - totalacc - revolutil -revolbal, data=trainset_caret, family=binomial)
loan_fit = glm(targetloanstatus ~ creditpolicy+dti+annualinc_bin+annualinc+installment+revolbal_bin+revolutil+grade+emp10years+emplength+homeowner+
                 purpose_mod+verified+delin2years+delinq2yrs+inqlast6mths+openacc, data=trainset_caret, family=binomial)

summary(loan_fit)
pdata <- predict(loan_fit, newdata = trainset_caret, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = trainset_caret$targetloanstatus)

#remove homeowner,dti
loan_fit2 = glm(targetloanstatus ~ creditpolicy+annualinc_bin+annualinc+installment+revolbal_bin+revolutil+grade+emp10years+emplength+
                  purpose_mod+verified+delin2years+delinq2yrs+inqlast6mths+openacc, data=trainset_caret, family=binomial)

summary(loan_fit2)
pdata2 <- predict(loan_fit2, newdata = trainset_caret, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = trainset_caret$targetloanstatus)

loan_fit3 = glm(targetloanstatus ~ dti+annualinc_bin+annualinc+installment+revolbal_bin+revolutil+emp10years+emplength+homeowner+
                  purpose_mod+verified+delin2years+delinq2yrs+inqlast6mths+openacc, data=trainset_caret, family=binomial)

summary(loan_fit3)
pdata3 <- predict(loan_fit3, newdata = trainset_caret, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata3>0.5)), reference = trainset_caret$targetloanstatus)


df <- data.frame(predictor = predict(loan_fit, trainset_caret),
                 known.truth = trainset_caret$targetloanstatus,
                 model = "train")

#df <- rbind(data.frame(predictor = predict(loan_fit, Pima.tr),
#                      known.truth = loans$targetloanstatus,
#                     model = "train"),
#data.frame(predictor = predict(glm.out.train, Pima.te),
#          known.truth = Pima.te$type,
#         model = "test"))

# the aesthetic names are not the most intuitive
# `d` (disease) holds the known truth
# `m` (marker) holds the predictor values 
ggplot(df, aes(d = known.truth, m = predictor, color = model)) + 
  geom_roc(n.cuts = 0)



loans_me = read.csv("cleanedloans.csv")
set.seed(9072)

#first partition
ind <- createDataPartition(loans_me$targetloanstatus,times=1, p=0.7,list=FALSE)

train <- loans_me[ind,]
nrow(train)/nrow(loans_me)
dim(train)

test <- loans_me[-ind,]
nrow(test)/nrow(loans_me)
dim(test)

#upsample
train_up = upSample(train, y = as.factor(train$targetloanstatus), list = FALSE, yname = "loanstatus")
train_up = select(train_up, -"loanstatus")

#########Random Forest_up
tic("model generation")
rf_up <- randomForest(targetloanstatus~., data=train_up,
                      ntree = 300,
                      mtry = 4,
                      importance = TRUE,
                      cutoff=c(0.5,1-0.5),
                      na.action=na.exclude)

# Parameters Tuning
tic("model tuning")
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:5))
Accuracy = 0.85
rf_gridsearch <- train(targetloanstatus ~., data=train_up, method="rf", metric=Accuracy, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

clock1 = toc()


# Prediction & Confusion Matrix - train data
p1_up = predict(rf_up,train_up)
confusionMatrix(p1_up,train_up$targetloanstatus) 


# Prediction & Confusion Matrix - test data
confusionMatrix(predict(rf_up, test),test$targetloanstatus)

p2_up <- predict(rf_up, test,type="prob")
par(pty='s')
roc(test$targetloanstatus,p2_up[,1],plot=TRUE,print.auc=TRUE,print.auc.x=0.4,legacy.axes=TRUE,col="green")

plot(rf_up)#plateaus after 250 ntrees
rf$err.rate

##########Logistic regression
loans_me_cont = select_if(loans_me,is.numeric)
corr.test(loans_me_cont) #loanamnt and installment

##########Logistic regression_up
tic("model generation")

glm_up = glm(targetloanstatus ~. -intrate-installment-totalacc-verified-dti-delin2years, data=train_up, family=binomial)
clock3 = toc()
summary(glm_up)
car::vif(glm_up)

# Prediction & Confusion Matrix - train data
l1_up = predict(glm_up,train_up,type="response")
l1_up = as.factor(ifelse(l1_up>0.5,"Default","No Default"))
confusionMatrix(data = l1_up,reference=train_up$targetloanstatus)

# Prediction & Confusion Matrix - test data
l2_up = predict(glm_up,test,type="response")
l2_up = as.factor(ifelse(l2_up>0.5,"Default","No Default"))
confusionMatrix(l2_up,test$targetloanstatus)

plot.roc(test$targetloanstatus,predict(glm_up,test,type="response"),print.auc=TRUE,print.auc.y=0.4,add=TRUE,col="black")

#########boosting

train_x = data.matrix(train_up[,-8])
train_y = train_up[,8]
train_y = ifelse(train_y=="Default","1","0")
test_x = data.matrix(test[,-8])
test_y = test[,8]
test_y = ifelse(test_y=="Default","1","0")

xgb_train = xgb.DMatrix(data=train_x, label=train_y)
xgb_test = xgb.DMatrix(data=test_x, label=test_y)

xgbc <- xgb.train(data = xgb_train, nfold = 5, nrounds = 100, verbose = FALSE,
                 objective = 'binary:logistic', eval_metric = 'auc')

x1_up = predict(xgbc, xgb_test,type="prob")
x1_up = as.factor(ifelse(x1_up>0.5,"Default","No Default"))
confusionMatrix(x1_up,test$targetloanstatus)


plot.roc(test$targetloanstatus,predict(xgbc, xgb_test,type="prob"),print.auc=TRUE,print.auc.y=0.2,add=TRUE,col="blue")
xgb.importance(model=xgbc)

#params = list(
 # booster="gbtree",
  #eta=0.01,
  #max_depth=5,
  #gamma=3,
  #subsample=0.75,
  #colsample_bytree=1,
  #eval_metric="mae"
#)

#xgbc = xgboost(data=xgb_train, nrounds=100)
#print(xgbc)
#pred = predict(xgbc, xgb_test,type="prob")
#pred[(pred>0.5)] = 2
#pred_y = as.factor((levels(test_y))[round(pred)])
#print(pred_y)

#cm = confusionMatrix(test_y, pred_y)
#print(cm)

result = cbind(orig=as.character(test_y),
               factor=as.factor(test_y),
               pred=pred,
               rounded=round(pred),
               pred=as.character(levels(test_y))[round(pred)])

print(data.frame(result))

p2_up <- predict(rf_up, test,type="prob")
par(pty='s')
roc(test$targetloanstatus,p2_up[,1],plot=TRUE,print.auc=TRUE,print.auc.x=0.4,legacy.axes=TRUE,col="green")


#downsample
train_down = downSample(train, y = as.factor(train$targetloanstatus), list = FALSE, yname = "loanstatus")
train_down = select(train_down, -"loanstatus")

#########Random Forest_down
tic("model generation")
rf_down <- randomForest(targetloanstatus~., data=train_down,
                        ntree = 300,
                        mtry = 4,
                        importance = TRUE,
                        cutoff=c(0.5,1-0.5),
                        na.action=na.exclude)
clock1 = toc()

# Prediction & Confusion Matrix - train data
p1_down = predict(rf_down,train_down)
confusionMatrix(p1_down,train_down$targetloanstatus) 


# Prediction & Confusion Matrix - test data
confusionMatrix(predict(rf_down, test),test$targetloanstatus)

p2_down <- predict(rf_down, test,type="prob")
par(pty='s')
roc(test$targetloanstatus,p2_down[,1],plot=TRUE,print.auc=TRUE,print.auc.x=0.4,legacy.axes=TRUE)

plot(rf_down)#plateaus after 250 ntrees
rf$err.rate

which(colnames(train)=="targetloanstatus")

tic("model tuning")
t <- tuneRF(train_up[,-8], train_up[,8],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)

clock2 = toc()

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T)

importance(rf)

varUsed(rf)

partialPlot(rf, train, intrate, "Default")

MDSplot(rf, train$dti)
