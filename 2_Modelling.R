
library(caretEnsemble)
# Stacking Algorithms - Run multiple algos in one call.
# https://stackoverflow.com/questions/45155872/r-caretensemble-warning-indexes-not-defined-in-trcontrol

# The trainControl below is only applicable for model that works with target variables
# which is factor type
trainControl <- trainControl(method="cv", 
                             number=5, 
                             index = createFolds(trainData$targetloanstatus, 5),
                             sampling = "down")


# To find the parameters of a model that can be tuned
# modelLookup(model='C5.0')

algorithmList <- c('rpart', 'rf', "C5.0")

set.seed(123)
models <- caretList(targetloanstatus ~ ., data=trainData, 
                    trControl=trainControl, 
                    methodList=algorithmList,
                    tuneLength=5, 
                    metric='ROC') 

# Compare model performances using resample()
results <- resamples(models)

# Summary of the models performances
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

#----------------------------------------------------------------------------#

# use caret to downsample the train dataset ----------------------------------#

trainDataDN <- loans_df[trainRowNumbers,]
trainDataDN = downSample(trainDataDN, y = as.factor(trainDataDN$targetloanstatus), list = FALSE)

glimpse(trainDataDN)
trainDataDN = trainDataDN[,-21]
trainDataDN = trainDataDN[,c(2,1,3:20)]

table(trainDataDN$targetloanstatus)
testData1 <- loans_df[-trainRowNumbers,]

#-----------------------------------------------------------------------------#

# Recursive Feature elimination: 
# It is a greedy optimization algorithm which aims to find the best performing 
# feature subset. It repeatedly creates models and keeps aside the best or the 
# worst performing feature at each iteration. 
# It constructs the next model with the left features until all the features 
# are exhausted. It then ranks the features based on the order of their elimination.

#Feature selection using rfe in caret
set.seed(123)
options(warn=-1)

subsets <- c(2:ncol(trainDataDN))

ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 2:ncol(trainData)], y=trainData$targetloanstatus,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

#-----------------------------------------------------------------------------#
# Training the models again with selected predictors

# Selecting the model algorithms to train
algorithmList <- c('rpart', 'rf', "C5.0")

set.seed(123)

outcomeName = "targetloanstatus"
predictors = names(trainData)[!names(trainData) %in% outcomeName] # Selecting all predictors
#Taking only the top predictors 
predictors = c("intrate", "revolutil", "term", "inqlast6mths", "purpose_mod.small_business")

model1 <- caretList(trainData[,predictors],
                    trainData[,outcomeName],
                    trControl=trainControl, 
                    methodList=algorithmList,
                    tuneLength=5,
                    metric = "ROC") ## Specify which metric to optimize

result1 <- resamples(model1)
summary(result1)

# Box plots to compare models
scale1 <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(result1, scales=scale1)

#-----------------------------------------------------------------------------#

