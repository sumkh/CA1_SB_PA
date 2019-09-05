#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-09-05 14:41:41 x86_64-apple-darwin15.6.0 

# Rattle version 5.2.0 user 'briansum'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-09-05 14:41:54 x86_64-apple-darwin15.6.0 

# Load a dataset from file.

fname         <- "file:///Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/loans_df.csv" 
loan_dfdataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-09-05 14:41:55 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=41336 train=28935 validate=6200 test=6201

set.seed(crv$seed)

loan_dfnobs <- nrow(loan_dfdataset)

loan_dftrain <- sample(loan_dfnobs, 0.7*loan_dfnobs)

loan_dfnobs %>%
  seq_len() %>%
  setdiff(loan_dftrain) %>%
  sample(0.15*loan_dfnobs) ->
loan_dfvalidate

loan_dfnobs %>%
  seq_len() %>%
  setdiff(loan_dftrain) %>%
  setdiff(loan_dfvalidate) ->
loan_dftest

# The following variable selections have been noted.

loan_dfinput     <- c("creditpolicy", "loanamnt", "term", "intrate",
                   "installment", "grade", "emplength",
                   "targetloanstatus", "dti", "inqlast6mths",
                   "openacc", "revolutil", "totalacc", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "verified", "purpose_mod")

loan_dfnumeric   <- c("creditpolicy", "loanamnt", "term", "intrate",
                   "installment", "emplength", "dti", "inqlast6mths",
                   "openacc", "revolutil", "totalacc")

loan_dfcategoric <- c("grade", "targetloanstatus", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "verified", "purpose_mod")

loan_dftarget    <- "revolbal_bin"
loan_dfrisk      <- NULL
loan_dfident     <- NULL
loan_dfignore    <- NULL
loan_dfweights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-05 14:43:19 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=41336 train=28935 validate=0 test=12401

set.seed(123)

loan_dfnobs <- nrow(loan_dfdataset)

loan_dftrain <- sample(loan_dfnobs, 0.7*loan_dfnobs)
loan_dfvalidate <- NULL

loan_dfnobs %>%
  seq_len() %>%
  setdiff(loan_dftrain) %>%
  setdiff(loan_dfvalidate) ->
loan_dftest

# The following variable selections have been noted.

loan_dfinput     <- c("creditpolicy", "loanamnt", "term", "intrate",
                   "installment", "grade", "emplength", "dti",
                   "inqlast6mths", "openacc", "revolutil",
                   "totalacc", "emp10years", "delin2years",
                   "homeowner", "annualinc_bin", "verified",
                   "purpose_mod", "revolbal_bin")

loan_dfnumeric   <- c("creditpolicy", "loanamnt", "term", "intrate",
                   "installment", "emplength", "dti", "inqlast6mths",
                   "openacc", "revolutil", "totalacc")

loan_dfcategoric <- c("grade", "emp10years", "delin2years",
                   "homeowner", "annualinc_bin", "verified",
                   "purpose_mod", "revolbal_bin")

loan_dftarget    <- "targetloanstatus"
loan_dfrisk      <- NULL
loan_dfident     <- NULL
loan_dfignore    <- NULL
loan_dfweights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-05 14:44:00 x86_64-apple-darwin15.6.0 

# Remap variables. 

# Transform into a factor.

  loan_dfdataset[["TFC_creditpolicy"]] <- as.factor(loan_dfdataset[["creditpolicy"]])

  ol <- levels(loan_dfdataset[["TFC_creditpolicy"]])
  lol <- length(ol)
  nl <- c(sprintf("[%s,%s]", ol[1], ol[1]), sprintf("(%s,%s]", ol[-lol], ol[-1]))
  levels(loan_dfdataset[["TFC_creditpolicy"]]) <- nl

#=======================================================================
# Rattle timestamp: 2019-09-05 14:44:01 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

loan_dfinput     <- c("loanamnt", "term", "intrate", "installment",
                   "grade", "emplength", "dti", "inqlast6mths",
                   "openacc", "revolutil", "totalacc", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "verified", "purpose_mod", "revolbal_bin",
                   "TFC_creditpolicy")

loan_dfnumeric   <- c("loanamnt", "term", "intrate", "installment",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc")

loan_dfcategoric <- c("grade", "emp10years", "delin2years",
                   "homeowner", "annualinc_bin", "verified",
                   "purpose_mod", "revolbal_bin", "TFC_creditpolicy")

loan_dftarget    <- "targetloanstatus"
loan_dfrisk      <- NULL
loan_dfident     <- NULL
loan_dfignore    <- "creditpolicy"
loan_dfweights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-05 14:45:38 x86_64-apple-darwin15.6.0 

# Remap variables. 

# Transform into a factor.

  loan_dfdataset[["TFC_term"]] <- as.factor(loan_dfdataset[["term"]])

  ol <- levels(loan_dfdataset[["TFC_term"]])
  lol <- length(ol)
  nl <- c(sprintf("[%s,%s]", ol[1], ol[1]), sprintf("(%s,%s]", ol[-lol], ol[-1]))
  levels(loan_dfdataset[["TFC_term"]]) <- nl

#=======================================================================
# Rattle timestamp: 2019-09-05 14:45:39 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

loan_dfinput     <- c("loanamnt", "intrate", "installment", "grade",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "verified", "purpose_mod", "revolbal_bin",
                   "TFC_creditpolicy", "TFC_term")

loan_dfnumeric   <- c("loanamnt", "intrate", "installment",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc")

loan_dfcategoric <- c("grade", "emp10years", "delin2years",
                   "homeowner", "annualinc_bin", "verified",
                   "purpose_mod", "revolbal_bin", "TFC_creditpolicy",
                   "TFC_term")

loan_dftarget    <- "targetloanstatus"
loan_dfrisk      <- NULL
loan_dfident     <- NULL
loan_dfignore    <- c("creditpolicy", "term")
loan_dfweights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-05 14:46:28 x86_64-apple-darwin15.6.0 

# Save the project data (variable crs) to file.

save(crs, file="/Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/loans_df.rattle", compress=TRUE)

#=======================================================================
# Rattle timestamp: 2019-09-05 14:46:44 x86_64-apple-darwin15.6.0 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

loan_dfrpart <- rpart(targetloanstatus ~ .,
    data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(loan_dfrpart)
printcp(loan_dfrpart)
cat("\n")

# Time taken: 0.17 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:47:48 x86_64-apple-darwin15.6.0 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

loan_dfrpart <- rpart(targetloanstatus ~ .,
    data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)],
    method="class",
    parms=list(split="information"),
      control=rpart.control(minbucket=2,
        usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(loan_dfrpart)
printcp(loan_dfrpart)
cat("\n")

# Time taken: 0.17 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:48:16 x86_64-apple-darwin15.6.0 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

loan_dfrpart <- rpart(targetloanstatus ~ .,
    data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)],
    method="class",
    parms=list(split="information"),
      control=rpart.control(minsplit=19,
           minbucket=2,
        usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(loan_dfrpart)
printcp(loan_dfrpart)
cat("\n")

# Time taken: 0.18 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:48:37 x86_64-apple-darwin15.6.0 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

loan_dfrpart <- rpart(targetloanstatus ~ .,
    data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)],
    method="class",
    parms=list(split="information"),
      control=rpart.control(minsplit=5,
           minbucket=2,
        usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(loan_dfrpart)
printcp(loan_dfrpart)
cat("\n")

# Time taken: 0.17 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:48:58 x86_64-apple-darwin15.6.0 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

loan_dfrf <- randomForest::randomForest(targetloanstatus ~ .,
  data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)], 
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

# Calculate the AUC Confidence Interval.

pROC::ci.auc(loan_dfrf$y, as.numeric(loan_dfrf$predicted))FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(loan_dfrf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 2.27 mins

#=======================================================================
# Rattle timestamp: 2019-09-05 14:54:38 x86_64-apple-darwin15.6.0 

# Regression model 

# Build a Regression model.

loan_dfglm <- glm(targetloanstatus ~ .,
    data=loan_dfdataset[loan_dftrain, c(loan_dfinput, loan_dftarget)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(loan_dfglm))

cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(loan_dfglm)[1],
            attr(logLik(loan_dfglm), "df")))

cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            loan_dfglm$null.deviance-loan_dfglm$deviance,
            loan_dfglm$df.null-loan_dfglm$df.residual))

cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(loan_dfglm$null.deviance-loan_dfglm$deviance,
                   loan_dfglm$df.null-loan_dfglm$df.residual)))

cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(loan_dfglm$y, loan_dfglm$fitted.values)))

cat('\n==== ANOVA ====\n\n')
print(anova(loan_dfglm, test="Chisq"))
cat("\n")

# Time taken: 4.82 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:54:54 x86_64-apple-darwin15.6.0 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
loan_dfnnet <- nnet(as.factor(targetloanstatus) ~ .,
    data=loan_dfdataset[loan_dftrain,c(loan_dfinput, loan_dftarget)],
    size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(loan_dfnnet$n, collapse="-"),
    length(loan_dfnnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(loan_dfnnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(loan_dfnnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(loan_dfnnet) ^ 2)))
cat("\n")
print(summary(loan_dfnnet))
cat('\n')

# Time taken: 0.60 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 14:55:15 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

loan_dfpr <- predict(loan_dfrpart, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)],
    type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

loan_dfpr <- predict(loan_dfrf, newdata=na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

loan_dfpr <- as.vector(ifelse(predict(loan_dfglm, 
   type    = "response",
   newdata = loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]) > 0.5, "No Default", "Default"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

loan_dfpr <- predict(loan_dfnnet, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-09-05 14:58:05 x86_64-apple-darwin15.6.0 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
loan_dfada <- ada::ada(targetloanstatus ~ .,
                    data=loan_dfdataset[loan_dftrain,c(loan_dfinput, loan_dftarget)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(loan_dfada)
round(loan_dfada$model$errs[loan_dfada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(loan_dfada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(loan_dfada))

# Time taken: 28.54 secs

#=======================================================================
# Rattle timestamp: 2019-09-05 15:03:05 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

loan_dfpr <- predict(loan_dfrpart, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)],
    type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

loan_dfpr <- predict(loan_dfada, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

loan_dfpr <- predict(loan_dfrf, newdata=na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

loan_dfpr <- as.vector(ifelse(predict(loan_dfglm, 
   type    = "response",
   newdata = loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]) > 0.5, "No Default", "Default"))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

loan_dfpr <- predict(loan_dfnnet, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus, loan_dfpr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-09-05 15:03:42 x86_64-apple-darwin15.6.0 

# Save the project data (variable crs) to file.

save(crs, file="/Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/loans_df.rattle", compress=TRUE)

#=======================================================================
# Rattle timestamp: 2019-09-05 15:15:53 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on loans_df.csv [test].

loan_dfpr <- predict(loan_dfrpart, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree loans_df.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on loans_df.csv [test].

loan_dfpr <- predict(loan_dfada, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost loans_df.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on loans_df.csv [test].

loan_dfpr <- predict(loan_dfrf, newdata=na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]),
    type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest loans_df.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on loans_df.csv [test].

loan_dfpr <- predict(loan_dfglm, 
   type    = "response",
   newdata = loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])

# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear loans_df.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on loans_df.csv [test].

loan_dfpr <- predict(loan_dfnnet, newdata=loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)])

# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net loans_df.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(loan_dfdataset[loan_dftest, c(loan_dfinput, loan_dftarget)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(loan_dfpr[-miss.list], no.miss)
} else
{
  pred <- prediction(loan_dfpr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-09-05 15:18:01 x86_64-apple-darwin15.6.0 

# Save the project data (variable crs) to file.

save(crs, file="/Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/loans_df.rattle", compress=TRUE)
