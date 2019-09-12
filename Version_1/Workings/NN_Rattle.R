#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-09-07 22:04:00 x86_64-apple-darwin15.6.0 

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
# Rattle timestamp: 2019-09-07 22:04:14 x86_64-apple-darwin15.6.0 

# Load a dataset from file.

fname         <- "file:///Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/R_Rattle/trainDataDN.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-09-07 22:04:14 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=8632 train=6042 validate=1295 test=1295

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("targetloanstatus", "creditpolicy", "loanamnt",
                   "term", "intrate", "installment", "grade",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "revolbal_bin", "verified")

crs$numeric   <- c("loanamnt", "intrate", "installment",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc")

crs$categoric <- c("targetloanstatus", "creditpolicy", "term",
                   "grade", "emp10years", "delin2years", "homeowner",
                   "annualinc_bin", "revolbal_bin", "verified")

crs$target    <- "purpose_mod"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-07 22:04:37 x86_64-apple-darwin15.6.0 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=8632 train=6042 validate=0 test=2590

set.seed(123)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- NULL

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("creditpolicy", "loanamnt", "term", "intrate",
                   "installment", "grade", "emplength", "dti",
                   "inqlast6mths", "openacc", "revolutil",
                   "totalacc", "emp10years", "delin2years",
                   "homeowner", "annualinc_bin", "revolbal_bin",
                   "verified", "purpose_mod")

crs$numeric   <- c("loanamnt", "intrate", "installment",
                   "emplength", "dti", "inqlast6mths", "openacc",
                   "revolutil", "totalacc")

crs$categoric <- c("creditpolicy", "term", "grade", "emp10years",
                   "delin2years", "homeowner", "annualinc_bin",
                   "revolbal_bin", "verified", "purpose_mod")

crs$target    <- "targetloanstatus"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-09-07 22:04:57 x86_64-apple-darwin15.6.0 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(targetloanstatus) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.29 secs

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:07 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:17 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
print(riskchart(crs$pr, 
    crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus, 
    title="Performance Chart Neural Net trainDataDN.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:27 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the nnet model on trainDataDN.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Generate a Lift Chart for the nnet model on trainDataDN.csv [train].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Also convert rate of positive predictions to percentage

per <- performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus),"lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Now plot the lift.

# Bug in ROCR 1.0-3 plot does not obey the add command.# Calling the function directly (.plot.performance) does work.

.plot.performance(per, col="#00CCCCFF", lty=2, add=TRUE)

# Add a legend to the plot.

legend("topright", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="Neural Net", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  trainDataDN.csv ",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:34 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on trainDataDN.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net trainDataDN.csv [test] targetloanstatus")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:41 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the nnet model on trainDataDN.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the nnet model on trainDataDN.csv [train].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="nnet", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  trainDataDN.csv ",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:51 x86_64-apple-darwin15.6.0 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for nnet model on trainDataDN.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the nnet model on trainDataDN.csv [train].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$targetloanstatus),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="nnet", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  trainDataDN.csv ",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2019-09-07 22:05:58 x86_64-apple-darwin15.6.0 

# Score the testing dataset. 

# Obtain probability scores for the Neural Net model on trainDataDN.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$test,], select=c("targetloanstatus"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="/Users/briansum/Documents/OneDrive/EBAC_M/GitHub/CA1_SB_PA/R_Rattle/trainDataDN_test_score_idents_NN.csv", row.names=FALSE)
