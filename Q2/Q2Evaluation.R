pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr, DescTools, ROCR)

#set wd to this R file's current folder.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

foreval = read.csv("foreval.csv", as.is = TRUE)
tofactor = c("targetloanstatus","creditpolicy","term")
foreval[,tofactor] = lapply(foreval[,tofactor], as.factor)
set.seed(2019)

########
# Evaluation Functions
########
pnl = function(predict, reference) {
  #profits -> predict no-default correctly (true-negative)
  #lost profits -> predict default incorrectly (false-positive)
  #losses -> predict no-default incorrectly (false-negative)
  thres = seq(0,1,0.01)
  mydf = data.frame(Threshold = numeric(),
                    Precision = numeric(),
                    Recall = numeric(),
                    F1 = numeric(),
                    fpr = numeric())
  for (i in thres) {
    cm = confusionMatrix(data = as.factor(as.numeric(predict>i)), reference = reference)
    precision = cm[["byClass"]][["Precision"]]
    recall = cm[["byClass"]][["Recall"]]
    f1 = cm[["byClass"]][["F1"]]
    fpr = 1- cm[["byClass"]][["Specificity"]]
    mydf[nrow(mydf) + 1,] = list(i,precision, recall, f1, fpr)
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

plotprofit = function(predict, foreval) {
  caseload = seq(0.01,1,0.01)
  a = data.frame(prob = predict, foreval)
  mydf = data.frame(caseload = numeric(),
                    profits = numeric())
  for (i in caseload) {
    profits = (a %>% top_n(-i*nrow(a), wt = prob) %>% summarise(total = sum(profit) - sum(loss)))[1,1]
    mydf[nrow(mydf) + 1,] = list(i, profits)
  }
  return(mydf)
}
set.seed(2019)
baseprofit = plotprofit(runif(nrow(foreval),0.01,1), foreval)

########
# evaluating ROC and PR curves curves

########
prroc_glm = pnl(foreval$pvalue_glm, foreval$targetloanstatus)

# plot PR curve
prroc_glm %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) + labs(title = "PR Curve for glm model") +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_glm$Recall, prroc_glm$Precision, method = "spline"),3)))

# plot ROC curve
prroc_glm %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) + labs(title = "ROC Curve for glm model") +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_glm$fpr, prroc_glm$Recall, method = "spline"),3)))

prroc_rf = pnl(foreval$pvalue_rf, foreval$targetloanstatus)

# plot PR curve
prroc_rf %>%
  ggplot(aes(x = Recall, y = Precision, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) + labs(title = "PR Curve for glm model") +
  annotate("text", x = 0.6, y = 0.88, label = str_c("AUC = ", round(AUC(prroc_glm$Recall, prroc_glm$Precision, method = "spline"),3)))

# plot ROC curve
prroc_rf %>%
  ggplot(aes(x = fpr, y = Recall, color = Threshold)) +
  geom_line() + scale_color_gradientn(colours = rainbow(3)) + labs(title = "ROC Curve for glm model") +
  annotate("text", x = 0.6, y = 0.5, label = str_c("AUC = ", round(AUC(prroc_glm$fpr, prroc_glm$Recall, method = "spline"),3)))

########
# evaluating lift curves
########

# lift charts
lift_glm = plotlift(foreval$pvalue_glm, foreval$targetloanstatus)
lift_bag = plotlift(foreval$pvalue_glm, foreval$targetloanstatus)


#combine lift data frame for plots
combinelift = data.frame(caseload = lift_glm$caseload,
                         glm = lift_glm$lift,
                         glmbag = lift_bag$lift)

combinelift %>%
  gather(key = model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = model)) + labs(title = "Lift curve with ranked caseload")

########
# evaluating profit curves
########

baseprofit = plotprofit(runif(nrow(foreval),0.01,1), foreval)
# lift charts
profits_glm = plotprofit(foreval$pvalue_glm, foreval)
profits_bag = plotprofit(foreval$pvalue_glm, foreval)

#combine profits data frame for plots
combineprofits = data.frame(caseload = profits_glm$caseload,
                            baseprofit = baseprofit$profits,
                            glm = profits_glm$profits,
                            glmbag = profits_bag$profits)

combineprofits %>%
  gather(key = model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = model)) + labs(title = "Profits curve with ranked caseload")
