
pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, ggpubr, DescTools, caret)


#set wd to this R file's current folder.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

foreval = read.csv("foreval.csv", as.is = TRUE)
tofactor = c("targetloanstatus","creditpolicy","term")
foreval[,tofactor] = lapply(foreval[,tofactor], as.factor)
set.seed(2019)

########
# Evaluation Functions
########
prroc = function(predict, reference) {
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
    cm = confusionMatrix(data = as.factor(as.numeric(predict>i)), reference = reference,positive="1")
    precision = cm[["byClass"]][["Precision"]]
    recall = cm[["byClass"]][["Recall"]]
    f1 = cm[["byClass"]][["F1"]]
    fpr = 1- cm[["byClass"]][["Specificity"]]
    mydf[nrow(mydf) + 1,] = list(i,precision, recall, f1, fpr)
  }
  return(mydf)
}

plotlift = function(predict, reference) {
  set.seed(2901)
  caseload = seq(0.01,1,0.01)
  a = data.frame(prob = predict, default = reference)
  b = cbind(arrange(a, desc(predict)), random = sample(reference))
  mydf = data.frame(caseload = numeric(),
                    lift = numeric(),
                    threshold = numeric())
  for (i in caseload) {
    predictdefault = (b %>% top_n(i*nrow(b), wt = prob) %>% count(default))[2,2]
    randomdefault = (b %>% top_n(i*nrow(b), wt = prob) %>% count(random))[2,2]
    lift = as.numeric(predictdefault/randomdefault)
    threshold = (b %>% top_n(i*nrow(b), wt = prob) %>% top_n(-1, wt = prob))[1,1]
    mydf[nrow(mydf) + 1,] = list(i, lift, threshold)
  }
  return(mydf)
}

plotprofit = function(predict, foreval) {
  caseload = seq(0.01,1,0.01)
  a = data.frame(prob = predict, foreval)
  mydf = data.frame(caseload = numeric(),
                    profits = numeric(),
                    threshold = numeric())
  for (i in caseload) {
    profits = (a %>% top_n(-i*nrow(a), wt = prob) %>% summarise(total = sum(profit) - sum(loss)))[1,1]
    threshold = (a %>% top_n(-i*nrow(a), wt = prob) %>% top_n(1, wt = prob))[1,1]
    mydf[nrow(mydf) + 1,] = list(i, profits, threshold)
  }
  return(mydf)
}

########
# evaluating ROC and PR curves curves

########
prroc_glm = prroc(foreval$pvalue_glm, foreval$targetloanstatus)
prroc_bag = prroc(foreval$pvalue_bag, foreval$targetloanstatus)
prroc_tree = prroc(foreval$pvalue_tree, foreval$targetloanstatus)
prroc_forest = prroc(foreval$pvalue_forest, foreval$targetloanstatus)
prroc_boosttree = prroc(foreval$pvalue_boosttree, foreval$targetloanstatus)
prroc_boostlinear = prroc(foreval$pvalue_boostlinear, foreval$targetloanstatus)
prroc_pca = prroc(foreval$pvalue_pca, foreval$targetloanstatus)
prroc_NN = prroc(foreval$pvalue_NN, foreval$targetloanstatus)

combinerecall = data.frame(threshold = prroc_glm$Threshold,
                           glm = prroc_glm$Recall,
                           glmbag = prroc_bag$Recall,
                           tree = prroc_tree$Recall,
                           forest = prroc_forest$Recall,
                           boosttree = prroc_boosttree$Recall,
                           boostlinear = prroc_boostlinear$Recall,
                           pca = prroc_pca$Recall,
                           NN = prroc_NN$Recall)

combineprecision = data.frame(threshold = prroc_glm$Threshold,
                              glm = prroc_glm$Precision,
                              glmbag = prroc_bag$Precision,
                              tree = prroc_tree$Precision,
                              forest = prroc_forest$Precision,
                              boosttree = prroc_boosttree$Precision,
                              boostlinear = prroc_boostlinear$Precision,
                              pca = prroc_pca$Precision,
                              NN = prroc_NN$Precision)

combinefpr = data.frame(threshold = prroc_glm$Threshold,
                        glm = prroc_glm$fpr,
                        glmbag = prroc_bag$fpr,
                        tree = prroc_tree$fpr,
                        forest = prroc_forest$fpr,
                        boosttree = prroc_boosttree$fpr,
                        boostlinear = prroc_boostlinear$fpr,
                        pca = prroc_pca$fpr,
                        NN = prroc_NN$fpr)

# plot PR curves
cbind(combinerecall %>%
        gather(key = Model, value = Recall, -threshold),
      combineprecision %>%
        gather(key = Model, value = Precision, -threshold) %>%
        select(Precision)) %>%
  ggplot(aes(x = Recall, y = Precision, color = Model)) +
  geom_line() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "PR Curves")

#Compute AUC
cbind(combinerecall %>%
        gather(key = Model, value = Recall, -threshold),
      combineprecision %>%
        gather(key = Model, value = Precision, -threshold) %>%
        select(Precision)) %>%
  filter(Model == "glm") -> mydf
#Change above model to compute AUC
AUC(mydf$Recall, mydf$Precision, method = "spline")

# plot ROC curve
cbind(combinefpr %>%
        gather(key = Model, value = fpr, -threshold),
      combinerecall %>%
        gather(key = Model, value = Recall, -threshold) %>%
        select(Recall)) %>%
  ggplot(aes(x = fpr, y = Recall, color = Model)) +
  geom_line() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "ROC curves")

#Compute AUC
cbind(combinefpr %>%
        gather(key = Model, value = fpr, -threshold),
      combinerecall %>%
        gather(key = Model, value = Recall, -threshold) %>%
        select(Recall)) %>%
  filter(Model == "glm") -> mydf2
#Change above model to compute AUC
AUC(mydf2$fpr, mydf2$Recall, method = "spline")

########
# evaluating lift curves
########
set.seed(2019)
baselift = plotlift(runif(nrow(foreval),0.01,1), foreval$targetloanstatus)
# lift charts
lift_glm = plotlift(foreval$pvalue_glm, foreval$targetloanstatus)
lift_bag = plotlift(foreval$pvalue_bag, foreval$targetloanstatus)
lift_tree = plotlift(foreval$pvalue_tree, foreval$targetloanstatus)
lift_forest = plotlift(foreval$pvalue_forest, foreval$targetloanstatus)
lift_boosttree = plotlift(foreval$pvalue_boosttree, foreval$targetloanstatus)
lift_boostlinear = plotlift(foreval$pvalue_boostlinear, foreval$targetloanstatus)
lift_pca = plotlift(foreval$pvalue_pca, foreval$targetloanstatus)
lift_NN = plotlift(foreval$pvalue_NN, foreval$targetloanstatus)

#combine lift data frame for plots
combinelift = data.frame(caseload = lift_glm$caseload,
                         random = baselift$lift,
                         glm = lift_glm$lift,
                         glmbag = lift_bag$lift,
                         tree = lift_tree$lift,
                         forest = lift_forest$lift,
                         boosttree = lift_boosttree$lift,
                         boostlinear = lift_boostlinear$lift,
                         pca = lift_pca$lift,
                         NN = lift_NN$lift)

#plot for linear models
combinelift %>%
  select(caseload, random, glm, glmbag, boostlinear, pca) %>%
  gather(key = Model, value = value, -caseload) %>% 
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("glmbag","glm","pca","boostlinear","random")))) + labs(title = "Lift charts with ranked caseload") +
  scale_color_manual(values = c("red3", "orchid2", "mediumpurple", "darkgoldenrod", "black"),
                     name = "Model")

combinelift %>%
  select(caseload, random, tree, forest, boosttree) %>%
  gather(key = Model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("forest","boosttree","tree","random")))) + labs(title = "Lift charts with ranked caseload") +
  scale_color_manual(values = c("green4", "cyan", "yellowgreen", "black"),
                     name = "Model")

combinelift %>%
  select(caseload, random, forest, glmbag, NN) %>%
  gather(key = Model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("forest","glmbag","NN","random")))) + labs(title = "Lift charts with ranked caseload") +
  scale_color_manual(values = c("green4", "red3", "gold2", "black"),
                     name = "Model")

########
# evaluating profit curves
########

baseprofit = plotprofit(runif(nrow(foreval),0.01,1), foreval)
# lift charts
profits_glm = plotprofit(foreval$pvalue_glm, foreval)
profits_bag = plotprofit(foreval$pvalue_bag, foreval)
profits_tree = plotprofit(foreval$pvalue_tree, foreval)
profits_forest = plotprofit(foreval$pvalue_forest, foreval)
profits_boosttree = plotprofit(foreval$pvalue_boosttree, foreval)
profits_boostlinear = plotprofit(foreval$pvalue_boostlinear, foreval)
profits_pca = plotprofit(foreval$pvalue_pca, foreval)
profits_NN = plotprofit(foreval$pvalue_NN, foreval)

#combine profits data frame for plots
combineprofits = data.frame(caseload = profits_glm$caseload,
                            random = baseprofit$profits,
                            glm = profits_glm$profits,
                            glmbag = profits_bag$profits,
                            tree = profits_tree$profits,
                            forest = profits_forest$profits,
                            boosttree = profits_boosttree$profits,
                            boostlinear = profits_boostlinear$profits,
                            pca = profits_pca$profits,
                            NN = profits_NN$profits)

combineprofits %>%
  select(caseload, random, glm, glmbag, boostlinear, pca) %>%
  gather(key = Model, value = value, -caseload) %>% 
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("glmbag","glm","pca","boostlinear","random")))) + labs(title = "Profit charts with ranked caseload") +
  scale_color_manual(values = c("red3", "orchid2", "mediumpurple", "darkgoldenrod", "black"),
                     name = "Model")

combineprofits %>%
  select(caseload, random, tree, forest, boosttree) %>%
  gather(key = Model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("forest","boosttree","tree","random")))) + labs(title = "Profit charts with ranked caseload") +
  scale_color_manual(values = c("green4", "cyan", "yellowgreen", "black"),
                     name = "Model")

combineprofits %>%
  select(caseload, random, forest, glmbag, NN) %>%
  gather(key = Model, value = value, -caseload) %>%
  ggplot(aes(x = caseload, y = value)) + 
  geom_line(aes(color = factor(Model, levels = c("forest","glmbag","NN","random")))) + labs(title = "Profit charts with ranked caseload") +
  scale_color_manual(values = c("green4", "red3", "gold2", "black"),
                     name = "Model")

colMax <- function(data) {
  sapply(data, max, na.rm = TRUE)
}

colMax(combineprofits)
colMax(combinelift)
