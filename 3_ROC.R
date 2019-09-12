
# Compare model performances using resample()
model_list <- list(CART = model_rpart, Random_Forest = model_rf)
models_compare <- resamples(model_list)

# Summary of the models performances
summary(models_compare)


roc.list = list(CART=roc1, Random_Forest=roc2)
test <- lapply(roc.list, ci)
test <- lapply(test, as.vector)
test <- do.call("rbind", test)
colnames(test) <- c("lower", "ROC", "upper")
test <- as.data.frame(test)

summary(models_compare, metric = "ROC")
test

groc(roc.list, aes=c("linetype", "color"), size = 1) +
  theme_minimal() + ggtitle("ROC curve") + legend(title = "Model") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="darkgrey", linetype="dashed")


#-----------------------------------------------------------------------------#
# Generate an ROC Curve.
library(ROCR)
loan_dfpr <- predict(model_rpart, testData2, type = "prob")
pred <- prediction(as.numeric(loan_dfpr), as.numeric(testData$targetloanstatus))

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)






#install.packages("plotROC")
library(plotROC)

set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)


longtest <- melt_roc(test, "D", c("M1", "M2"))
head(longtest)
##     D          M name
## M11 1 1.48117155   M1
## M12 1 0.61994478   M1
## M13 0 0.57613345   M1
## M14 1 0.85433197   M1
## M15 0 0.05258342   M1
## M16 1 0.66703989   M1


# Rattle provides evaluateRisk() and riskchart().


evaluateRisk(predict_set, testData$targetloanstatus)
riskchart(predict_set, testData$targetloanstatus, 
          title="Performance Chart Decision Tree", 
          show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


# Generate an ROC Curve.
library(ROCR)
loan_dfpr <- predict(model_rpart, testData2)
pred <- prediction(as.numeric(loan_dfpr), as.numeric(testData$targetloanstatus))

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)



predict_set = predict(model_rpart, testData2) # Transformed testData
actual_set = testData$targetloanstatus

roc_t = calculate_roc(M=as.numeric(actual_set), D=as.numeric(predict_set), ci = T)

roc.list = list(CART=roc1, Random_Forest=roc2)

calculate_multi_roc(roc.list, M=actual_set, D=as.numeric(predict_set), ci = T)

basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()


basicplot + 
  style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  ggtitle("Themes and annotations") + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2))) +
  scale_x_continuous("1 - Specificity", breaks = seq(0, 1, by = .1))

binorm.plot <- ggplot(binorm.roc, aes(x = FPF, y = TPF, label = c)) + 
  geom_roc(stat = "identity") + style_roc(theme = theme_grey)
binorm.plot




model_glm_1_pred <- 
  predict.glm(object = model_glm_1, newdata = test, type = "response")

model_pred_t <- function(pred, t) ifelse(pred > t, TRUE, FALSE)
caret::confusionMatrix(data = model_pred_t(model_glm_1_pred, 0.5), 
                       reference = test$default,
                       positive = "TRUE")


roc_rf <- pROC::roc(response = test$default, predictor = model_rf_pred)
roc_glm_1

pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               col = "green", print.auc = FALSE, print.auc.y = .4)

legend(x = "bottomright", legend=c("glm_1 AUC = 0.664"), 
       col = c("green"), lty = 1, cex = 1.0)


model_rf_pred <- predict(model_rf, testData2, type = "prob")
roc_rf <- pROC::roc(response = testData$targetloanstatus, predictor = model_rf_pred)
roc_rf
pROC::plot.roc(x = roc_rf, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               col = "green", print.auc = FALSE, print.auc.y = .4)

legend(x = "bottomright", legend=c("AUC = ___"), 
       col = c("green"), lty = 1, cex = 1.0)


