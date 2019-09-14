pacman::p_load(dplyr, tidyverse, ggplot2, reshape2, car, caret, ggpubr, boot)

# bagging a glm model

bagglm = function(model, agg = 10) {
  # takes a glm model input and returns a list of aggregate of models.
  df = model[["model"]]
  formula = formula(model)
  modellist = list()
  for ( i in 1:10) {
    a = sample_n(df, 0.9*nrow(df), replace = TRUE)
    modellist[[length(modellist) + 1]] = glm(formula, family=binomial,  data=a)
  }
  return(modellist)
}

#predicting with a bagged glm model.
predictbag = function(model, test_df, method = "mean") {
  # method = max or mean
  predictlist = data.frame()[1:nrow(test_df),]
  for (i in model) {
    predictlist = cbind(predictlist,predict(i, newdata = test_df, type = "response"))
  }
  if (method == "max") {
    predictlist$p = apply(predictlist,1,max)
  } else {
    predictlist$p = apply(predictlist,1,mean)
  }
  return(predictlist$p)
}
