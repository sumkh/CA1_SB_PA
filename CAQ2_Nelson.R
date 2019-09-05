
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

#set initial seed for reproducibility
set.seed(123)
# collect the data indices returned in a list
inds = createDataPartition(loans_df$targetloanstatus, p=0.7, list=FALSE,times=1) 

train_set = loans_df[inds,]
nrow(train_set)/nrow(loans_df)
dim(train_set)

test_set = loans_df[-inds,]
nrow(test_set)/nrow(loans_df)



# ------------------------------------------------------------------#

# Modelling ------------------------------------------------------------------#

#assume that targetloanstatus defines defaults -> defaulted loans: targetloanstatus == 1
glimpse(loans)
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

# 36036 non default cases and 6409 default cases. 89% chance of non-default. Create our training set using stratified sampling.
set.seed(42)

#using sample_n to downsample.
trainset = loans %>%
  group_by(targetloanstatus) %>%
  sample_n(3000)

# use caret to downsample.
trainset_caret = caret::downSample(loans, y = as.factor(loans$targetloanstatus), list = FALSE, yname = "default")

# create dummy variables
dummies_train <- dummyVars("~. -id -targetloanstatus", data = trainset_caret, 
            fullRank = FALSE)

train_down_dummy <-
  trainset_caret %>%
  select(-grade) %>%
  cbind(predict(dummies_train, newdata = trainset_caret))

#glm model
model1 = trainset_caret %>%
  glm(default ~ revolutil + grade, data = ., family = binomial)
summary(model1)

#Randomforest
# Create model with default paramters
# train control to decide how we want to do the training. eg. "repeatedcv: repeated crossvalidation, 10-fold, repeated 3 times.
control <- trainControl(method="repeatedcv", number=10, repeats=3)
trainset_caret2 = sample_n(trainset_caret, 50)

#this takes a while to run.
model2 = train(default ~ revolutil + grade,data = trainset_caret2, model = "rf", metric = "Accuracy", trControl = control)
print(model2)
  # use caret and compute a confusion matrix
pdata <- predict(model1, newdata = trainset_caret, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata>0.5)), reference = trainset_caret$default)

#create a confusion matrix of original data set.
pdata2 = predict(model1, newdata = loans, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = loans$targetloanstatus)

#some exploration
trainset %>%
  group_by(targetloanstatus) %>%
  summarise(avgint = mean(intrate), avginsta = mean(installment))

trainset %>%
  group_by(targetloanstatus) %>%
  count()

