##################################################################################################
# SB and PA CA - Question 2 Loans Approval

# load library - pacman::p_load(tidyverse, caret, corrplot, caTools,knitr,car, ROCR,IRdisplay, repr)
pacman::p_load(tidyverse, caret, corrplot, caTools,knitr,car, 
               ROCR,IRdisplay, e1071, earth)

library(scales)
# set working directory
setwd("~/WorkDirectory")

# read csv file
loan =  read.csv('loans.csv')

# data exploration
head(loan)

str(loan)

summary(loan)

# set the creditpolicy and targetloanstatus variables to factors (categorical data)
cols =  c('creditpolicy', 'targetloanstatus')

loan[,cols] = lapply(loan[,cols], as.factor)

str(loan)
summary(loan)

# check for number of na in loans dataset
colMeans(is.na(loan))

# na observed for annualinc, delinq2yrs,inqlast6mths, openacc, revolutil, totalacc. Impute NA entries for 
# annualinc, delinq2yrs,inqlast6mths, openacc, revolutil with zero.
# Since number of missing entries for totalacc < 5% of total dataset, we discard these missing entries
#na_to_zero_vars =
#  c("annualinc", "delinq2yrs",
#    "inqlast6mths", "openacc", "revolutil")

#loan2 = 
#  loan %>%
#  mutate_at(.vars = na_to_zero_vars, .funs = list(~replace(., is.na(.), 0)))

#colMeans(is.na(loan2))

# Since number of missing entries <5% of total dataset, we discard these missing entries
# filter out complete entries for analysis

loan = loan %>%
        filter(complete.cases(.))

str(loan)
summary(loan)

# explore data using ggplot (loanamnt, grade, homeownership, annualinc, emplength, term
# int_rate, verification_status, loan_status, purpose

# 1) plot graph on current loan default status 

loan$targetloanstatus <- factor(loan$targetloanstatus, levels = c("0", "1"),
                  labels = c("No Default", "Default"))

# check for the 0 and 1 using contrasts
contrasts(loan$targetloanstatus)

loan %>%
  group_by(targetloanstatus)%>%
  summarise(per = n()/nrow(loan))%>%
  ggplot(aes(x=targetloanstatus, y=per, fill = targetloanstatus)) + 
  geom_bar(stat='identity') +
  geom_text(aes(label = round(per, 2)), vjust = 2) + labs(fill = "Loan Status", title = "Summary of Current Loan Status", x = "Loan Status", y = "Percentage")

# 15% of borrowers defaulted on loan

# 2) plot credit policy against loan status

loan %>%
  group_by(creditpolicy, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = creditpolicy, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = "fill", stat = 'identity') +
  labs(title="Current Loan Status by Credit Policy", x = "Credit Policy", y = "Percentage", fill = "Loan Status")

# higher percentage of default for credit policy = 0

# 3) plot graph of of loan amount against loan status

loan %>%
  ggplot(aes(x = loanamnt)) + geom_histogram(bins = 15) + facet_grid(~targetloanstatus) + labs(x = "Loan Amount", y = "Number") 

# 4) Plot loan status against term

loan %>%
  group_by(term, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = term, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(title="Current Loan Status by Term", x = "Term", y = "Percentage", fill = "Loan Status") 

# higher chance of default for long term (60 months) loan

# 5) Plot distribution of installment amount

loan %>%
  ggplot(aes(x = installment)) + geom_histogram(bins = 30) + facet_grid(~targetloanstatus) + labs(x = "Installment Amount", y = "Number") 

# distribution for installment amount and loan amount are quite similiar

# 6a) plot of loan amount against grade and loan status

loan %>%
  ggplot(aes(grade, loanamnt)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  scale_y_continuous() +
  facet_wrap(~ targetloanstatus) +
  labs(title="Loan Amount by Grade", x = "Grade", y = "Loan Amount")

# Typically higher loan amount at lower grade
# Outliers are common for grades A - D
# the loan amount spread (IQR) is higher for lower grade loans

# 6b) Plot loan status against grade

loan %>%
  group_by(grade, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = grade, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(title="Current Loan Status by Grade", x = "Grade", y = "Percentage", fill = "Loan Status") 

# higher chance of default for lower grade

# 9) plot credit policy against loan status

# 7) plot employment length against loan status

loan %>%
  group_by(emplength, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = emplength, y = percentage, fill = targetloanstatus)) +
  geom_bar(position = "fill", stat = 'identity') +
  labs(title="Current Loan Status by Employment Length", x = "Employment Length", y = "Percentage", fill = "Loan Status")

# 8) plot home ownership against loan status

loan %>%
  group_by(homeownership, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = homeownership, y = count_level, fill = targetloanstatus)) +
  geom_bar(stat = 'identity') +
  labs(title="Current Loan Status by Homeownership", x = "Home Ownership", y = "Number", fill = "Loan Status")

# Very few home ownership status are none and others
# propose to combine home ownership status to factor 0/1 - 1 for mortgage and own and 0 for all other categories

# 9a) plot boxplot of loan status by annual income 

loan %>%
  ggplot(aes(x=0, y = annualinc)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~ targetloanstatus) +
  labs(title="Annual Income Distribution", y = "Annual Income") +
  scale_y_continuous(labels = comma)

# higher loan count at lower income level

# 9b) Alternative plot: graph of loan status by annual income 
loan %>%
  ggplot(aes(x = annualinc)) + geom_histogram(bins = 50) + labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Number") + scale_x_continuous(labels = comma)


# 10) Plot of verification status 

loan %>%
  group_by(verificationstatus, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = verificationstatus, y = count_level, fill = targetloanstatus)) +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(title="Verification Status", x = "Verification Status", y = "Number", fill = "Loan Status")

# verification status not key factor influencing default status

# 11) Plot of purpose 

loan %>%
  group_by(purpose, targetloanstatus) %>%
  summarise(count_level = n(), percentage = n()/nrow(loan)) %>%
  ggplot(aes(x = purpose, y = count_level, fill = targetloanstatus)) +
  geom_bar(stat = 'identity') +
  labs(title="Purpose", x = "Purpose", y = "Number", fill = "Loan Status")


# 12) Plot interest rate by grade and term
loan %>%
  ggplot(aes(grade, intrate)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  scale_y_continuous() +
  labs(title="Interest Rate by Grade", x = "Grade", y = "Interest Rate") +
  facet_wrap(~ term)

# interest rate is higher for longer period of loan
# Interest rate increases as grade gets lower


# 13) plot delinquency distribution

loan %>%
  ggplot(aes(x = delinq2yrs)) + geom_histogram(bins = 30) + facet_grid(~targetloanstatus) + labs(x = "Delinquency Incidents", y = "Number") 

# Plot of revolving balance
loan %>%
  ggplot(aes(x=0, y = revolbal)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~ targetloanstatus) +
  labs(title="Revolving Balance", y = "Revolving Balance") +
  scale_y_continuous(labels = comma)

# Correlation Plot
corrplot::corrplot(cor(loan2[, sapply(loan2, is.numeric)],
                       use="complete.obs"), method = "number", type='lower')

# loan amount and installment, totalacc and openacc are highly correlated and cannot exist in the same model
# installment can be derived from loan amount, term and interest rate

# Feature engineering
# create new variable - amount of credit the borrower is using = revolutil * revolbal

loan$credit = loan$revolutil * loan$revolbal

# create new variable, homeownership_mod

unique(loan$homeownership)

loan = loan%>%
  mutate(homeownership_mod = recode_factor(homeownership, "MORTGAGE" = "1", "OWN" = "1", "RENT" = "0", "OTHER" = "0", "NONE" = "0")) 

str(loan$homeownership_mod)

# remove n/a from emplength

loan= loan %>%
  group_by(emplength) %>%
  filter (emplength != "n/a")

unique(loan$emplength)

# Binning
loan$level[loan$annualinc < 100000] = 'low Income'
loan$level[loan$annualinc < 500000 & loan$annualinc >= 100000] = 'Middle Income'
loan$level[loan$annualinc >= 500000] = 'High Income'

loan$level = factor(loan$level, ordered = TRUE)
str(loan)
###################################################################################################
#set initial seed
set.seed(123)

# create a boolean flag to split data

train_index <- 
  caret::createDataPartition(y = loan$targetloanstatus, times = 1, 
                             p = .7, list = FALSE)

#split_data
# create train and test datasets
train_set = loan[train_index,]
nrow(train_set)/nrow(loan)
test_set = loan[-train_index,]
nrow(test_set)/nrow(loan)

colnames(train_set)

# imbalance on training set
library(ROSE)
table(train_set$targetloanstatus)
# balanced data set with both over and under sampling
balanced <- ovun.sample(targetloanstatus~., data=train_set,
                                p=0.5,
                                seed=1, method="under")$data
table(balanced$targetloanstatus)

# use train to create our 1st model
# use all independent variables 
model = glm(targetloanstatus ~ ., data = train_set, family = binomial)
summary(model)
