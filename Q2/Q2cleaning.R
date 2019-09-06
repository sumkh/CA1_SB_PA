pacman::p_load(dplyr, tidyverse, ggplot2, lubridate, reshape2, stringr, car, caret, ggpubr, dlookr, inspectdf)

setwd("C:/Users/nelso/Documents/Masters/EBA5002/CA Doc/data")
#setwd("~/WorkDirectory")

loans_all = read.csv("loans.csv", as.is = TRUE)
summary(loans_all)
loans_all = loans_all %>%
  mutate(potl_profit = intrate*loanamnt,
         loss = case_when(targetloanstatus == 0 ~ 0,
                          targetloanstatus == 1 ~ as.double(loanamnt)),
         profit = case_when(loss == 0 ~ potl_profit,
                            loss != 0 ~ 0))

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
loans$targetloanstatus <- as.factor(loans$targetloanstatus)

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


#-----------------------------------------------------------------------------#
#assume that targetloanstatus defines defaults -> defaulted loans: targetloanstatus == 1

loans %>%
  group_by(targetloanstatus) %>%
  summarise(ratio = n()/nrow(loans), count = n())

#Some data visualization

loans %>%
  group_by(targetloanstatus) %>%
  count(creditpolicy) %>%
  mutate(perc = n/sum(n))

loans %>%
  group_by(grade, targetloanstatus) %>%
  summarize(sum = sum(potl_profit)) %>%
  ungroup() %>%
  mutate(perc = sum/sum(sum))

loans %>%
  group_by(grade) %>%
  summarize(profits = sum(profit), losses = sum(loss), potentialprofits = sum(potl_profit), profitperloss = profits/losses) %>%
  melt(id.vars = "grade") %>%
  ggplot(aes(x = grade)) + 
  geom_col(aes(y = value, fill = variable), position = "dodge") + facet_wrap(~variable, scales = "free_y")

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
  geom_boxplot(aes(y=potl_profit), outlier.shape = NA) + facet_grid(~grade) + 
  scale_y_continuous(limits = c(0,10000))

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

#-----------------------------------------------------------------------------#

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
  ggplot(aes(x = purpose_mod, y = percentage, fill = targetloanstatus)) +
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

write.csv(loans_df, "cleanedloans.csv", row.names = F)

# Generating Reports with dlookr packages
#loans_df %>% diagnose_report(output_format = "html", output_file = "Diagn.html", output_dir = ".")

#loans_df %>% eda_report(target = targetloanstatus, output_format = "html", output_file = "EDA.html", output_dir = ".")
