
pacman::p_load(dplyr, tidyverse, ggplot2, lubridate, reshape2, stringr, car, caret, ggpubr)

loans_all = read.csv("loans.csv", as.is = TRUE)
summary(loans_all)

# data exploration
# check if all lines are filled
# remove data with missing values since instances of NA is not significant
loans = loans_all[complete.cases(loans_all),]
glimpse(loans)
summary(loans)

# code dependent variable target loan status as factor and plot graph
loans$targetloanstatus <- factor(loans$targetloanstatus, levels = c("0", "1"),
                                 labels = c("No Default", "Default"))

# check for the 0 and 1 using contrasts
contrasts(loans$targetloanstatus)

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

#this is to compute percentage default against ordinal variables.

#-----------------------------------------------------------------------------#

# clean emplength
table(loans$term, useNA = "always")
#loans$term %>%
#  str_extract("\\d{1,2}") -> loans$term
loans$term = str_remove_all(loans$term, "\\s")


table(loans$emplength, useNA = "always")
loans$emplength %>%
  str_replace("< 1", "0") %>%
  str_extract("\\d{1,2}") -> loans$emplength
loans$emplength = as.numeric(loans$emplength)
loans = na.omit(loans)

# Create 2 new variables for emp10years and delinq2ears
loans = loans %>%
  mutate(emp10years = as.factor(case_when(emplength > 9 ~ "Y",
                                          emplength <= 9 ~ "N")),
         emp10years = as.factor(case_when(emplength > 9 ~ "Y",
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
loans$annualinc_bin = as.character(loans$annualinc_bin)
str(loans$annualinc_bin)

loans %>% 
  group_by(annualinc_bin) %>%
  summarise(min(annualinc), max(annualinc))

# Binning the revolbal
loans$revolbal_bin = dlookr::binning(loans$revolbal, nbins = 5, type = "quantile", 
                                     ordered = T, labels = paste0("Gp",seq(1:5)))

table(loans$revolbal_bin, useNA = "always")
class(loans$revolbal_bin)
loans$revolbal_bin = as.character(loans$revolbal_bin)
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

# creditpolicy
table(loans$creditpolicy)
loans$creditpolicy = factor(as.character(loans$creditpolicy), levels = c("0","1"), labels = c("N","Y"))
contrasts(loans$creditpolicy)
table(loans$creditpolicy)

# Converting catagorical variables as characters
glimpse(loans)
loans = loans %>% mutate_if(is.factor, as.character)


# Selecting Features for Modelling
loans_df = select(loans, -c("id","homeownership","annualinc", "revolbal","verificationstatus","delinq2yrs", "purpose"))

# Factorise catagorical variables
loans_df$targetloanstatus = factor(loans_df$targetloanstatus, levels = c("No_Default", "Default"))
loans_df = loans_df %>% mutate_if(is.character, as.factor)

glimpse(loans_df)
library("inspectdf")
#inspect_cat(loans_df) %>% show_plot()
#inspect_num(loans_df) %>% show_plot()

write.csv(loans_df, "loans_df.csv", row.names = F)