knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE)


library(tidyverse)
library(ggplot2)
library(bibtex)
library(tree)
library(dplyr)
library(leaps)
library(car)
library(randomForest)
library(knitr)

# Read in Vdem Dataset
vdem_data <- readRDS("Data/V-Dem-CY-Full+Others-v11.1.rds")

# Removal of NAs and filtering year to 1980 onwards
malaysia_data <- vdem_data %>%
  filter(country_name == "Malaysia")

coredata <- malaysia_data %>% 
  filter(year >= "1980")

coredata <- coredata %>%
  select_if(~ !any(is.na(.)))

unique_data <- sapply(lapply(coredata, unique), length)
unique_prune <- coredata[ , unique_data >= 3]

# Regex to be used to filter additional versions of variables included in Vdem
# Ordinal scale, mean, standard deviation measures etc. 

versions <- "(_osp|_ord|_codelow|codehigh|_sd|_mean|_nr|_3C|_4C|_5C|e_|commnt)"

prune_ver <- grep(versions, colnames(unique_prune), 
                  value=TRUE, ignore.case =F)

clean_data <- unique_prune %>%
  select(-prune_ver)

# Subset Selection to identify significant variables. 
# v2xnp_regcorr removed due to high collinearity with v2x_corr
set.seed(333)
corrupt_subset <- regsubsets(v2x_corr ~. -v2xnp_regcorr, 
                             data = clean_data, method = "forward")

corsubset_rss <- subsets(corrupt_subset, statistic="rss", 
                         legend = TRUE, max.size = 6, 
                         main = "Corruption Subset: Residual Sum of Squares")

corsubset_cp <- subsets(corrupt_subset, statistic="cp", 
                        legend = TRUE, max.size = 6, 
                        main = "Corruption Subset: Mallow's Cp")

corsubset_bic <- subsets(corrupt_subset, statistic="bic", 
                         legend = TRUE, max.size = 6, 
                         main = "Corruption Subset: Bayesian Information Criteria")

corsubset_adjr2 <- subsets(corrupt_subset, statistic="adjr2", 
                           legend = TRUE, max.size = 6, 
                           main = "Corruption Subset: Adjusted R-squared")

newtree <- tree(v2x_corr ~ v2elsnlfc_13 + v2lginelup + 
                  v2juaccnt + v2clrgstch_3 + v2svdomaut + 
                  v2x_execorr, data = clean_data)

summary(newtree)

plot(newtree, main = "Political Corruption Decision Tree: Malaysia")
text(newtree)


set.seed(777)
treevar.fit <- lm(v2x_corr ~ v2x_execorr + v2juaccnt + 
                    v2clrgstch_3, data = clean_data)

# Variance inflation factor for tree based method
vif(treevar.fit)

set.seed(369)
corrupt_rf <- randomForest(v2x_corr ~. -v2x_rule -v2x_neopat 
                           -v2x_execorr -v2lgcrrpt -v2x_pubcorr 
                           -v2jucorrdc -v2juhccomp -v2xnp_regcorr 
                           -v2juaccnt -v2jupack -v2exembez
                           -v2clrspct -v2xcl_rol -v2jucomp
                           -v2x_jucon -v2mecrit -v2xnp_pres 
                           -v2clacjstm -v2xcl_prpty -v2clprptym 
                           -v2x_clpriv, 
                           data = clean_data)

corrupt_rf

varImpPlot(corrupt_rf, main = "Corrupt RF Variable Importance Plot")

set.seed(0101)
forest.fit <- lm(v2x_corr ~ v2exbribe + v2x_egal + 
                   v2excrptps, data = clean_data)

# Variance inflation factor for forest based method
vif(forest.fit)

summary(forest.fit)

summary(treevar.fit)

# Random Forest model's residual plots
par(mfrow = c(2,2))
plot(forest.fit)

# Decision Tree model's residual plots
par(mfrow = c(2,2))
plot(treevar.fit)

# Removal of NAs and filtering year to 1980 onwards, 
# Country selection of the Philippines only
philippines_data <- vdem_data %>%
  filter(country_name == "Philippines")

testdata <- philippines_data %>% 
  filter(year >= "1980")

testdata <- testdata %>%
  select_if(~ !any(is.na(.)))

# No columns with fewer than 3 unique values will be included, 
# to prevent modelling errors.
# unique_phil created as a vector to allow filtering
unique_data2 <- sapply(lapply(testdata, unique), length)
unique_phil <- testdata[ , unique_data2 >= 3]

# Regex ("versions") will be applied, 
# to filter different versions of key indices
# including ordinal, mean and others. 

versions2 <- "(_osp|_ord|_codelow|codehigh|_sd|_mean|_nr|_3C|_4C|_5C|e_|commnt)"

# Filter variables with non-unique/low variability values 
prune_phil <- grep(versions2, colnames(unique_phil), 
                   value=TRUE, ignore.case =F)

clean_data2 <- unique_phil %>%
  select(-prune_phil)

# Linear Model using tree method's selected  variables
set.seed(121212)
tree.fitphil <- lm(v2x_corr ~ v2x_execorr + 
                     v2juaccnt + v2clrgstch_3, 
                   data = clean_data2)

# Linear Model using random forest method's selected variables
set.seed(212121)
forest.fitphil <- lm(v2x_corr ~ v2exbribe +
                       v2x_egal + v2excrptps,
                     data = clean_data2)


summary(tree.fitphil)


summary(forest.fitphil)

# Decision Tree method based model, residual plots
par(mfrow = c(2,2))
plot(tree.fitphil)

# Random Forest method based model, residual plots
par(mfrow = c(2,2))
plot(forest.fitphil)

# Plot showing differentiation between Malaysia & Philippines
# for the variability between indicators

# Malaysian Indicators
mal_plot <- clean_data %>%
  select(year, v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3) %>%
  pivot_longer(cols = c(v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3),
               names_to = "index", values_to = "values") %>%
  ggplot(aes(year, values, col = index)) +
  geom_line() + 
  geom_point() +
  labs(title = "Figure 1: Key Determinants of Corruption (Malaysia, 1980 - 2020)",
       subtitle = "Judicial accountability and civil liberties rose steeply, in step with a rapid decline in \nboth corruption indices. However, a return to previous levels has occurred only \na few years later",
       x = "Index Values",
       y = "Year")

# Philippine Indicators

phil_plot <- clean_data2 %>%
  select(year, v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3) %>%
  pivot_longer(cols = c(v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3),
               names_to = "index", values_to = "values") %>%
  ggplot(aes(year, values, col = index)) +
  geom_line() + 
  geom_point() +
  labs(title = "Figure 2: Key Determinants of Corruption (Philippines, 1980 - 2020)",
       subtitle = "Judicial accountability has fallen steeply in the Philippines, while other indicators \nremain relatively stable though both corruption indices increased as a response",
       x = "Index Values",
       y = "Year")

par(mfrow = c(1,2))
mal_plot + scale_fill_discrete(labels = c("Civil Liberties",
                                 "Judicial Accountability",
                                 "Political Corruption",
                                 "Executive Corruption"))
phil_plot + scale_fill_discrete(labels = c("Civil Liberties",
                                 "Judicial Accountability",
                                 "Political Corruption",
                                 "Executive Corruption"))

# Removal of NAs and filtering year to 1980 onwards, 
# Country selection of the Taiwan only
tai_data <- vdem_data %>%
  filter(country_name == "Taiwan")

newdata <- tai_data %>% 
  filter(year >= "1980")

newdata <- newdata %>%
  select_if(~ !any(is.na(.)))


# Plot showing variability of indicators

# Taiwanese Indicators
tai_plot <- newdata %>%
  select(year, v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3) %>%
  filter(year >= "1980") %>%
  pivot_longer(cols = c(v2x_corr, v2x_execorr, v2juaccnt, v2clrgstch_3),
               names_to = "index", values_to = "values") %>%
  ggplot(aes(year, values, col = index)) +
  geom_line() + 
  geom_point() +
  labs(title = "Figure 3: Key Determinants of Corruption (Taiwan, 1980 - 2020)",
       subtitle = "Taiwan has lower corruption indices overall, and major increases in civil liberties and judicial \naccountability seem to have a lower impact on reducing corruption, so the relationship between \nindicators may differ by context",
       x = "Index Values",
       y = "Year")

tai_plot

## 
