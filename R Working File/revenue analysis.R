getwd()
setwd("C:/Users/lenovo/Documents/R in CU/APAN 5205/Project/raw data")

rev = read.csv("sort by rev.csv", stringsAsFactors = F)
str(rev)

library(dplyr)
library(lubridate)
library(tidyverse)
library(psych)
library(rpart); library(rpart.plot)
library(mgcv)

####--------------------------Explore data type-----------------------------####

#Convert revenue into numeric
rev$revenue <- gsub(",", "", rev$revenue)
rev$revenue <- as.numeric(rev$revenue)

#Relabel rerun variable
rev <- rev %>%
  mutate(rerun = case_when(
    rerun == 0 ~ "a",
    rerun == 1 ~ "b",
    rerun == 2 ~ "c",
    rerun == 3 ~ "d",
    TRUE ~ as.character(rerun)))

#Convert date of birth into birth month and birth day columns
rev <- rev %>% 
  mutate(rev, 
         birthmonth = sub("\\d+-", "", birthday),
         birthday = paste0(gsub("\\D", "", birthday)))

#Convert release date into release year and release month columns
rev$release_date = as.Date(rev$release_date, format = "%Y/%m/%d")
rev <- rev %>% 
  mutate(release_year = year(release_date)) %>% 
  select(-release_date)

#Convert max_lv_hp into numeric
rev$max_lv_hp <- gsub(",", "", rev$max_lv_hp)
rev$max_lv_hp <- as.numeric(rev$max_lv_hp)

#Convert speical stat into numeric
special_cols <- grep("special_", names(rev), value = TRUE)
special_cols <- special_cols[!grepl("special_dish", special_cols)]
rev[special_cols] <- sapply(rev[special_cols], function(x) as.numeric(gsub("%", "", x)) / 100)

#Separate model type
rev <- rev %>%
  separate(model, into = c("size", "gender"), sep = " ")

str(rev)


####--------------------standardize numerical variable----------------------####
head(rev)
rev[, 35:50] <- scale(rev[, 35:50])
rev <- rev %>% select(-special_0, -special_1)
sum(is.na(rev))


####-------------------------feature selection------------------------------####

#####stepwise selection#####
rev1 <- rev %>% 
  select(-id, -character_name, -rarity, -voice_eng, -voice_cn, -voice_jp, -voice_kr, 
         -special_dish, -talent_material, -talent_book_1.2,	-talent_book_2.3,
         -talent_book_3.4, -talent_book_4.5, -talent_book_5.6, -talent_book_6.7,
         -talent_book_7.8,-talent_book_8.9)

start_mod = lm(revenue~1, data = rev1)
empty_mod = lm(revenue~1, data = rev1)
full_mod = lm(revenue~., data = rev1)
hybridStepwise = step(start_mod, scope = list(upper = full_mod, lower = empty_mod), direction = "both")
summary(hybridStepwise)


####----------------------------Regression----------------------------------####

#####Method 1: linear regression: combining stepwise selection#####
lm_model = lm(revenue~base_atk + rerun + special_2 + talent_weekly + special_6 + 
                release_year + size, data = rev1)
summary(lm_model)

#Build a coeffecient table
coefficients <- as.data.frame(summary(lm_model)$coefficients)
colnames(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
coefficients

#Plot the partial effefct graph
library(effects)
plot(allEffects(lm_model)$base_atk)
plot(allEffects(lm_model)$special_2)
plot(allEffects(lm_model)$special_6)

rerun_df <- as.data.frame(allEffects(lm_model)$rerun)
rerun <- ggplot(rerun_df, aes(x = factor(rerun), y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  labs(x = "rerun", y = "revenue")
rerun

effect_df <- as.data.frame(allEffects(lm_model)$talent_weekly)
talent_weekly <- ggplot(effect_df, aes(x = factor(talent_weekly), y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "talent weekly", y = "revenue")
talent_weekly

#Anova analysis
anova(lm_model)
pred_lm = predict(lm_model)
rmse_lm = sqrt(mean((pred_lm-rev1$revenue)^2)); rmse_lm

#Prediction for the new character Arlecchino
df <- data.frame("base_atk" = 1.71054410, "rerun" = "a", "special_2" = 0.67404647, 
                 "talent_weekly" = "Ashen Heart", 
                 "special_6" = 0.67392712, "release_year" = 2024,
                 "size" = "Tall")
predict_new = predict(lm_model, newdata = df)


#####Method 2: nonlinear regression-gam#####

gam_model <- gam(revenue~s(base_atk) + rerun + s(special_2, k = 8) + talent_weekly + 
                   s(special_6, k = 8) + release_year + size, method = 'REML', 
                 data = rev1)
summary(gam_model)
pred_gam = predict(gam_model)
rmse_gam = sqrt(mean((pred_gam-rev1$revenue)^2)); rmse_gam

unique(rev1$special_6)

#####Method 3: regression tree#####
rev2 <- rev %>% 
  select(-id, -character_name)
tree1 = rpart(revenue~.,data = rev2, method = 'anova')
summary(tree1)
tree1$variable.importance
rpart.plot(tree1)
pred_tree = predict(tree1)
rmse_tree = sqrt(mean((pred_tree-rev2$revenue)^2)); rmse_tree


####----------------------------rmse table----------------------------------####
model <- c("Multiple linear regression", "Generalized Additive Model", 
           "Decision Tree Model")
rmse <- c(rmse_lm, rmse_gam, rmse_tree)

rmse_df <- data.frame("model" = model, "rmse" = rmse)
rmse_df

####-------------------------Association rules------------------------------####
str(rev1)
chr_vars <- rev1 %>% 
  select(where(is.character))
write.csv(chr_vars, file = 'association rule.csv', row.names = F)

items = read.transactions('association rule.csv', format='basket', sep=',', header = T)
as(items, 'data.frame')

itemFrequencyPlot(items, support = 0.0, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue2", las = 1, topN=5,
                  xlab = paste("Proportion of Market Baskets Containing Item"))
crossTable(items, measure = 'count', sort = T)

rules_all = apriori(items, parameter = list(support = 0.0, confidence = 0))
summary(rules_all)
x = inspect(rules_all)
























