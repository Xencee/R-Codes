
library(tidyverse)

# Read in file and rename columns
setwd()
santd <- read.csv('train_ver2_0.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)
s_colname <- read.csv('santander_column_desc.csv', header = TRUE, sep = ',', 
                      stringsAsFactors = FALSE)
s_colname <- t(s_colname[3])
colnames(santd) <- s_colname
# write.csv(santd, 'santander.csv', row.names = FALSE)
# santd <- read.csv('santander.csv')

# Select 300000 customer codes from August 2015
unique(santd$date1)
custs <- santd %>% 
  filter(cust_code, date1 == '2015-08-28') %>% 
  slice(1:300000) %>% 
  select(cust_code)

# Create new data frame with cust_codes from custs
set1 <- subset(santd, cust_code %in% custs$cust_code) 

# Create month column
set1$month1 <- as.integer(substr(set1$date1, 6, 7))

# Save new object as csv and RDS
write.csv(set1, 'set1.csv', row.names = FALSE)
# set1 <- read.csv('set1.csv')
saveRDS(set1, 'set1.rds')
# set1 <- readRDS('set1.rds')

# View nuber of credit cards per month
set1 %>% select(month1, credit_cards) %>% 
  group_by(month1, credit_cards) %>% count(credit_cards)

# Create labels for when data in month1==7 or 8; create new flag colimn and select data
month8 <- set1 %>% select(cust_code, month1, credit_cards) %>% 
  filter(cust_code, month1 == 8)
month7 <- set1 %>% select(cust_code, month1, credit_cards) %>% 
  filter(cust_code, month1 == 7)
setlabels <- left_join(month8, month7, by = "cust_code")
setlabels$new_card <- as.integer(ifelse(setlabels$credit_cards.x > setlabels$credit_cards.y, 1, 0))
setlabels <- setlabels[c(1, 6)]

# View count of columns in dataset
table(setlabels$new_card)
setlabels %>%
  group_by(new_card) %>%
  summarise(n_distinct(cust_code))

# Cleaning the dataset
set1 <- set1[, -c(1,7,11)]
set1[, c(5,7)] <- sapply(set1[, c(5,7)], as.numeric)
set1[,9] <- as.factor(gsub('.0', '', set1[,9]))

# Find out what columns are already factors
names <- colnames(set1)
factors <- sapply(set1, is.factor)
colnames <- names[factors]

# Create new features with levels of columns
str(set1)

# emp_code
levels(set1$emp_code) 
unique(set1$emp_code)
set1$duplicate <- set1$emp_code
levels(set1$duplicate) <- c(1:6)
emp_code <- set1 %>% spread(key = emp_code, value = duplicate)
emp_code <- emp_code[,c(1, 46:50)] # Remove empty column 
saveRDS(emp_code, 'emp_code.rds')

# country
levels(set1$country)
unique(set1$country)
set1$duplicate <- set1$country
levels(set1$duplicate) <- c(1:117)
country <- set1 %>% spread(key = country, value = duplicate)
country <- country[,c(1, 47:116)]# Remove empty column 
saveRDS(country, 'country.rds')

# gender
levels(set1$gender) 
unique(set1$gender)
set1$duplicate <- set1$gender
levels(set1$duplicate) <- c(1:3)
gender <- set1 %>% spread(key = gender, value = duplicate)
gender <- gender[,c(1, 47:48)]# Remove empty column 
saveRDS(gender, 'gender.rds')

# mo_starty_cust_type
levels(set1$mo_starty_cust_type) 
unique(set1$mo_starty_cust_type)
set1$duplicate <- set1$mo_starty_cust_type
levels(set1$duplicate) <- c(1:6)
cust_type <- set1 %>% spread(key = mo_starty_cust_type, value = duplicate)
cust_type <- cust_type[,c(1, 47:51)]# Remove empty column 
saveRDS(cust_type, 'cust_type.rds')

# Cust_status
levels(set1$Cust_status)
unique(set1$Cust_status)
set1$duplicate <- set1$Cust_status
levels(set1$duplicate) <- c(1:5)
cust_status <- set1 %>% spread(key = Cust_status, value = duplicate)
cust_status <- cust_status[,c(1, 47:50)]# Remove empty column 
saveRDS(cust_status, 'cust_status.rds')

# Resident
levels(set1$Resident)
unique(set1$Resident)
set1$duplicate <- set1$Resident
levels(set1$duplicate) <- c(1:3)
resident <- set1 %>% spread(key = Resident, value = duplicate)
resident <- resident[,c(1, 47:48)]# Remove empty column 
saveRDS(resident, 'resident.rds')

# Foreigner
levels(set1$Foreigner)
unique(set1$Foreigner)
set1$duplicate <- set1$Foreigner
levels(set1$duplicate) <- c(1:3)
foreigner <- set1 %>% spread(key = Foreigner, value = duplicate)
foreigner <- foreigner[,c(1, 47:48)]# Remove empty column 
saveRDS(foreigner, 'foreigner.rds')

# emp_spouse
levels(set1$emp_spouse)
unique(set1$emp_spouse)
# The column is empty

# Acq_channel
levels(set1$Acq_channel)
unique(set1$Acq_channel)
set1$duplicate <- set1$Acq_channel
levels(set1$duplicate) <- c(1:160)
acq_channel <- set1 %>% spread(key = Acq_channel, value = duplicate)
acq_channel <- acq_channel[,c(1, 47:165)]# Remove obviously empty column
saveRDS(acq_channel, 'acq_channel.rds')

# Deceased
levels(set1$Deceased)
unique(set1$Deceased)
set1$duplicate <- set1$Deceased
levels(set1$duplicate) <- c(1:3)
deceased <- set1 %>% spread(key = Deceased, value = duplicate)
deceased <- deceased[,c(1, 47:48)]# Remove column that contained empty rows
saveRDS(deceased, 'deceased.rds')

# province
levels(set1$province)
unique(set1$province)
set1$duplicate <- set1$province
levels(set1$duplicate) <- c(1:53)
province <- set1 %>% spread(key = province, value = duplicate)
province <- province[,c(1, 47:98)]# Remove column that contained empty rows
saveRDS(province, 'province.rds')

# segment
levels(set1$segment)
unique(set1$segment)
set1$duplicate <- set1$segment
levels(set1$duplicate) <- c(1:4)
segment <- set1 %>% spread(key = segment, value = duplicate)
segment <- segment[,c(1, 47:49)]# Remove column that contained empty rows
saveRDS(segment, 'segment.rds')

# Bind spread dataframes together
y <- cbind(emp_code, country, gender, cust_type, cust_status, resident,
           foreigner, acq_channel, deceased, province, segment) 

names(y)

# Remove all cust_code columns
y <- y[, -c(7, 78, 81, 87, 92, 95, 98, 218, 221, 274)]
x <- y[2:267]

# Check current factor levels
sapply(x, levels)

# Convert all columns to 1 and 0
x[!is.na(x)] = '1'
x[] <- lapply(x[], function(x){
  levels(x)[levels(x) == '2'] <- '0'
  x
})
x[is.na(x)] = 0

# Drop all other factor levels
x <- droplevels(x)
sapply(x, levels)

# Recreate transformed data frame
y <- cbind(y$cust_code, x)
colnames(y)[1] <- 'cust_code'
saveRDS(y, 'y.rds')

# Create set2 data frame for model
set1 <- set1[, -c(2, 3, 4, 9:15, 18, 21)] # Saved RDS just in case
set2 <- cbind(set1, y)
set2[35] <- NULL # Remove second cust_code column
saveRDS(set2, 'set2.rds')
set2<- mutate_if(set2, is.factor, ~ as.numeric(as.character(.x)))
str(set2)

# Create test and train sets
set2_m7 <- set2 %>% select(everything()) %>% 
  filter(cust_code, month1 == 7)
saveRDS(set2_m7, 'set2_m7.rds')
m7 <- inner_join(set2_m7, setlabels, by = "cust_code") # Train set

set2_m8 <- set2 %>% select(everything()) %>% 
  filter(cust_code, month1 == 8)
saveRDS(set2_m8, 'set2_m8_rds')
m8 <- inner_join(set2_m8, setlabels, by = "cust_code") # Test set

# Create training set

# Run XGBoost model
library(xgboost)

#Creating the model using xgboost
fit.xgb <- xgboost(data = as.matrix(m7[,2:300]), label = as.matrix(m7[,301]),
              max_depth = 4, eta = 0.5, nrounds = 5, objective = "binary:logistic", verbose = 2)

# View classification tree
library(DiagrammeR)
xgb.plot.tree(model = fit.xgb)

# Predict class on test data
pred <- predict(fit.xgb, as.matrix(m8[, -c(1, 301)]))
set3 <- as.data.frame(cbind(round(pred, 2), m8[,301]))

# Find optimal cutoff points
library(InformationValue)
optCutOff <- optimalCutoff(m8$new_card, pred)[1]
misClassError(m8$new_card, pred, threshold = optCutOff)

# View important variables in model
print(xgb.importance(model = fit.xgb))
print(round(xgb.importance(model = fit.xgb)[,2:4],2))
imp <- xgb.importance(model = fit.xgb) 
print(cbind(imp[,1],round(imp[,2:4],2)))

