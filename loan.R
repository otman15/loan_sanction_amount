# Introduction ####

## Install all needed libraries if it is not present ####
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(corrplot)) install.packages("corrplot")
if(!require(xgboost)) install.packages("Xgboost")

## load libraries ####
library(tidyverse)
library(data.table)
library(caret)
library(xgboost)
library(corrplot)

## load data ####
unzip("loan_amount.zip")

loan_data <- fread("train.csv", na.strings = "")
file.remove("test.csv", "sample_submission.csv")
# initial exploration ####

## first look ####
head(loan_data)

## dimension
dim(loan_data)

## structure
str(loan_data)

## the features are
names(loan_data)

loan_data <- loan_data %>% select(-Name)
## looking for nas ####
nas <- sapply(loan_data, function(i){
  sum(is.na(i))
}) 

nas %>% as.data.frame() # there are 11 features with nas

## nas percentage ####
loan_data %>% select(names(which(nas != 0))) %>% 
  sapply(function(i){
  mean(is.na(i))
}) %>% as.data.frame() 

rm(nas)

# Since there are a lot of features with nas first we will deal with nas

## deal with nas ####

### loan sanction amount : the outcome ####
hist(loan_data$`Loan Sanction Amount (USD)`)

summary(loan_data$`Loan Sanction Amount (USD)`)

sum(is.na(loan_data$`Loan Sanction Amount (USD)`))

sum(loan_data$`Loan Sanction Amount (USD)` < 0, na.rm = TRUE) # must be an error

   # remove the rows for which the outcome is na
loan_data <- loan_data %>% filter(!is.na(`Loan Sanction Amount (USD)`))

   # loan amount < 0 to 0 , since the loan must be >= 0
loan_data$`Loan Sanction Amount (USD)`[which(
  loan_data$`Loan Sanction Amount (USD)` < 0)] <- 0

### gender ####
table(loan_data$Gender) # there is as much "Females" as "males" 
sum(is.na(loan_data$Gender)) # the number of nas is small 

  # replace the 52 nas with "F" or "M" 
loan_data$Gender[which(
  is.na(loan_data$Gender))] <- rep(c("F", "M"), times = 26)

### income stability ####
table(loan_data$`Income Stability`)

sum(is.na(loan_data$`Income Stability`))

loan_data$`Income Stability`[which(
  is.na(loan_data$`Income Stability`))] <- "Low"

### type of employment ####
table(loan_data$`Type of Employment`) %>% sort(decreasing = TRUE) %>%
  as.data.frame()

sum(is.na(loan_data$`Type of Employment`))

loan_data$`Type of Employment`[which(
  is.na(loan_data$`Type of Employment`))] <- "other"

### dependent ####
table(loan_data$Dependents)

sum(is.na(loan_data$Dependents))

  #replace nas
loan_data$Dependents[which(
  is.na(loan_data$Dependents))] <- 2

  # replace classes that are almost insignificant in number
loan_data$Dependents[loan_data$Dependents %in% 5:14] <- 2

### Has active card ####
table(loan_data$`Has Active Credit Card`) # almost same number for each class

sum(is.na(loan_data$`Has Active Credit Card`))

  # replace 1546 nas with same number of classes 
loan_data$`Has Active Credit Card`[
  which(is.na(loan_data$`Has Active Credit Card`))] <- c(
           rep(c("Active", "Inactive","Unpossessed"), times = 515), "Active") 

### property location ####
table(loan_data$`Property Location`) # almost same number of classes

sum(is.na(loan_data$`Property Location`))

loan_data$`Property Location`[which(
  is.na(loan_data$`Property Location`))] <- c(
                                     rep(c("Rural", "Semi-Urban","Urban"),
                                         times = 115), 
                                     "Semi-Urban", "Semi-Urban")

### income ####
summary(loan_data$`Income (USD)`) # 4493 nas


loan_data  %>% ggplot(aes(`Income (USD)`)) + 
  geom_histogram(color = "black") + xlim(c(0, 10000)) 


  # there is outliers so we replace nas with the mediane
loan_data$`Income (USD)`[which(
  is.na(loan_data$`Income (USD)`))] <- median(loan_data$`Income (USD)`,
                                            na.rm = TRUE)

### current loan expenses ####
summary(loan_data$`Current Loan Expenses (USD)`) # 167 nas

loan_data %>% ggplot(aes(`Current Loan Expenses (USD)`)) +
  geom_histogram(color = "black")

sum(loan_data$`Current Loan Expenses (USD)` < 0, na.rm = TRUE) # must be error

  #replace nas 
loan_data$`Current Loan Expenses (USD)`[which(
  is.na(loan_data$`Current Loan Expenses (USD)`))] <- median(
    loan_data$`Current Loan Expenses (USD)`, na.rm = TRUE)

  # replace loan expenses < 0
loan_data$`Current Loan Expenses (USD)`[which(
  loan_data$`Current Loan Expenses (USD)` == -999)] <- median(
    loan_data$`Current Loan Expenses (USD)`, na.rm = TRUE)


## Credit score :
summary(loan_data$`Credit Score`) # 1670 na's

loan_data %>% ggplot(aes(`Credit Score`)) +
  geom_histogram(color = "black") + theme_bw()

  # most values are between 680 and 800 with small variation 
loan_data$`Credit Score`[which(
  is.na(loan_data$`Credit Score`))] <- seq(680, 800, length = 1670)


### Property age ####
summary(loan_data$`Property Age`) # 4760 nas 

  # we notice the existance of outliers that must be errors
loan_data %>% ggplot(aes(`Property Age`)) +
  geom_histogram(color = "black") + theme_bw() + 
  scale_x_continuous(trans = "log10")

  # replace nas with median due to outliers
loan_data$`Property Age`[which(
  is.na(loan_data$`Property Age`))] <- median(loan_data$`Property Age`,
                                              na.rm = TRUE)



## now we can see how many distinct value in character columns ####
loan_data %>% select(-c(`Customer ID`)) %>% select_if(is.character)  %>%
  sapply(function(i){
    n <- which(class(i) == "character")
    length(unique(i))
  }) %>% as.data.frame()


# Exploratory analysis ####


## Exploring continuous features first ####
 ### heatmap of continuous features ####

M <- loan_data %>% select_if(is.numeric) %>% cor()
heatmap(M, scale = "column")
rm(M)

 ### customer ID : all the customers are unique
length(unique(loan_data$`Customer ID`))

 ### age ####

  #### age distribution ####
loan_data %>% ggplot(aes(Age)) + 
  geom_histogram(color = "black") +
  ggtitle("Age distribution")

  #### Boxplot of loan sanction amount vs age ####
loan_data %>% mutate( age = as.factor(Age)) %>% group_by(age) %>%
  ggplot(aes(x = age, y = `Loan Sanction Amount (USD)`, fill = age)) +
  geom_boxplot() + ylab("loan amount") +
  theme_bw() + ylim(c(0, 2*10^5)) +
  theme(axis.text.x = element_text(
                          angle = 90, hjust = 1),legend.position = "none")

### income ####
 
  #### Income distribution ####
loan_data %>% ggplot(aes(`Income (USD)`)) + 
  geom_histogram( color = "black") +
  scale_x_continuous(trans = "log10", limits = c(300, 10^5)) +
  ggtitle("Income distribution") + theme_bw()

  #### loan sanction amount vs income ####
loan_data  %>%
  ggplot(aes(`Income (USD)`, `Loan Sanction Amount (USD)`)) +
  geom_point() + scale_x_continuous(limits = c(300, 10^4)) +
  ylab("loan Sanction") + 
  theme_bw() + ggtitle("loan amount vs income")


### Loan amount request ####

  #### Distribution of loan request ####  
loan_data %>% ggplot(aes(`Loan Amount Request (USD)`)) +
  geom_histogram(color = "black") +
  theme_bw() + ggtitle("Loan amount request dist.")

  #### Loan  sanction vs loan request  amounts ####
loan_data %>% ggplot(
  aes(`Loan Amount Request (USD)`, `Loan Sanction Amount (USD)`)) +
  geom_point() + ylab("loan_sanction") +
  xlab("loan_request") + 
  theme_bw() + ggtitle("Loan sanction vs loan request")

### Current loan expenses ####

  #### Distribution of current loan expenses ####
loan_data %>% ggplot(aes(`Current Loan Expenses (USD)`)) + 
  geom_histogram(color = "black") +
  theme_bw() + ggtitle("Current loan expenses distribution")

  #### Loan sanction vs current loan expanses ####
loan_data %>% ggplot(
  aes(`Current Loan Expenses (USD)`, `Loan Sanction Amount (USD)`)) +
  geom_point() +
  xlab("current loan exp") + ylab("loan sanction") +
  theme_bw() + ggtitle("Loan sanction vs current loan expenses")


### credit score ####

  #### Distribution  of credit score ####
loan_data %>% ggplot(aes(`Credit Score`)) +
  geom_histogram(color = "black") + theme_bw() +
  ggtitle("credit score distribution")

  #### Loan sanction vs credit score ####
loan_data %>% ggplot(
  aes(`Credit Score`, `Loan Sanction Amount (USD)`)) +
  geom_point() + theme_bw() + ylab("loan sanction") +
  theme_bw() + ggtitle("Loan sanction vs credit score")

### Property age ####

  #### Distribution of proprety age ####
loan_data %>% ggplot(aes(`Property Age`)) +
  geom_histogram(color = "black") + 
  xlim(c(0, 12000)) + theme_bw() +
  ggtitle("Proprety age distribution")

  #### Loan sanction vs property age ####
loan_data %>% ggplot(aes(`Property Age`, `Loan Sanction Amount (USD)`)) +
  geom_point() + ylab("loan sanction")  + xlim(c(0, 12000)) +
  theme_bw() + ggtitle("Loan sanction vs proprety age")

### property price ####

  #### Property price distribution ####
loan_data %>% ggplot(aes(`Property Price`)) +
  geom_histogram(color = "black") +
  xlim(c(0, 6*10^5)) +
  theme_bw() + ggtitle("Proprety price distribution")

  #### Loan sanction amount vs property price ####
loan_data %>% ggplot(aes(`Property Price`, `Loan Sanction Amount (USD)`)) +
  geom_point() +  ylab("loan sanction") +
  theme_bw() + ggtitle("Loan sanction vs proprety price")


### loan sanction amount ####

summary(loan_data$`Loan Sanction Amount (USD)`)

  #### Distribution of loan sanction amount ####
loan_data %>% ggplot(aes(`Loan Sanction Amount (USD)`)) +
  geom_histogram(color = "black") + 
  theme_bw() +
  ggtitle("loan sanction amount distribution")

### create the percentage of approved on requested loan variable ####

loan_data <- loan_data %>% 
  mutate(approved_percentage = 
           `Loan Sanction Amount (USD)` / `Loan Amount Request (USD)`)

summary(loan_data$approved_percentage)

  #### Distribution of percentage approved ####
loan_data %>% ggplot(aes(approved_percentage)) +
  geom_histogram(color = "black") + 
  theme_bw() +
  ggtitle("approved_percentage distribution")

### Correlation between continuous variables #### 

continuous_features <- loan_data %>% 
  select(`Income (USD)`,
         `Loan Sanction Amount (USD)`, `Loan Amount Request (USD)`,
         Age, `Current Loan Expenses (USD)`, `Credit Score`, 
         `Property Age`, `Property Price`, approved_percentage) %>%
          names()

loan_data %>% select(all_of(continuous_features))%>% cor() %>%
corrplot(method = "number", type = "upper",  cl.cex = 0.7,
         tl.cex = 0.8, tl.col = "blue")

# the correlation between property age and income is 1, which is weird for me,
# if we look closer we find they have the sames values, the income must have
# been taken wrongly for the property age, so let drop the property age.
loan_data <- loan_data %>% select(-`Property Age`)
continuous_features <- continuous_features[
                                    continuous_features != "Property Age"]


### Dependents ####

  #### Dependents barplot ####
loan_data %>% mutate(Dependents = as.factor(Dependents)) %>%
  ggplot(aes(Dependents, fill = Dependents)) + 
  geom_bar() +
  xlab("") +theme_bw() + 
  ggtitle("Dependents barplot")

  #### Boxplot of loan sanction vs dependents ####
loan_data %>% mutate(Dependents = as.factor(Dependents)) %>%
  ggplot(aes(Dependents, `Loan Sanction Amount (USD)`, fill = Dependents)) +
  geom_boxplot() + ylim(c(0, 3*10^5)) + theme_bw() +
  theme(legend.position = "none") +
  ylab("loan sanction") + 
  ggtitle("Boxplot of loan sanction vs dependents")

### number of defaults ####

  #### Number of defaults barplot ####
loan_data %>% mutate(`No. of Defaults` = as.factor(`No. of Defaults`)) %>%
  ggplot(aes(`No. of Defaults`, fill = `No. of Defaults`)) + 
  geom_bar() +theme_bw() + xlab("") +
  ggtitle("Number of defaults barplot") +
  theme(axis.text.x = element_blank())

  #### boxplot of loan sanction vs number of defaults ####
loan_data %>% mutate(`No. of Defaults` = as.factor(`No. of Defaults`)) %>%
  ggplot(aes(`No. of Defaults`,
            `Loan Sanction Amount (USD)`, fill =`No. of Defaults`)) +
  geom_boxplot() + scale_y_continuous(trans = "log10") +
  theme_bw() + ylab("loan sanction") + 
  ggtitle("Boxplot of loan sanction vs number of defaults") +
  theme(legend.position = "none")

### has credit card ####

  #### Has credit card barplot ####
loan_data %>% ggplot(
  aes(`Has Active Credit Card`, fill = `Has Active Credit Card`)) +
  geom_bar() + theme_bw() + xlab("") +
  ggtitle("Barplot of has credit card") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs has credit card ####
loan_data %>% ggplot(
  aes(`Has Active Credit Card`, `Loan Sanction Amount (USD)`,
      fill = `Has Active Credit Card`)) +
  geom_boxplot() + theme_bw() +
  ylab("loan sanction") + xlab("") + 
  ggtitle("Boxplot of loan sanction vs has credit card") + 
  theme(axis.text.x = element_blank())

### Property type #### 

  #### Proprety type barplot ####
loan_data %>% mutate(`Property Type` = as.factor(`Property Type`)) %>%
  ggplot(aes(`Property Type`, fill = `Property Type`)) +
  geom_bar() + theme_bw() + xlab("") +
  ggtitle("Barplot Property type") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction amount vs property type ####
loan_data %>% mutate(`Property Type` = as.factor(`Property Type`)) %>%
  ggplot(aes(`Property Type`, `Loan Sanction Amount (USD)`,
             fill = `Property Type`)) + geom_boxplot() +
  theme_bw() + ylab("loan sanction") + xlab("") +
  theme(axis.text.x = element_blank())
  

### Property location ####

  #### Barplot of property location ####
loan_data %>% ggplot(aes(`Property Location`, fill = `Property Location`)) +
  geom_bar(color = "black") + theme_bw() + xlab("") +
  ggtitle("Barplot Property location") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs property location ####
loan_data %>% 
  ggplot(aes(`Property Location`,
             `Loan Sanction Amount (USD)`, fill = `Property Location`)) +
  geom_boxplot() + 
  theme_bw() + ylab("loan sanction") + xlab("") +
  ggtitle("Boxplot of loan sanction vs Property location") + 
  theme(axis.text.x = element_blank())

### coapplicant ####

table(loan_data$`Co-Applicant`)

  #### the co_applicant must be >= 0
loan_data$`Co-Applicant`[
  which(loan_data$`Co-Applicant` < 0)] <- 1

  #### Co_applicant barplot ####
loan_data %>% mutate(`Co-Applicant` = as.factor(`Co-Applicant`)) %>%
  ggplot(aes(`Co-Applicant`, fill = `Co-Applicant`)) +
  geom_bar(color = "black") + theme_bw() + xlab("") +
  ggtitle("Co-applicant barplot") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs co-applicant ####
loan_data %>% mutate(`Co-Applicant` = as.factor(`Co-Applicant`)) %>%
  ggplot(aes(`Co-Applicant`,
             `Loan Sanction Amount (USD)`, fill =`Co-Applicant`)) +
  geom_boxplot() + theme_bw() +
  ylab("loan sanction amount") + xlab("") +
  ggtitle("Boxplot of loan sanction vs co-applicant") + 
  theme(axis.text.x = element_blank())


### Gender ####

  #### Gender barplot ####
loan_data %>% ggplot(aes(Gender, fill = Gender)) +
  geom_bar() + xlab("") + 
  ggtitle("Gender barplot") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs gender ####
loan_data %>% ggplot(aes(x = Gender, y = `Loan Sanction Amount (USD)`, 
                         fill = Gender)) +
  geom_boxplot() + xlab("") + ylab("loan amount") + 
  ggtitle("Boxplot of loan sanction vs gender") + 
  theme(axis.text.x = element_blank())

### income stability ####

  #### Barplot of income stability #### 
loan_data %>% ggplot(aes(`Income Stability`, fill = `Income Stability`)) +
  geom_bar() + xlab("") + 
  ggtitle("Income stability barplot") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs income stability #### 
loan_data %>% 
  ggplot(aes(`Income Stability`,
             `Loan Sanction Amount (USD)`, fill = `Income Stability`)) +
  geom_boxplot() + 
  xlab("") + ylab("loan amount") + 
  ggtitle("Boxplot of loan sanction vs income stability") + 
  theme(axis.text.x = element_blank())

### profession ####

loan_data$Profession %>% table()

  #### deal with singular classes ####
loan_data$Profession[loan_data$Profession %in% c(
  "Maternity leave", "Student",
  "Businessman","Unemployed")] <- "other"

  #### Barplot of profession ####
loan_data %>% ggplot(aes(Profession, fill = Profession)) + 
  geom_bar() +xlab("") + theme_bw() +
  ggtitle("Profession barplot") + 
  theme(axis.text.x = element_blank())
  
  #### Boxplot of loan sanction vs profession #### 
loan_data %>% 
  ggplot(aes(Profession,
             `Loan Sanction Amount (USD)`, fill = Profession)) +
  geom_boxplot() + 
  xlab("") + ylab("loan amount") + 
  ggtitle("Boxplot of loan sanction vs Profession") + 
  theme(axis.text.x = element_blank())

### Type of employment ####

  #### Barplot of type of employment ####
loan_data %>% ggplot(aes(`Type of Employment` , fill = `Type of Employment`)) + 
  geom_bar() +xlab("") + theme_bw() +
  ggtitle("Type of Employment barplot") +  theme_bw() +
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs type of employment #### 
loan_data %>% 
  ggplot(aes(`Type of Employment`,
             `Loan Sanction Amount (USD)`, fill = `Type of Employment`)) +
  geom_boxplot() + 
  xlab("") + ylab("loan amount") + 
  ggtitle("Boxplot of loan sanction vs type of employment") + 
  theme(axis.text.x = element_blank())

### Location #####

  #### Barplot of location ####
loan_data %>% ggplot(aes(Location, fill = Location)) + 
  geom_bar() +xlab("") + theme_bw() +
  ggtitle("location barplot") + 
  theme(axis.text.x = element_blank())

  #### Boxplot of loan sanction vs Location #### 
loan_data %>% 
  ggplot(aes(Location,`Loan Sanction Amount (USD)`, fill = Location)) +
  geom_boxplot() + 
  xlab("") + ylab("loan amount") + 
  ggtitle("Boxplot of loan sanction vs Location") + 
  theme(axis.text.x = element_blank())

### Expenses type ####

 #### Barplot of Expenses type ####
loan_data %>% ggplot(aes(`Expense Type 1`, fill = `Expense Type 1`)) + 
  geom_bar() +xlab("") + theme_bw() +
  ggtitle("Expense Type 1 barplot") + 
  theme(axis.text.x = element_blank())

loan_data %>% ggplot(aes(`Expense Type 2`, fill = `Expense Type 2`)) + 
  geom_bar() +xlab("") + theme_bw() +
  ggtitle("Expense Type 2 barplot") + 
  theme(axis.text.x = element_blank())

loan_data %>% select(`Expense Type 1`, `Expense Type 2`) %>% 
  gather(key = "type", value = "value") %>% 
  ggplot(aes(value, fill = type)) + geom_bar() + theme_bw() +
  ggtitle("Expense Type barplot") + xlab("") +
  theme(axis.text.x = element_blank())
  
  #### Boxplot of loan sanction vs Expenses type #### 
  loan_data %>% 
    ggplot(aes(`Expense Type 1`,`Loan Sanction Amount (USD)`,
               fill = `Expense Type 1`)) +
    geom_boxplot() + 
    xlab("") + ylab("loan amount") + 
    ggtitle("Boxplot of loan sanction vs Expense Type 1") + 
    theme(axis.text.x = element_blank())
  
  loan_data %>% 
    ggplot(aes(`Expense Type 2`,`Loan Sanction Amount (USD)`,
               fill = `Expense Type 2`)) +
    geom_boxplot() + 
    xlab("") + ylab("loan amount") + 
    ggtitle("Boxplot of loan sanction vs Expense Type 2") + 
    theme(axis.text.x = element_blank())


# Data processing ####

## drop features we don't need on models building ####

loan_data <- loan_data %>% select(-`Property ID`)

## deal with categorical features ####


 ### binarize categorical data ####
  cat_var <- loan_data %>% select(-all_of(continuous_features),
                                  `Customer ID`) %>% names()
  cat_var
  #### Profession
loan_data <- loan_data %>%
  mutate(value = as.integer(Profession %in% Profession)) %>%
  spread(key = Profession, value = value, fill = 0)

 #### Location
loan_data <- loan_data %>%
  mutate(value = as.integer(Location %in% Location)) %>%
  spread(key = Location, value = value, fill = 0)

 #### Property location
loan_data <- loan_data %>%
  mutate(value = as.integer(
    `Property Location` %in% `Property Location`)) %>%
  spread(key = `Property Location`, value = value, fill = 0)

 #### Type of employment
loan_data <- loan_data %>%
  mutate(value = as.integer(
    `Type of Employment` %in% `Type of Employment`)) %>%
  spread(key = `Type of Employment`, value = value, fill = 0)

 #### Property type
loan_data <- loan_data %>% 
  mutate(`Property Type` = paste("prop type", `Property Type`)) %>%
  mutate(value = as.integer(
    `Property Type` %in% `Property Type`)) %>%
  spread(key = `Property Type`, value = value, fill = 0)

 #### Dependents
loan_data <- loan_data %>% 
  mutate(Dependents = paste("dependents", Dependents)) %>%
  mutate(value = as.integer(
    Dependents %in% Dependents)) %>%
  spread(key = Dependents, value = value, fill = 0)

 #### Has credit card
loan_data <- loan_data %>%
  mutate(value = as.integer(
    `Has Active Credit Card` %in% `Has Active Credit Card`)) %>%
  spread(key = `Has Active Credit Card`, value = value, fill = 0)

 #### Gender
loan_data$Gender <- ifelse(loan_data$Gender == "F", 1, 0)

 #### Income stability
loan_data$`Income Stability` <- ifelse(loan_data$`Income Stability` == "High",
                                       1, 0)

 #### Expenses type 1
loan_data$`Expense Type 1` <- ifelse(loan_data$`Expense Type 1` == "Y", 1, 0)

 #### Expenses type2
loan_data$`Expense Type 2` <- ifelse(loan_data$`Expense Type 2` == "Y", 1, 0)

# the data now looks like :
head(loan_data)

### scale our continuous data exept the outcome

 #### variables to be scaled

cont_var <- continuous_features[!continuous_features %in% 
                              c("Loan Sanction Amount (USD)")]
rm(continuous_features)
 
#### scale vars 

loan_data <- loan_data %>% mutate_at(cont_var, scale) %>% as.data.frame()


head(loan_data)

## Split data into train and test set ####

y <- loan_data$`Loan Sanction Amount (USD)`

set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

test_set <- loan_data %>% dplyr::slice(test_index)
train_set <- loan_data %>% dplyr::slice(-test_index)

train_y <- train_set$`Loan Sanction Amount (USD)`

train_x <- train_set %>% 
  select(-c(`Customer ID`, `Loan Sanction Amount (USD)`))

test_x <- test_set %>% 
  select(-c(`Customer ID`, `Loan Sanction Amount (USD)`))

test_y <- test_set$`Loan Sanction Amount (USD)`

# Building models ####

##metric used : we well the RMSE function of the caret package
RMSE

## linear regression ####
# with continuous variables only !

### train the model 
set.seed(1, sample.kind = "Rounding")

trControl  <- trainControl(method="repeatedcv",
                                number = 4,
                                repeats = 5) 

train_lm <- train(train_x[cont_var], train_y, method = "lm",
                  trControl = trControl)

### Get the predictions

pred_lm <- predict(train_lm, newdata = test_x[cont_var])

### get results 

rmse_lm <- RMSE(pred = pred_lm, test_y)
rmse_lm

plot(pred_lm, test_y)

results <- data.frame(method = "lm", rmse = rmse_lm)


# we see that the prediction for value different then zero are good otherwise
# for the ones near zero they are not
# it would be better to fit a model to predict null targets first if we 
#insisted to perform a linear model

## Rpart model ####

### train model
set.seed(1, sample.kind = "Rounding")

trControl  <- trainControl(method="repeatedcv",
                           number = 4,
                           repeats = 5)

train_rpart<- train(train_x, train_y, method = "rpart", trControl = trControl)

### prediction 

pred_rpart <- predict(train_rpart, newdata = test_x)

### get results 

rmse_rpart <- RMSE(pred = pred_rpart, test_y)
rmse_rpart

plot(pred_rpart, test_y)

results <- results %>% add_row(method = "rpart", rmse = rmse_rpart)
results

## Random forest ####

set.seed(1, sample.kind = "Rounding")

 ### train the model
tuneGrid <- expand.grid(.mtry = 1: 10)
control <- trainControl(method="cv", number = 10, p = 0.8)

train_rf <- train(train_x, train_y,
                 method = "rf",
                 tuneGrid = tuneGrid,
                 trControl = control,
                 nodesize = 10,
                 ntree = 10)
 ### prediction 

pred_rf <- predict(train_rf, newdata = test_x)

### get results 

rmse_rf <- RMSE(pred = pred_rf, test_y)
rmse_rf

plot(pred_rf, test_y)

results <- results %>% add_row(method = "rf", rmse = rmse_rf)
results


## Xgboost model ####

set.seed(1, sample.kind = "Rounding")

### define final training and testing sets
dtrain <- xgb.DMatrix(as.matrix(train_x), 
                      label = train_y)

dtest <- xgb.DMatrix(as.matrix(test_x), 
                     label = test_y)

### define watchlist
watchlist <- list(train = dtrain, eval = dtest)

### define params
params <- list(max_depth = 6, 
               objective = "reg:squarederror",
               silent = 0)

 
### use cross validation to find the best tunes

cv_model <- xgb.cv(params = params,
                   data = dtrain, 
                   nrounds = 150, 
                   watchlist = watchlist,
                   nfold = 5,
                   prediction = TRUE) # prediction of cv folds

### best nround
cv_model$evaluation_log %>%
  filter(test_rmse_mean == min(test_rmse_mean))



### define final model : 
final = xgboost(data = dtrain, max.depth = 5, nrounds = 150, verbose = FALSE)


### Get predictions 
pred_xgboost <- predict(final, data.matrix(test_x))

### Get results 

rmse_xgboost <- RMSE(pred = pred_xgboost, test_y)
rmse_xgboost

plot(pred_xgboost, test_y)

results <- results %>% add_row(method = "xgboost", rmse = rmse_xgboost)
results

