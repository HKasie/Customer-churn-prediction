datatech1 <- read.csv("ERS.csv")
datatech2 <- read.csv("ERS.csv", na.strings=c("NA", "NULL"))



# add a new factor (label) variable for the user_status (0 for cancelled, 1 for active) 
#USER_STATUS <- NA
datatech1$USER_STATUS <- ifelse(is.na(datatech1$USERS_2016), '1', '0')
datatech1$USER_STATUS <- factor(datatech1$USER_STATUS)

sum(is.na (datatech1)) # the sum of all null values

datatech1[is.na(datatech1)] <- 0  # replace the na values with 0

datatech_sub<-(datatech1[c(3:19)]) # subsetting variables

summary(datatech1)   # summary statistics

# Check data for completeness or Null values

library(visdat)
vis_dat(datatech1)
vis_miss(datatech1)


# Visual analytics 

boxplot(datatech_sub, main= "Boxplot of selected variables for churn analysis")

boxplot(datatech_sub$MEDICAL_REVENUE_2016, datatech_sub$SCIENTIFIC_REVENUE_2016, datatech_sub$TECHNICAL_REVENUE_2016, 
        datatech_sub$MEDICAL_REVENUE_2015, datatech_sub$SCIENTIFIC_REVENUE_2015, datatech_sub$TECHNICAL_REVENUE_2015,
        main= "Boxplot of selected variables for churn analysis")


hist(datatech1$MEDICAL_NO_SESSIONS_2015, main = "Histogram of User status")


# scatterplot of price vs. mileage
plot(x = USER_STATUS, y = MEDICAL_REVENUE_2016,
     main = "Scatterplot of Ratio Of churn status vs. medical revenue",
     xlab = "users_2016",
     ylab = "medical revenue_2016")

plot(datatech_sub$USERS_2016,
     main = "Scatterplot of medical revenue 2016")

library(ggplot2)
ggplot(datatech1, aes(x=MEDICAL_NO_SESSIONS_2016,
                      y=NPS_SCORE_2016,
                      color=USERS_2016)) +
  geom_point()+
  geom_line()+
  expand_limits(y=0)

ggplot(datatech1, aes(x=NPS_SCORE_2016,
                      y=MEDICAL_NO_SESSIONS_2016,
                      color=USER_STATUS)) +
  geom_point()

# Create plots

plot(density(datatech1$SCIENTIFIC_REVENUE_2015, main = "Density Plot of 2016 scientific revenue"))

plot(density(datatech1$USER_STATUS, main = "Density Plot of User Status"))



# Correlations among numeric variables in
# # cor(data, use="complete.obs", method="spearman")
str(datatech_sub)
cor.matrix<-cor(datatech_sub, use="complete.obs", method="spearman")
cor.df <-as.data.frame(cor.matrix)

# look at the correlations graphically
library(psych)

pairs.panels(datatech_sub, method="spearman", ellipses=FALSE)




sum(datatech1$USER_STATUS == 'Active')/length(datatech1$USER_STATUS)# 81% active users

# For categories in User status changed to numeric (0 and 1)

#datatech2 <- datatech1$USER_STATUS <- ifelse(is.na(datatech1$USERS_2016), 1, 0)

head(datatech1)

# All Users (both cancelled active) that registered before 2016
allUsersb416 <- subset(datatech1, (datatech1$FIRST_YR_CUSTOMER < 2016))

head(allUsersb416)
sum(allUsersb416$USER_STATUS == 'Active')/length(allUsersb416$USER_STATUS)

# Only active users b4 2016
Actb42016 <- subset (allUsersb416, (allUsersb416$USER_STATUS == 'Active'))

head(Actb42016)

# Only inactive (cancelled) users b4 2016

InActb42016 <- subset (allUsersb416, (allUsersb416$USER_STATUS == 'Cancelled'))

head(InActb42016)







library(sqldf)

sqldf("SELECT User_Status, First_Yr_Customer, Count(1) as N FROM allUsersb416
       GROUP BY First_Yr_Customer")

sqldf("SELECT User_Status, First_Yr_Customer, Count(1) as N FROM allUsersb416
       GROUP BY User_Status")

# Counting by Only active users b4 2016

sqldf("SELECT User_Status, First_Yr_Customer, Count(1) as N FROM Actb42016
       GROUP BY First_Yr_Customer")

# Counting by Only inactive (cancelled) users b4 2016

sqldf("SELECT User_Status, First_Yr_Customer, Count(1) as N FROM InActb42016
       GROUP BY First_Yr_Customer")








# Use glimpse from dplyr package to view data struecture
library(readr)
library(dplyr)

glimpse(InActb42016)
glimpse(datatech1)

# Count Active and Inactive customers - check it out

datatech1 %>% 
  count(USER_STATUS)

# Calculate average churn rate 

datatech1 %>% 
  
  summarize(churn_rate = mean(USER_STATUS))

#Calculate churn rate at each first_Yr_customer was registered

CusYr_reg <- datatech1 %>%  
  group_by(FIRST_YR_CUSTOMER) %>% 
  summarize(churn_CusYrReg = mean(USER_STATUS))

CusYr_reg


# Visualize the results
library(ggplot2)

ggplot(CusYr_reg, aes(x = FIRST_YR_CUSTOMER, y = churn_CusYrReg)) + 
  geom_col()









#  This is an example for previewing data, joining data, and visualizing the distribution with box plot

# View the structure of survey dataset ..please adapt it.
glimpse(survey)

# Complete the code to join survey to org3 dataset
org_final <- left_join(org3, survey, by = "mgr_id")

# Compare manager effectiveness scores
ggplot(datatech1, aes(x = status, y = mgr_effectiveness)) +
  geom_boxplot()




# Example of how to calculate Information value..please adapt it

emp_tenure <- emp_jhi %>%
  mutate(tenure = ifelse(status == "Active", 
                         time_length(interval(date_of_joining, cutoff_date), 
                                     "years"), 
                         time_length(interval(date_of_joining, last_working_date), 
                                     "years")))

# Compare tenure of active and inactive employees
ggplot(emp_tenure, aes(x = status, y = tenure)) + 
  geom_boxplot()


# Logistic regression model

# Load caret
library(caret)
# Set seed
set.seed(567)
# Subset dataset for modelling
model_data<-(datatech1[c(2:19)]) 

# Store row numbers for training dataset
index_train <- createDataPartition(model_data$USER_STATUS, p = 0.7, list = FALSE)
# Create training dataset
train_set <- model_data[index_train, ]
# Create testing dataset
test_set <- model_data[-index_train, ]

multi_log <- glm(USER_STATUS ~ ., family = "binomial",
                 data = train_set)

summary(multi_log)


# Multicollinearity

# Load car package
library(car)

# Calculate VIF
vif(multi_log)


# Remove variables with high variance inflation factors (VIF)
model_data1<-(model_data[c(1,8:13,16,17,18)])

# Store row numbers for training dataset
index_train1 <- createDataPartition(model_data1$USER_STATUS, p = 0.7, list = FALSE)
# Create training dataset
train_set1 <- model_data1[index_train1, ]
# Create testing dataset
test_set1 <- model_data1[-index_train1, ]

#Repeat the logistic regression with the subset of variables

multi_log1 <- glm(USER_STATUS ~ ., family = "binomial",
                  data = train_set1)

summary(multi_log1)

# Make predictions for testing dataset
prediction_test <- predict(multi_log1, newdata = test_set1,
                           type = "response")

#plot probability range of test dataset
# Look at the predictions range
hist(prediction_test)

# Classify predictions using a cut-off of 0.5
pred_cutoff_50_test <- ifelse(prediction_test > 0.5, 1, 0)

## Creating confusion matrix
table(pred_cutoff_50_test, test_set1$USER_STATUS)

#Confusion matrix calculation using Caret package
library(caret)
# Construct a confusion matrix
conf_matrix_50 <- confusionMatrix(table(test_set1$USER_STATUS,
                                        pred_cutoff_50_test))
conf_matrix_50



# Churn retention strategy and calculating churn risk probability for the customers

# To know customers who could churn

# Calculate churn risk probality
# Load tidypredict 
library(tidypredict)

# Calculate probability of turnover ...use the original dataset, the dependent variable and the model 
cus_risk <- datatech1  %>%  
  filter(USER_STATUS == 0) %>%
  tidypredict_to_column(multi_log1)

# Run the code with Customer account Id variable to see the perhaps top 5 customers that are likely to churn
cus_risk %>% 
  select(ACCT_ID, fit) %>% 
  top_n(5, wt = fit)



# Create turnover risk buckets
emp_risk_bucket <- cus_risk %>% 
  mutate(risk_bucket = cut(fit, breaks = c(0, 0.5, 0.6, 0.8, 1), 
                           labels = c("no-risk", "low-risk", 
                                      "medium-risk", "high-risk")))

# Count employees in each risk bucket
emp_risk_bucket %>%  
  count(risk_bucket)


# Create churn model with Decision tree algorithm
library(party)
tree <- ctree(USER_STATUS~NPS_SCORE_2015+NPS_SCORE_2016+MEDICAL_NO_SESSIONS_2015,
              train_set1)
plot(tree)

# Confusion matrix
pred_tree <- predict(tree, test_set1)
print("Confusion Matrix for Decision Tree"); table(Predicted =
                                                     pred_tree, Actual = test_set1$USER_STATUS)
# Accuracy

p1 <- predict(tree, train_set1)
tab1 <- table(Predicted = p1, Actual = train_set1$USER_STATUS)
tab2 <- table(Predicted = pred_tree, Actual = test_set1$USER_STATUS)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
