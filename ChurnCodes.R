##Make sure all packages are ready to use

library(reshape2)
library(dplyr)
library(moments)
library(corrplot)
library(pROC)
library(ISLR)
library(caret)

install.packages('C50')
library(C50)
data(churn)
remove(churnTest)

#Set levels so "yes" is second
churnTrain$churn=factor(churnTrain$churn,levels(churnTrain$churn)[c(2,1)])

##Confirm levels are set so "yes" is second
levels(churnTrain$churn)

##View summary of churnTrain data to see what data types are present
summary(churnTrain)
summary(churnTrain$number_vmail_messages)
summary(churnTrain$total_intl_calls)

#notice in the summary data that there are 0.00 values in most 
#of the charge fields but that all have some total night charges

#check for null values in data
colMeans(is.na(churnTrain))
colMeans(is.na(Customers_To_Predict))


#While logarithmic regression analysis does not require
#normality of independent variables, viewing histograms/
#skewness of data may provide some insight into fields
hist(churnTrain$account_length)
hist(churnTrain$number_vmail_messages)
hist(churnTrain$total_day_minutes)
hist(churnTrain$total_day_calls)
hist(churnTrain$total_day_charge)
hist(churnTrain$total_eve_minutes)
hist(churnTrain$total_eve_calls)
hist(churnTrain$total_eve_charge)
hist(churnTrain$total_night_minutes)
hist(churnTrain$total_night_calls)
hist(churnTrain$total_night_charge)
hist(churnTrain$total_intl_minutes)
hist(churnTrain$total_intl_calls)
hist(churnTrain$total_intl_charge)
hist(sqrt(churnTrain$total_intl_calls))



#Confirming that skew in total international calls can be reduced
skewness(churnTrain$total_intl_calls)
skewness(sqrt(churnTrain$total_intl_calls))
#Need to add 1 to calls to remove 0 values before trying log 
#log of total international calls results in smaller skew than square root
skewness(log(churnTrain$total_intl_calls+1))

#reducing skewness in voicemail messages
skewness(churnTrain$number_vmail_messages)
#Need to add 1 to calls to remove 0 values before trying log 
#log of number voicemail messages results in slightly smaller skew
skewness(log(churnTrain$number_vmail_messages+.000001))


##looking for visual correlations between variables
pairs(churnTrain[,6:19])

corrplot(cor(churnTrain[c(2,6:19)]), method = "number")

#Confirm correlation between minutes and charges is essentially 1
cor(churnTrain$total_day_minutes, churnTrain$total_day_charge)
cor(churnTrain$total_eve_minutes, churnTrain$total_eve_charge)
cor(churnTrain$total_night_minutes, churnTrain$total_night_charge)
cor(churnTrain$total_intl_minutes, churnTrain$total_intl_charge)

#Inclination may be that there is a correlation between
#number of international calls and international charges
#confirming there is no correlation between total number of 
#international calls and international charges:
cor(churnTrain$total_intl_charge, churnTrain$total_intl_calls)

#Run a model using minutes, then using charges to confirm coefficients and AUC is the same
Model = glm(churn~state+account_length+area_code+international_plan+voice_mail_plan
            +number_vmail_messages + total_day_minutes + total_day_calls +
              total_eve_minutes + total_eve_calls + total_night_minutes + total_night_calls
            + total_intl_minutes + total_intl_calls + number_customer_service_calls, family="binomial", data=churnTrain)
summary(Model)

Predicted_Values<-predict(Model, newdata=churnTrain, type='response')
roc(churnTrain$churn,Predicted_Values)

Model = glm(churn~state+account_length+area_code+international_plan+voice_mail_plan
            +number_vmail_messages + total_day_charge + total_day_calls +
              total_eve_charge + total_eve_calls + total_night_charge + total_night_calls
            + total_intl_charge + total_intl_calls + number_customer_service_calls, family="binomial", data=churnTrain)

summary(Model)
Predicted_Values<-predict(Model, newdata=churnTrain, type='response')
roc(churnTrain$churn,Predicted_Values)

#Create new dataframe to store manipulated data that may be useful in model
churnTrainClean <- churnTrain

churnTrainClean$total_intl_calls_nm = log(churnTrain$total_intl_calls+1)

##Create new variable of total charges
churnTrainClean$total_charge = churnTrainClean$total_day_charge + churnTrainClean$total_eve_charge + churnTrainClean$total_night_charge + churnTrainClean$total_intl_charge

##Create new variable of all domestic charges (do not include international in sum)
churnTrainClean$total_charge_no_intl = churnTrainClean$total_day_charge + churnTrainClean$total_eve_charge + churnTrainClean$total_night_charge
churnTrainClean$number_vmail_messages_nm = log(churnTrain$number_vmail_messages+.000001)

#Create an indicator of states with statistical signifance from model above
#Note that all of these states had a positive Z value so have the same
#kind of effect on likelihood to churn. Focusing only on those states that have
#significant impact simplifies model

churnTrainClean$state_ind <- ifelse(churnTrainClean$state == "CA" |
                                      churnTrainClean$state == "ME" |
                                      churnTrainClean$state == "MI" |
                                      churnTrainClean$state == "MS" |
                                      churnTrainClean$state == "MT" |
                                      churnTrainClean$state == "NJ" |
                                      churnTrainClean$state == "NV" |
                                      churnTrainClean$state == "SC" |
                                      churnTrainClean$state == "TX" |
                                      churnTrainClean$state == "WA", "Y","N")

#Total calls at different times of day were not statistically significant, 
#Created variable of totals calls for all times of day, just in case overall number
#of calls is significant
churnTrainClean$total_calls_no_intl = churnTrainClean$total_day_calls + churnTrainClean$total_eve_calls + churnTrainClean$total_night_calls
churnTrainClean$total_calls = churnTrainClean$total_day_calls + churnTrainClean$total_eve_calls + churnTrainClean$total_night_calls + churnTrainClean$total_intl_calls

##Look at summary data for any general trends
##between those who churn and those who don't
summarized_data_percent <- churnTrainClean %>%
  group_by(churn) %>%
  summarize(
    overall_churn_percent = (n()/nrow(churnTrainClean))*100,
    intl_plan_churn_percent = (sum(ifelse(international_plan == 'yes',1,0))/sum(ifelse(churnTrainClean$international_plan == 'yes',1,0)))*100,
    vmail_plan_churn_percent = (sum(ifelse(voice_mail_plan == 'yes',1,0))/sum(ifelse(churnTrainClean$voice_mail_plan == 'yes',1,0)))*100)

summarized_data_percent

summarized_data_avg <- churnTrainClean %>%
  group_by(churn) %>%
  summarize(
    avg_vm = mean(number_vmail_messages),
    avg_length = mean(account_length),
    avg_d_charge = mean(total_charge_no_intl),
    avg_i_charge = mean(total_intl_charge),
    avg_d_calls = mean(total_calls_no_intl),
    avg_i_calls = mean(total_intl_calls),
    avg_cs_calls = mean(number_customer_service_calls))

summarized_data_avg

###Are any customers charged different rates?

churnTrainClean$avg_day_rate = churnTrainClean$total_day_charge/churnTrainClean$total_day_minutes
churnTrainClean$avg_eve_rate = churnTrainClean$total_eve_charge/churnTrainClean$total_eve_minutes
churnTrainClean$avg_night_rate = churnTrainClean$total_night_charge/churnTrainClean$total_night_minutes
churnTrainClean$avg_intl_rate = churnTrainClean$total_intl_charge/churnTrainClean$total_intl_minutes

##summary data shows that every customers appears to be charged
##the same rates for each time of day and for domestic and 
##international calls
summary(churnTrainClean$avg_day_rate)
summary(churnTrainClean$avg_eve_rate)
summary(churnTrainClean$avg_night_rate)
summary(churnTrainClean$avg_intl_rate)


###Here we are going to loop through 10 iterations on 5 different models
## to try and identify the model that produces the best 
##area under the curve


##assign 10 seed values to be used in each of our models to create the 
##same set of testing and training data
seeds <- c(100,200,300,400,500,600,700,800,900,1000)

#establish vectors to hold AUC results
auc_test1 <-0
auc_train1 <-0
auc_all1 <-0

auc_test2 <-0
auc_train2 <-0
auc_all2 <-0

auc_test3 <-0
auc_train3 <-0
auc_all3 <-0

auc_test4 <-0
auc_train4 <-0
auc_all4 <-0

auc_test5 <-0
auc_train5 <-0
auc_all5 <-0

#MODEL 1
for (i in 1:10) {
  set.seed(seeds[i])
  
  #Split dataset into test and train data set
  Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
  data_Train<-churnTrainClean[Index_Train,]
  data_Test<-churnTrainClean[-Index_Train,]
  
  Model=glm(churn~state+
              #state_ind+
              international_plan+
              voice_mail_plan+
              number_vmail_messages+
              total_intl_charge+
              number_customer_service_calls +
              
              account_length+area_code+
              total_day_calls+total_eve_calls+ total_night_calls+
              #total_calls_no_intl +
              #total_charge_no_intl + 
              total_day_charge + total_eve_charge + total_night_charge +
              total_intl_calls
            #total_intl_calls_nm
            ,family = "binomial",data = data_Train)
  
  ##Store AUC values for test, train, and entire data set 
  Predict_test<-predict(Model,data_Test, type = "response")
  roc_obj_test <- roc(data_Test$churn, Predict_test)
  auc_test1[i] <- as.numeric(auc(roc_obj_test))
  
  Predict_train<-predict(Model,data_Train, type = "response")
  roc_obj_train <- roc(data_Train$churn, Predict_train)
  auc_train1[i] <- as.numeric(auc(roc_obj_train))
  
  Predict_all<-predict(Model,churnTrainClean, type = "response")
  roc_obj_all <- roc(churnTrainClean$churn, Predict_all)
  auc_all1[i] <- as.numeric(auc(roc_obj_all))
  
}

#MODEL 2
for (i in 1:10) {
  set.seed(seeds[i])
  
  #Split dataset into test and train data set
  Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
  data_Train<-churnTrainClean[Index_Train,]
  data_Test<-churnTrainClean[-Index_Train,]
  
  Model=glm(churn~state_ind+
              international_plan+
              voice_mail_plan+
              number_vmail_messages+
              total_intl_charge+
              number_customer_service_calls +
              
              account_length+area_code+
              #total_day_calls+total_eve_calls+ total_night_calls+
              total_calls_no_intl +
              #total_charge_no_intl + 
              total_day_charge + total_eve_charge + total_night_charge +
              total_intl_calls
            #total_intl_calls_nm
            ,family = "binomial",data = data_Train)
  
  ##Store AUC values for test, train, and entire data set 
  Predict_test<-predict(Model,data_Test, type = "response")
  roc_obj_test <- roc(data_Test$churn, Predict_test)
  auc_test2[i] <- as.numeric(auc(roc_obj_test))
  
  Predict_train<-predict(Model,data_Train, type = "response")
  roc_obj_train <- roc(data_Train$churn, Predict_train)
  auc_train2[i] <- as.numeric(auc(roc_obj_train))
  
  Predict_all<-predict(Model,churnTrainClean, type = "response")
  roc_obj_all <- roc(churnTrainClean$churn, Predict_all)
  auc_all2[i] <- as.numeric(auc(roc_obj_all))
  
}

#MODEL 3
for (i in 1:10) {
  set.seed(seeds[i])
  
  #Split dataset into test and train data set
  Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
  data_Train<-churnTrainClean[Index_Train,]
  data_Test<-churnTrainClean[-Index_Train,]
  
  Model=glm(churn~state_ind+
              international_plan+
              voice_mail_plan+
              number_vmail_messages+
              total_intl_charge+
              number_customer_service_calls +
              
              account_length+area_code+
              #total_day_calls+total_eve_calls+ total_night_calls+
              total_calls_no_intl +
              total_charge_no_intl + 
              #total_day_charge + total_eve_charge + total_night_charge +
              total_intl_calls
            #total_intl_calls_nm
            ,family = "binomial",data = data_Train)
  
  ##Store AUC values for test, train, and entire data set 
  Predict_test<-predict(Model,data_Test, type = "response")
  roc_obj_test <- roc(data_Test$churn, Predict_test)
  auc_test3[i] <- as.numeric(auc(roc_obj_test))
  
  Predict_train<-predict(Model,data_Train, type = "response")
  roc_obj_train <- roc(data_Train$churn, Predict_train)
  auc_train3[i] <- as.numeric(auc(roc_obj_train))
  
  Predict_all<-predict(Model,churnTrainClean, type = "response")
  roc_obj_all <- roc(churnTrainClean$churn, Predict_all)
  auc_all3[i] <- as.numeric(auc(roc_obj_all))
  
}

#MODEL 4
for (i in 1:10) {
  set.seed(seeds[i])
  
  #Split dataset into test and train data set
  Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
  data_Train<-churnTrainClean[Index_Train,]
  data_Test<-churnTrainClean[-Index_Train,]
  
  Model=glm(churn~state_ind+
              international_plan+
              voice_mail_plan+
              number_vmail_messages+
              total_intl_charge+
              number_customer_service_calls +
              
              #account_length+area_code+
              #total_day_calls+total_eve_calls+ total_night_calls+
              #total_calls_no_intl +
              total_charge_no_intl + 
              #total_day_charge + total_eve_charge + total_night_charge +
              total_intl_calls
            #total_intl_calls_nm
            ,family = "binomial",data = data_Train)
  
  ##Store AUC values for test, train, and entire data set 
  Predict_test<-predict(Model,data_Test, type = "response")
  roc_obj_test <- roc(data_Test$churn, Predict_test)
  auc_test4[i] <- as.numeric(auc(roc_obj_test))
  
  Predict_train<-predict(Model,data_Train, type = "response")
  roc_obj_train <- roc(data_Train$churn, Predict_train)
  auc_train4[i] <- as.numeric(auc(roc_obj_train))
  
  Predict_all<-predict(Model,churnTrainClean, type = "response")
  roc_obj_all <- roc(churnTrainClean$churn, Predict_all)
  auc_all4[i] <- as.numeric(auc(roc_obj_all))
  
}

#MODEL 5
for (i in 1:10) {
  set.seed(seeds[i])
  
  #Split dataset into test and train data set
  Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
  data_Train<-churnTrainClean[Index_Train,]
  data_Test<-churnTrainClean[-Index_Train,]
  
  Model=glm(churn~state_ind+
              international_plan+
              voice_mail_plan+
              number_vmail_messages+
              total_intl_charge+
              number_customer_service_calls +
              
              #account_length+area_code
              #total_day_calls+total_eve_calls+ total_night_calls+
              
              total_charge_no_intl + 
              #total_day_charge + total_eve_charge + total_night_charge +
              #total_intl_calls+
              total_intl_calls_nm,family = "binomial",data = data_Train)
  
  ##Store AUC values for test, train, and entire data set 
  Predict_test<-predict(Model,data_Test, type = "response")
  roc_obj_test <- roc(data_Test$churn, Predict_test)
  auc_test5[i] <- as.numeric(auc(roc_obj_test))
  
  Predict_train<-predict(Model,data_Train, type = "response")
  roc_obj_train <- roc(data_Train$churn, Predict_train)
  auc_train5[i] <- as.numeric(auc(roc_obj_train))
  
  Predict_all<-predict(Model,churnTrainClean, type = "response")
  roc_obj_all <- roc(churnTrainClean$churn, Predict_all)
  auc_all5[i] <- as.numeric(auc(roc_obj_all))
  
}

auc_all_test <- cbind(auc_test1, auc_test2, auc_test3, auc_test4, auc_test5)
auc_all_train <- cbind(auc_train1, auc_train2, auc_train3, auc_train4, auc_train5)
auc_all_all <- cbind(auc_all1, auc_all2, auc_all3, auc_all4, auc_all5)

##Ultimately, we are using the average of performance on the training dataset
#to determine the best model to use. However, we wanted to run the model
#against the training dataset, and entire dataset to see how it performed
colMeans(auc_all_test)
colMeans(auc_all_train)
colMeans(auc_all_all)

#On average, model 5 performs better than any of the other 4 models

#pull seed that was associated with the best test output from model 5
seeds[which(auc_test5 == max(auc_test5))]

##Use the seed with the best overall AUC to  build model. 


#MODEL FINAL

set.seed(seeds[which(auc_test5 == max(auc_test5))])
#Split dataset into test and train data set
Index_Train <- createDataPartition(churnTrainClean$churn, p = .8, list = FALSE)
data_Train<-churnTrainClean[Index_Train,]
data_Test<-churnTrainClean[-Index_Train,]

Model_ABC_Wireless=glm(churn~state_ind+
                         international_plan+
                         voice_mail_plan+
                         number_vmail_messages+
                         total_intl_charge+
                         number_customer_service_calls +
                         total_charge_no_intl + 
                         total_intl_calls_nm,family = "binomial",data = data_Train)

##Store AUC values for test, train, and entire data set 
Predict_test_final<-predict(Model_ABC_Wireless,data_Test, type = "response")
Predict_train_final<-predict(Model_ABC_Wireless,data_Train, type = "response")
Predict_all_final<-predict(Model_ABC_Wireless,churnTrainClean, type = "response")



roc(data_Test$churn, Predict_test_final)
plot(roc(data_Test$churn, Predict_test_final), col="red")

roc(data_Train$churn, Predict_train_final)
roc(churnTrainClean$churn, Predict_all_final)

summary(Model_ABC_Wireless)


#Append the predicted value to ChurnTrainClean to compare to actual churn

churnTrainClean$predict <- predict(Model_ABC_Wireless,churnTrainClean, type = "response")

##dividing up prediction ranges by .10 intervals
##the number in front of the colon is just to ensure proper sorting in table
churnTrainClean$prediction_range <- ifelse (churnTrainClean$predict> .90, '1: .90-1.0', 
                                            ifelse(churnTrainClean$predict> .80, '2: .80-.89',
                                                   ifelse(churnTrainClean$predict> .70, '3: .70-.79',
                                                          ifelse(churnTrainClean$predict> .60, '4: .60-.69',
                                                                 ifelse(churnTrainClean$predict> .50, '5: .50-.59',
                                                                        ifelse(churnTrainClean$predict> .40, '6: .40-.49',
                                                                               ifelse(churnTrainClean$predict> .30, '7: .30-.39',
                                                                                      ifelse(churnTrainClean$predict> .20, '8: .20-.29',
                                                                                             ifelse(churnTrainClean$predict> .10, '9: .10-.19','99: 0-.09'))))))))) 

#counting prediction ranges by .10 intervals
churnTrainThreshold <- churnTrainClean %>%
  group_by(prediction_range, churn) %>%
  summarize(avg_prediction = mean(predict), count = n())


#looking at the distribution of our prediction ranges versus the actual churn
churnTrainThreshold_table = dcast(churnTrainThreshold, 
                                  prediction_range~churn,
                                  value.var = "count")
churnTrainThreshold_table


##USING THE FINAL MODEL WITH CUSTOMERS TO PREDICT
##Create required variables for the model

Customers_To_Predict$total_intl_calls_nm = log(Customers_To_Predict$total_intl_calls+1)

Customers_To_Predict$total_charge_no_intl = Customers_To_Predict$total_day_charge + Customers_To_Predict$total_eve_charge + Customers_To_Predict$total_night_charge

Customers_To_Predict$state_ind <- ifelse(Customers_To_Predict$state == "CA" |
                                           Customers_To_Predict$state == "ME" |
                                           Customers_To_Predict$state == "MI" |
                                           Customers_To_Predict$state == "MS" |
                                           Customers_To_Predict$state == "MT" |
                                           Customers_To_Predict$state == "NJ" |
                                           Customers_To_Predict$state == "NV" |
                                           Customers_To_Predict$state == "SC" |
                                           Customers_To_Predict$state == "TX" |
                                           Customers_To_Predict$state == "WA", "Y","N")


#Run model on Customers to Predict
Churn_Prob<-predict(Model_ABC_Wireless, newdata=Customers_To_Predict, type = 'response')

#Append the predicted value to Customers to Predict

Customers_To_Predict$predict <- Churn_Prob

##dividing up prediction ranges by .10 intervals
##the number in front of the colon is just to ensure proper sorting in table
Customers_To_Predict$prediction_range <- ifelse (Customers_To_Predict$predict> .90, '1: .90-1.0', 
                                                 ifelse(Customers_To_Predict$predict> .80, '2: .80-.89',
                                                        ifelse(Customers_To_Predict$predict> .70, '3: .70-.79',
                                                               ifelse(Customers_To_Predict$predict> .60, '4: .60-.69',
                                                                      ifelse(Customers_To_Predict$predict> .50, '5: .50-.59',
                                                                             ifelse(Customers_To_Predict$predict> .40, '6: .40-.49',
                                                                                    ifelse(Customers_To_Predict$predict> .30, '7: .30-.39',
                                                                                           ifelse(Customers_To_Predict$predict> .20, '8: .20-.29',
                                                                                                  ifelse(Customers_To_Predict$predict> .10, '9: .10-.19','99: 0-.09'))))))))) 

#counting prediction ranges by .10 intervals
churnThreshold <- Customers_To_Predict %>%
  group_by(prediction_range) %>%
  summarize(count = n())

churnThreshold$cumsum = cumsum(churnThreshold$count)
churnThreshold