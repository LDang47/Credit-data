if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071","readxl","rpart","partykit","randomForest","httr","h2o","corrplot","ggplot2","xgboost","dplyr","tibble","DataExplorer") #Check, and if needed install the necessary packages

options(scipen = 999,digits = 10)
memory.size(max = TRUE) # Setting memory to max

github_link <- "https://github.com/LDang47/MMA-867-Credit-data/blob/master/MMA867%20A3%20--%20credit%20data.xlsx?raw=true"
temp_file <- tempfile(fileext = ".xlsx")
req <- GET(github_link, 
           # authenticate using GITHUB_PAT
           authenticate(Sys.getenv("GITHUB_PAT"), ""),
           # write result to disk
           write_disk(path = temp_file))
df <- readxl::read_excel(temp_file)

github_link_1 <- "https://github.com/LDang47/MMA-867-Credit-data/blob/master/MMA867%20A3%20--%20new%20applications.xlsx?raw=true"
temp_file_1 <- tempfile(fileext = ".xlsx")
req <- GET(github_link_1,
           authenticate(Sys.getenv("GITHUB_PAT"), ""),
           write_disk(path = temp_file_1))
df_test <- readxl::read_excel(temp_file_1)

# Change variables to factors: df
cols.to.factors <- c("SEX", "EDUCATION", "MARRIAGE", "PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5",  "PAY_6", "default_0")
df[cols.to.factors] <- lapply(df[cols.to.factors], factor)

# Change variables to factors: df_test
cols.to.factors <- c("SEX", "EDUCATION", "MARRIAGE", "PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5",  "PAY_6")
df_test[cols.to.factors] <- lapply(df_test[cols.to.factors], factor)

################################################################################################################
### DATA EXLPLORATION

colnames(df)

table(df$default_0, df$LIMIT_BAL)

df_eda <- df[1:24000,]


### Correlation plot
df_plot <- df_eda
df_plot$default_0 <- as.numeric(df_plot$default_0) #change default_0 to numeric to draw corplot
numericVars <- which(sapply(df_plot, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

df_numVar <- df_plot[, numericVars]
cor_numVar <- cor(df_numVar, use="everything", method = c("pearson", "kendall", "spearman")) #correlations of all numeric variables

#sort on decreasing correlations with default_0
cor_sorted <- as.matrix(sort(cor_numVar[,'default_0'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot(cor_numVar, method = "circle", tl.pos = "lt", tl.col = "black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE) 



####  Convert variables to Factor and change category names

df_eda2 <- df_eda


factorvar <- c("SEX", "EDUCATION", "MARRIAGE", "PAY_1", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "default_0")
df_eda2[factorvar] <- lapply(df_eda2[factorvar], function(x) as.factor(x))


levels(df_eda2$SEX) <- c("Male", "Female")

levels(df_eda2$EDUCATION) <- c("Unknown", "Graduate", "Undergraduate", "High school", "Other", "Unknown", "Unknown")

levels(df_eda2$MARRIAGE) <- c("Other", "Married", "Single", "Other")



### Demographic Charts

# Education plot
g1 <- ggplot(df_eda2, aes(x =EDUCATION, fill = default_0)) +
  geom_bar() +
  labs(x = 'Education')


#  Gender plot
g2 <- ggplot(df_eda2, aes(x = SEX, y = (..count..), fill = default_0)) +
  geom_bar(position = "dodge") +
  labs(x = 'Sex')


# Marital status plot
g3 <- ggplot(df_eda2, aes(x = MARRIAGE, fill = default_0)) +
  geom_bar(position = "dodge") +
  labs(x = 'Marriage')


# Age plot
g4 <- ggplot(df_eda2, aes(x = AGE, fill = default_0)) +
  geom_bar() +
  labs(x = 'Age')

gridExtra::grid.arrange(g1, g2, g3, g4)


# Sex and Education interaction plot
ggplot(df_eda2, aes(x = SEX, fill = default_0)) +
  geom_bar() +
  labs(x = 'SEX') +
  facet_wrap(~EDUCATION)


# Sex and Marriage interaction plot
ggplot(df_eda2, aes(x = SEX, fill = default_0)) +
  geom_bar(position = "dodge" ) +
  labs(x = 'SEX') +
  facet_wrap(~MARRIAGE)


### Pay status Charts

# Pay 1 plot
pg1 <- ggplot(df_eda2, aes(x = PAY_1, fill = default_0)) + geom_bar() +labs(x = 'Pay 1')


# Pay 2 plot
pg2 <- ggplot(df_eda2, aes(x = PAY_2, fill = default_0)) +
  geom_bar() +
  labs(x = 'Pay 2')


# Pay 3 plot
pg3 <- ggplot(df_eda2, aes(x = PAY_3, fill = default_0)) +
  geom_bar() +
  labs(x = 'Pay 3')


# Pay 4 plot
pg4 <- ggplot(df_eda2, aes(x = PAY_4, fill = default_0)) +
  geom_bar() +
  labs(x = 'Pay 4')


# Pay 5 plot
pg5 <- ggplot(df_eda2, aes(x = PAY_5, fill = default_0)) +
  geom_bar() +
  labs(x = 'Pay 5')


# Pay 6 plot
pg6 <- ggplot(df_eda2, aes(x = PAY_6, fill = default_0)) +
  geom_bar() +
  labs(x = 'Pay 6')

gridExtra::grid.arrange(pg1, pg2, pg3, pg4, pg5, pg6)


### Pay Amount and Bill Amount Variables charts

plot_histogram(df_eda2[, 13:24])




################################################################################################################
### LOGISTIC with the "as is" data
# Include all variables in the "base-case" model
df <- data.frame(df)
df_test <- data.frame(df_test)

set.seed(123) #set a random number generation seed to ensure that the split is the same everytime
inTrain_1 <- createDataPartition(y = df$default_0,
                                 p = 19243/24000, list = FALSE)
training <- df[inTrain_1,]
testing <- df[ -inTrain_1,]

baseline_logistic <-glm(default_0 ~.
                        , data=training, family="binomial"(link="logit"))

summary(baseline_logistic) 

## Stepwise regressions
baseline_logistic_stepwiseAIC <-stepAIC(baseline_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(baseline_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(baseline_logistic_stepwiseAIC) #Error plots
par(mfrow=c(1,1))

### Adding Lasso regularisation for feature selection

# Dumy code categorical predictor variables
x <- model.matrix(default_0~., training)[,-1]

# Convert the outcome (class) to a numerical variable
y <- training$default_0

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial") #create cross-validation data
plot(cv.lasso)
cv.lasso$lambda.min

# Estimate the model with the optimal penalty
model_logistic <- glmnet(x, y, alpha = 1, family = "binomial",
                         lambda = cv.lasso$lambda.min)

# Display regression coefficients
coef(model_logistic)

# Make predictions on the test data
x.test <- model.matrix(default_0 ~., testing)[,-1]
logistic_probabilities <- predict(model_logistic, newx = x.test, type="response")

# Translate probabilities to predictions
logistic_classification <- rep(1,nrow(testing))
logistic_classification <- ifelse(logistic_probabilities > 0.2210553, "1", "0")
logistic_classification <- as.factor(logistic_classification)
testing$default_0 <- as.factor(testing$default_0)

# Model accuracy
observed_classes <- testing$default_0
mean(logistic_classification == observed_classes)

###Confusion matrix 
confusionMatrix(logistic_classification,testing$default_0,positive = "1")

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp.logit <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp.logit@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value

#### Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#####################################################################################################################################
### DATA PREPARATION
set.seed(1)

df$dtype <- 'train'
df_test$dtype <- 'new_appl'
df$ID <- as.character(df$ID)

df <- dplyr::bind_rows(df,df_test)
str(df)

#Creating variables using PAY

df$coll_rec_1 <- as.factor(ifelse((df$PAY_1>2),1,0))
df$coll_rec_2 <- as.factor(ifelse((df$PAY_2>2),1,0))
df$coll_rec_3 <- as.factor(ifelse((df$PAY_3>2),1,0))
df$coll_rec_4 <- as.factor(ifelse((df$PAY_4>2),1,0))
df$coll_rec_5 <- as.factor(ifelse((df$PAY_5>2),1,0))
df$coll_rec_6 <- as.factor(ifelse((df$PAY_6>2),1,0))

df$coll_rec_all <- as.factor(ifelse(((df$coll_rec_1==1) & (df$coll_rec_2==1) & (df$coll_rec_3==1)
                                     &(df$coll_rec_4==1) & (df$coll_rec_5==1) & (df$coll_rec_6==1)),1,0))


df$Ocl1 <- as.factor(ifelse((df$BILL_AMT1>df$LIMIT_BAL),1,0))
df$Ocl2 <- as.factor(ifelse((df$BILL_AMT2>df$LIMIT_BAL),1,0))
df$Ocl3 <- as.factor(ifelse((df$BILL_AMT3>df$LIMIT_BAL),1,0))
df$Ocl4 <- as.factor(ifelse((df$BILL_AMT4>df$LIMIT_BAL),1,0))
df$Ocl5 <- as.factor(ifelse((df$BILL_AMT5>df$LIMIT_BAL),1,0))
df$Ocl6 <- as.factor(ifelse((df$BILL_AMT6>df$LIMIT_BAL),1,0))
x_factor_list <- c('SEX','EDUCATION','MARRIAGE','PAY_1','PAY_2','PAY_3','PAY_4',
                   'PAY_5','PAY_6','default_0')
df[x_factor_list] <- lapply(df[x_factor_list], factor)

str(df)

table(df$MARRIAGE)
table(df$EDUCATION)
df$MARRIAGE[df$MARRIAGE==0] <- 3
df$EDUCATION[df$EDUCATION==0] <- 4

###### FEATURE ENGINEERING
## CREDIT USED / UTIL
df$credit_used_1 <- df$BILL_AMT1/df$LIMIT_BAL
df$credit_used_2 <- df$BILL_AMT2/df$LIMIT_BAL
df$credit_used_3 <- df$BILL_AMT3/df$LIMIT_BAL
df$credit_used_4 <- df$BILL_AMT4/df$LIMIT_BAL
df$credit_used_5 <- df$BILL_AMT5/df$LIMIT_BAL
df$credit_used_6 <- df$BILL_AMT6/df$LIMIT_BAL

## Average CREDIT USAGE
df$avg_credit_used16 <- (df$credit_used_1+df$credit_used_2+df$credit_used_3+df$credit_used_4+df$credit_used_5+df$credit_used_6)/6
df <- df %>% mutate(avg_credit_used_bin= case_when(avg_credit_used16 <0 ~ '0',# Util Negative
                                                   avg_credit_used16 >=0 & avg_credit_used16 <0.35  ~ '1',# Util Average
                                                   avg_credit_used16 >=0.35 & avg_credit_used16 <0.5  ~ '2',# Util Average
                                                   avg_credit_used16 >=0.5 & avg_credit_used16 <0.7  ~ '3',# Util Average
                                                   avg_credit_used16 >=0.7 & avg_credit_used16 <0.85  ~ '4',# Util Average
                                                   avg_credit_used16 >=0.85 & avg_credit_used16 <=1   ~ '5',# Util Average
                                                   avg_credit_used16 >1  ~ '6'# Util Average
))

df$avg_credit_used13 <- (df$credit_used_1+df$credit_used_2+df$credit_used_3)/3

df$avg_credit_used46 <- (df$credit_used_4+df$credit_used_5+df$credit_used_6)/3


df$avg_credit_used15 <- (df$credit_used_1+df$credit_used_2+df$credit_used_3+df$credit_used_4+df$credit_used_5)/5 #last 5 months							
df$avg_credit_used14 <- (df$credit_used_1+df$credit_used_2+df$credit_used_3+df$credit_used_4)/4  #last 4 months							
df$average36_16 <- (df$avg_credit_used13/df$avg_credit_used16)  #ratio of utilization of last 3 months and 6 months							
df$average46_16 <- (df$avg_credit_used46/df$avg_credit_used16)  #ratio of utilization of first 3 months and 6 months							

## CREDIT USED FLAG util at 0.35
df$credit_used_035_1 <- ifelse(df$credit_used_1<.35,0,1) 
df$credit_used_035_2 <- ifelse(df$credit_used_2<.35,0,1) 
df$credit_used_035_3 <- ifelse(df$credit_used_3<.35,0,1) 
df$credit_used_035_4 <- ifelse(df$credit_used_4<.35,0,1) 
df$credit_used_035_5 <- ifelse(df$credit_used_5<.35,0,1) 
df$credit_used_035_6 <- ifelse(df$credit_used_6<.35,0,1) 

## CREDIT USED FLAG util at 0.5
df$credit_used_050_1 <- ifelse(df$credit_used_1<.5,0,1) 
df$credit_used_050_2 <- ifelse(df$credit_used_2<.5,0,1) 
df$credit_used_050_3 <- ifelse(df$credit_used_3<.5,0,1) 
df$credit_used_050_4 <- ifelse(df$credit_used_4<.5,0,1) 
df$credit_used_050_5 <- ifelse(df$credit_used_5<.5,0,1) 
df$credit_used_050_6 <- ifelse(df$credit_used_6<.5,0,1) 

## CREDIT USED FLAG util at 0.6
df$credit_used_060_1 <- ifelse(df$credit_used_1<.6,0,1) 
df$credit_used_060_2 <- ifelse(df$credit_used_2<.6,0,1) 
df$credit_used_060_3 <- ifelse(df$credit_used_3<.6,0,1) 
df$credit_used_060_4 <- ifelse(df$credit_used_4<.6,0,1) 
df$credit_used_060_5 <- ifelse(df$credit_used_5<.6,0,1) 
df$credit_used_060_6 <- ifelse(df$credit_used_6<.6,0,1) 

## CREDIT USED FLAG util at 0.7
df$credit_used_070_1 <- ifelse(df$credit_used_1<.7,0,1) 
df$credit_used_070_2 <- ifelse(df$credit_used_2<.7,0,1) 
df$credit_used_070_3 <- ifelse(df$credit_used_3<.7,0,1) 
df$credit_used_070_4 <- ifelse(df$credit_used_4<.7,0,1) 
df$credit_used_070_5 <- ifelse(df$credit_used_5<.7,0,1) 
df$credit_used_070_6 <- ifelse(df$credit_used_6<.7,0,1) 

## CREDIT USED FLAG util at 0.8
df$credit_used_080_1 <- ifelse(df$credit_used_1<.8,0,1) 
df$credit_used_080_2 <- ifelse(df$credit_used_2<.8,0,1) 
df$credit_used_080_3 <- ifelse(df$credit_used_3<.8,0,1) 
df$credit_used_080_4 <- ifelse(df$credit_used_4<.8,0,1) 
df$credit_used_080_5 <- ifelse(df$credit_used_5<.8,0,1) 
df$credit_used_080_6 <- ifelse(df$credit_used_6<.8,0,1) 

## CREDIT USED FLAG util at 0.9
df$credit_used_090_1 <- ifelse(df$credit_used_1<.9,0,1) 
df$credit_used_090_2 <- ifelse(df$credit_used_2<.9,0,1) 
df$credit_used_090_3 <- ifelse(df$credit_used_3<.9,0,1) 
df$credit_used_090_4 <- ifelse(df$credit_used_4<.9,0,1) 
df$credit_used_090_5 <- ifelse(df$credit_used_5<.9,0,1) 
df$credit_used_090_6 <- ifelse(df$credit_used_6<.9,0,1) 


## CREDIT USED FLAG util at 1
df$credit_used_100_1 <- ifelse(df$credit_used_1<1,0,1) 
df$credit_used_100_2 <- ifelse(df$credit_used_2<1,0,1) 
df$credit_used_100_3 <- ifelse(df$credit_used_3<1,0,1) 
df$credit_used_100_4 <- ifelse(df$credit_used_4<1,0,1) 
df$credit_used_100_5 <- ifelse(df$credit_used_5<1,0,1) 
df$credit_used_100_6 <- ifelse(df$credit_used_6<1,0,1) 


## Monthly Debit Adjusted
df$monthly_debit_adj_1 <- df$BILL_AMT1-df$BILL_AMT2-df$PAY_AMT1
df$monthly_debit_adj_2 <- df$BILL_AMT2-df$BILL_AMT3-df$PAY_AMT2
df$monthly_debit_adj_3 <- df$BILL_AMT3-df$BILL_AMT4-df$PAY_AMT3
df$monthly_debit_adj_4 <- df$BILL_AMT4-df$BILL_AMT5-df$PAY_AMT4
df$monthly_debit_adj_5 <- df$BILL_AMT5-df$BILL_AMT6-df$PAY_AMT5

df$avg_monthly_debit <- (df$monthly_debit_adj_1+df$monthly_debit_adj_2+df$monthly_debit_adj_3+df$monthly_debit_adj_4+df$monthly_debit_adj_5)/6
df$avg_monthly_debit_limit_ratio <- df$avg_monthly_debit/df$LIMIT_BAL

## Total Payment
df$total_payment <- df$PAY_AMT1+df$PAY_AMT2+df$PAY_AMT3+df$PAY_AMT4+df$PAY_AMT5+df$PAY_AMT6
df$avg_payment <- df$total_payment/6
df$avg_payment_last_3 <- (df$PAY_AMT1+df$PAY_AMT2+df$PAY_AMT3)/3
df$avg_payment_first_3 <- (df$PAY_AMT4+df$PAY_AMT5+df$PAY_AMT6)/3
df$avg_payment_limit_ratio <- df$avg_payment/df$LIMIT_BAL
# md.pattern(df)

						
df <- df %>% mutate(sex_marriage= case_when(
SEX==1  & MARRIAGE == 1  ~ '1',		
SEX==1  & MARRIAGE == 2  ~ '2',			
SEX==1  & MARRIAGE == 3  ~ '3', 				
SEX==2  & MARRIAGE == 1  ~ '4', 	
SEX==2  & MARRIAGE == 2  ~ '5',			
SEX==2  & MARRIAGE == 3  ~ '6'))					

df$sex_marriage <- as.factor(df$sex_marriage)
				
df <- df %>% mutate(cat_age= case_when(
	AGE >= 20  & AGE <= 29 ~ '1',
	AGE >= 30  & AGE <= 39 ~ '2',
	AGE >= 40  & AGE <= 49 ~ '3',
	AGE >= 50  & AGE <= 59 ~ '4',
	AGE >= 60 ~ '5'
    ))


df <- df %>% mutate(age_sex= case_when(
	SEX==1  & cat_age == 1  ~ '1',
	SEX==1  & cat_age == 2  ~ '2',
	SEX==1  & cat_age == 3  ~ '3',
	SEX==1  & cat_age == 4  ~ '4',
	SEX==1  & cat_age == 5  ~ '5',
	SEX==2  & cat_age == 1  ~ '6',
	SEX==2  & cat_age == 2  ~ '7',
	SEX==2  & cat_age == 3  ~ '8',
	SEX==2  & cat_age == 4  ~ '9',
	SEX==2  & cat_age == 5  ~ '10'				
))			

df$age_sex <- as.factor(df$age_sex)

df <- df %>% mutate(sex_education= case_when(
	SEX==1  & EDUCATION == 1  ~ '1',
	SEX==1  & EDUCATION == 2  ~ '2',
	SEX==1  & EDUCATION == 3  ~ '3',
	SEX==1  & EDUCATION == 4  ~ '4',
	 SEX==1  & EDUCATION == 5  ~ '5',
	 SEX==1  & EDUCATION == 6  ~ '6',
	 SEX==2  & EDUCATION == 1  ~ '7',
	 SEX==2  & EDUCATION == 2  ~ '8',
	 SEX==2  & EDUCATION == 3  ~ '9',
	 SEX==2  & EDUCATION == 4  ~ '10',
	 SEX==2  & EDUCATION == 5  ~ '11',
	 SEX==2  & EDUCATION == 6  ~ '12'				                                 
))						

df$sex_education <- as.factor(df$sex_education)

# Take log of LIMIT_BAL
df$log_LIMIT_BAL <- log(df$LIMIT_BAL)

# Take log of AGE
df$log_AGE <- log(df$AGE)

# Ratio: Payment / Bill amount (Payment this month/ Bill amount last month)
df$Payment_Over_TotalBill1 <- ifelse(df$PAY_AMT1==0 & df$BILL_AMT2==0,0,df$PAY_AMT1/ df$BILL_AMT2)
df$Payment_Over_TotalBill2 <- ifelse(df$PAY_AMT2==0 & df$BILL_AMT3==0,0,df$PAY_AMT2/df$BILL_AMT3)
df$Payment_Over_TotalBill3 <- ifelse(df$PAY_AMT3==0& df$BILL_AMT4==0,0,df$PAY_AMT3/ df$BILL_AMT4)
df$Payment_Over_TotalBill4 <- ifelse(df$PAY_AMT4==0& df$BILL_AMT5==0,0,df$PAY_AMT4/ df$BILL_AMT5)
df$Payment_Over_TotalBill5 <- ifelse(df$PAY_AMT5==0& df$BILL_AMT6==0,0,df$PAY_AMT5/ df$BILL_AMT6)

## Data handling
df$Payment_Over_TotalBill1 <- ifelse(df$PAY_AMT1>0 & df$BILL_AMT2==0,1,df$Payment_Over_TotalBill1)
df$Payment_Over_TotalBill2 <- ifelse(df$PAY_AMT2>0 & df$BILL_AMT3==0,1,df$Payment_Over_TotalBill2)
df$Payment_Over_TotalBill3 <- ifelse(df$PAY_AMT3>0& df$BILL_AMT4==0,1,df$Payment_Over_TotalBill3)
df$Payment_Over_TotalBill4 <- ifelse(df$PAY_AMT4>0& df$BILL_AMT5==0,1,df$Payment_Over_TotalBill4)
df$Payment_Over_TotalBill5 <- ifelse(df$PAY_AMT5>0& df$BILL_AMT6==0,1,df$Payment_Over_TotalBill5)


summary(df)
# md.pattern(df[1:24000,])
## change class of somw new variables
df$bill_negative <- as.factor(ifelse(df$BILL_AMT1<0 &
                                       df$BILL_AMT2<0 &
                                       df$BILL_AMT3<0 &
                                       df$BILL_AMT4<0 &
                                       df$BILL_AMT5<0 &
                                       df$BILL_AMT6<0,1,0))

df$pay_zero <- as.factor(ifelse(df$PAY_AMT1==0 &
                                  df$PAY_AMT2==0 &
                                  df$PAY_AMT3==0 &
                                  df$PAY_AMT4==0 &
                                  df$PAY_AMT5==0 &
                                  df$PAY_AMT6==0,1,0 ))

df$bill_neg_pay_zero <- as.factor(ifelse(df$bill_negative==1 & df$pay_zero==1,1,0))

df$never_use <-  as.factor(ifelse(df$BILL_AMT1==0 &
                                    df$BILL_AMT2==0 &
                                    df$BILL_AMT3==0 &
                                    df$BILL_AMT4==0 &
                                    df$BILL_AMT5==0 &
                                    df$BILL_AMT6==0,1,0))
df$revolver <- as.factor(ifelse(df$PAY_1==0 &
                                  df$PAY_2==0 &
                                  df$PAY_3==0 &
                                  df$PAY_4==0 &
                                  df$PAY_5==0 &
                                  df$PAY_6==0,1,0 ))

df$txnc <- as.factor(ifelse((df$PAY_1==-1 | df$PAY_1==-2) &  
                              (df$PAY_2==-1 | df$PAY_2==-2) &  
                              (df$PAY_3==-1 | df$PAY_3==-2) &  
                              (df$PAY_4==-1 | df$PAY_4==-2) & 
                              (df$PAY_5==-1 | df$PAY_5==-2) &
                              (df$PAY_6==-1 | df$PAY_6==-2), 1,0))

# md.pattern(df)
summary(df)
df$average36_16 <- ifelse(df$average36_16==-Inf, 1.04, df$average36_16) # Median Imputation
df$average46_16 <- ifelse(df$average46_16==Inf, 0.9577, df$average46_16) # Median Imputation

df$average36_16[is.na(df$average36_16)] <- 0
df$average46_16[is.na(df$average46_16)] <- 0


str(df)
x_factor_list <- c('SEX','EDUCATION','MARRIAGE','PAY_1','PAY_2','PAY_3','PAY_4',
                   'PAY_5','PAY_6','default_0', "credit_used_035_1", "credit_used_035_2", 
                   "credit_used_035_3", "credit_used_035_4", "credit_used_035_5", 
                   "credit_used_035_6", "credit_used_050_1", "credit_used_050_2", 
                   "credit_used_050_3", "credit_used_050_4", "credit_used_050_5", 
                   "credit_used_050_6", "credit_used_060_1", "credit_used_060_2", 
                   "credit_used_060_3", "credit_used_060_4", "credit_used_060_5", 
                   "credit_used_060_6", "credit_used_070_1", "credit_used_070_2", 
                   "credit_used_070_3", "credit_used_070_4", "credit_used_070_5", 
                   "credit_used_070_6", "credit_used_080_1", "credit_used_080_2", 
                   "credit_used_080_3", "credit_used_080_4", "credit_used_080_5", 
                   "credit_used_080_6", "credit_used_090_1", "credit_used_090_2", 
                   "credit_used_090_3", "credit_used_090_4", "credit_used_090_5", 
                   "credit_used_090_6", "credit_used_100_1", "credit_used_100_2", 
                   "credit_used_100_3", "credit_used_100_4", "credit_used_100_5", 
                   "credit_used_100_6","cat_age","avg_credit_used_bin")
df[x_factor_list] <- lapply(df[x_factor_list], factor)

#interac_sex_marriage_ed <- model.matrix(~(SEX+EDUCATION+MARRIAGE)^2,df)

#df <- cbind(df,interac_sex_marriage_ed[,-1])
str(df, list.len=ncol(df))
df_test <- df %>% filter(dtype=="new_appl")
df <- df %>% filter(dtype=="train") 
# df_train <- df %>% sample_frac(.8) 
# df_valid <- df %>% filter(!ID %in% c(df_train$ID))

##########################################################################################################################
df <- subset(df, select = -dtype)
df_test <- subset(df_test, select = -dtype)

df$ID <- as.numeric(df$ID)

set.seed(123) #set a random number generation seed to ensure that the split is the same everytime

inTrain <- createDataPartition(y = df$default_0,
                                 p = 19243/24000, list = FALSE)
train <- df[inTrain,]
test <- df[ -inTrain,]

### CTREE 
###

ctree_tree<-ctree(default_0 ~ .
                  ,data=train) #Run ctree on training data

plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities <- predict(ctree_tree,newdata=test,type="prob") #Predict probabilities
ctree_classification <-rep("1",4756)
mean(testing$default_0 == "1")
ctree_classification[ctree_probabilities[,2] < 0.2210553]="0"
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")

####ROC Curve 
ctree_probabilities_testing <-predict(ctree_tree,newdata=test,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], test$default_0) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp_1 <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp_1@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value

#### Lift chart
plotLift(ctree_probabilities[,2],  test$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

##############################################################################################################################
### RPART
###
CART_cp = rpart.control(cp = 0.00000001) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0 ~ .
                  ,data=train, method="class", control=CART_cp) #"Grow" a tree on training data

caret::varImp(rpart_tree, surrogates = FALSE, competes = TRUE)

prunned_rpart_tree<-prune(rpart_tree, cp=  0.0013) #Prun the tree
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree 

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) 
printcp(rpart_tree)

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=test, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,test$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=test,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], test$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp_2 <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp_2@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

###########################################################################################################################
## XGBoost
dput(colnames(df))
predictors <- c(colnames(df)[1],colnames(df)[7:25],colnames(df)[27:40],colnames(df)[77:109])
predictors

predictors_1 <- c("ID","default_0","PAY_1", 
                  "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "BILL_AMT1", "BILL_AMT2", 
                  "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", 
                  "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "coll_rec_1", "coll_rec_2", "coll_rec_3", "coll_rec_4", 
                  "coll_rec_5", "coll_rec_6", "coll_rec_all", "Ocl1", "Ocl2", "Ocl3", 
                  "Ocl4", "Ocl5", "Ocl6", "credit_used_1", "credit_used_2", "credit_used_3", 
                  "credit_used_4", "credit_used_5", "credit_used_6", "credit_used_100_1", "credit_used_100_2", "credit_used_100_3", 
                  "credit_used_100_4", "credit_used_100_5", "credit_used_100_6","avg_credit_used16", 
                  "avg_credit_used_bin", "avg_credit_used13", "avg_credit_used46", 
                  "avg_credit_used15", "avg_credit_used14", "average36_16", "average46_16",
                  "monthly_debit_adj_1", "monthly_debit_adj_2", "monthly_debit_adj_3", 
                  "monthly_debit_adj_4", "monthly_debit_adj_5", "avg_monthly_debit", 
                  "avg_monthly_debit_limit_ratio", "total_payment", "avg_payment", 
                  "avg_payment_last_3", "avg_payment_first_3", "avg_payment_limit_ratio", 
                  "sex_marriage", "cat_age", "age_sex", "sex_education", "log_LIMIT_BAL", 
                  "log_AGE", "Payment_Over_TotalBill1", "Payment_Over_TotalBill2", 
                  "Payment_Over_TotalBill3", "Payment_Over_TotalBill4", "Payment_Over_TotalBill5", 
                  "bill_negative", "pay_zero", "bill_neg_pay_zero", "never_use", 
                  "revolver", "txnc")
predictors_1

df_train <- df %>% dplyr::select(c(predictors_1))%>% sample_n(19200) 
df_valid <- df %>% dplyr::filter(!ID %in% c(df_train$ID))%>% dplyr::select(c(predictors_1))
df_test <- df_test%>% dplyr::select(c(predictors_1))

library(xgboost)
model_XGboost<-xgboost(data = data.matrix(df_train[,c(-1,-2)]), 
                       label = as.numeric(as.character(df_train$default_0)), 
                       eta = 0.1,       # hyperparameter: learning rate
                       max_depth = 3,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations
                       objective = "binary:logistic",
                       
)

XGboost_prediction<-predict(model_XGboost,newdata=data.matrix(df_valid[,c(-1,-2)]), type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.24,1,0)),df_valid$default_0,positive="1") #Display confusion matrix

results_df <- data.frame(threshold = c(""), expected_profit = c(as.numeric('')),tpc=c(""),tnc=c(""),fpc=c(""),fnc=c(""))


for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)) {
  print(threshold)
  ctest <- confusionMatrix(as.factor(ifelse(XGboost_prediction>threshold,1,0)),df_valid$default_0,positive="1")
  df_c <- data.frame(ctest$table)
  
  total_profit <- 1500 * df_c[1,3] - 5000 * df_c[3,3]
  
  results_df <- rbind(results_df,
                      data.frame(threshold = c(as.character(paste(threshold))), 
                                 expected_profit = c(as.numeric(paste(total_profit))),
                                 tpc=c(as.character(paste(df_c[4,3]))),
                                 tnc=c(as.character(paste(df_c[1,3]))),
                                 fpc=c(as.character(paste(df_c[2,3]))),
                                 fnc=c(as.character(paste(df_c[3,3])))
                      ))
}

results_df <- results_df[-1,]
results_df$ratio <- as.double(results_df$tnc)/as.double(results_df$fnc)



write.csv(results_df,"C:/result_df_xgboost.csv")
plot(results_df$threshold,results_df$expected_profit,type="l")
lines(results_df$threshold,results_df$expected_profit,type="l")


####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, df_valid$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, df_valid$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)

xgboost_final <-predict(model_XGboost,newdata=data.matrix(df_test[,c(-1,-2)]), type="response")

################################################################################################################################
### GLM and GBM using h2o

detach("package:h2o", unload = TRUE)
library(h2o)
# Initiallizing H2O with 2 threads and 4 gigs of memory
h2o.shutdown()
h2o.init(nthreads = 2,max_mem_size = "4g")

# Loading data frame to H2o frame
df_train_h20 <- as.h2o(as.data.frame(df))
# df_test_h20 <- as.h2o(as.data.frame(df_test))

# Spliting the the dataset in 80 TRAIN - 20 VALIDATION
df.splits <- h2o.splitFrame(data =  df_train_h20, ratios = .8)
train <- df.splits[[1]] # 80 % data
valid <- df.splits[[2]] # 20 % data validation

predictors_1 <- c("PAY_1", 
                  "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "BILL_AMT1", "BILL_AMT2", 
                  "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", 
                  "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "coll_rec_1", "coll_rec_2", "coll_rec_3", "coll_rec_4", 
                  "coll_rec_5", "coll_rec_6",  "Ocl1", "Ocl2", "Ocl3", 
                  "Ocl4", "Ocl5", "Ocl6", "credit_used_1", "credit_used_2", "credit_used_3", 
                  "credit_used_4", "credit_used_5", "credit_used_6", "avg_credit_used16", 
                  "avg_credit_used_bin",
                  "monthly_debit_adj_1", "monthly_debit_adj_2", "monthly_debit_adj_3", 
                  "monthly_debit_adj_4", "monthly_debit_adj_5", "avg_monthly_debit", 
                  "avg_monthly_debit_limit_ratio", "total_payment", "avg_payment", 
                  "avg_payment_last_3", "avg_payment_first_3", "avg_payment_limit_ratio", 
                  "sex_marriage", "cat_age", "age_sex", "sex_education", "log_LIMIT_BAL", 
                  "log_AGE", "Payment_Over_TotalBill1", "Payment_Over_TotalBill2", 
                  "Payment_Over_TotalBill3", "Payment_Over_TotalBill4", "Payment_Over_TotalBill5", 
                  "bill_negative", "pay_zero", "bill_neg_pay_zero", "never_use", 
                  "revolver")
predictors_1

# Setting Y variable
response <- "default_0"

##### GLM (SELECTED MODEL)

# interact_list <- c("SEX","MARRIAGE")
credit_glm <- h2o.glm(x = predictors_1, y = response, training_frame = train,
                      validation_frame = valid,seed = 1,family = 'binomial',
                      link='logit',nfolds=10
                      
                      # ,interaction_pairs =list(
                      # c("SEX","MARRIAGE") )
                      # ,interactions = interact_list
                      # ignore_const_cols = FALSE
)

print(h2o.auc(credit_glm, valid = TRUE))

# Checking Accuracy on the validation frame
h2o.confusionMatrix(credit_glm,valid=T)
h2o.confusionMatrix(credit_glm,thresholds=0.22,valid=T)

# Plotting the coefficients of variables
h2o.std_coef_plot(credit_glm,num_of_features=50)
h2o.varimp_plot(credit_glm,num_of_features=50)
x <- h2o.varimp(credit_glm)

h2o.performance(credit_glm)

#ROC curve for the validation dataset
plot(h2o.performance(credit_glm), type="roc", main="ROC Curve for GLM")

h2o.exportFile(h2o.predict(credit_glm,as.h2o(df_test),thresholds=0.22),"C:/Users/Alo-Ai day-Toi day/Desktop/final_pred_3.csv")

############### GBM (NOT SELECTED)

credit.gbm <- h2o.gbm(x = predictors_1, y = response, training_frame = train, validation_frame = valid,
                      ntrees = 50, nfolds = 5, seed = 1234)

h2o.auc(credit.gbm,valid=T)
h2o.confusionMatrix(credit.gbm,valid=T)
h2o.varimp_plot(credit.gbm,num_of_features = 40)
h2o.performance(credit.gbm)

#ROC curve for the validation dataset
plot(h2o.performance(credit.gbm), type="roc", main="ROC Curve for GBM")

#h2o.exportFile(h2o.predict(credit.gbm,as.h2o(df_test)),"C:/Users/Alo-Ai day-Toi day/Desktop/final_pred_5.csv")


