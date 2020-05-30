test_h20 <- h2o.exportFile(h2o.predict(credit_glm_wo_sex,as.h2o(valid)),"E:/MMA2020/5 MMA 867 Predictive modelling/Team Assignment 2/test_h2o_glm_wo_gender.csv")

test_h2o_1 <- read.csv("E:/MMA2020/5 MMA 867 Predictive modelling/Team Assignment 2/test_h2o_glm_wo_gender.csv")

test_h20_valid <- h2o.exportFile(valid,"E:/MMA2020/5 MMA 867 Predictive modelling/Team Assignment 2/test_h2o_valid_glm_wo_gender.csv")

test_h20_valid <- read.csv("E:/MMA2020/5 MMA 867 Predictive modelling/Team Assignment 2/test_h2o_valid_glm_wo_gender.csv")

test_h20_valid <- cbind(test_h20_valid,test_h2o_1)
# test_h20_valid <- test_h20_valid %>% dplyr::select(default_0,predict)


table(test_h20_valid$SEX,test_h20_valid$predict)

#Calculate Profit using the formula given in the assignment specifications
Calculate_Profit <- function(Default_0, Predicted_Values){
  #If the credit is issued and repaid. Default_0=0 and Predicted=0
  if (Default_0 == 0 && Predicted_Values == 0){
    Profit <- 1500
  }else if (Default_0 == 1 && Predicted_Values == 0){#Credit is granted but the client defaults; Default_0=1 and Predicted=0
    Profit <- (-5000)
  }else{
    Profit <- 0
  }
  return(Profit)
}
#Make the function capable of working with Vector data.
v_Calculate_Profit <- Vectorize(Calculate_Profit)

test_h20_valid <- mutate(test_h20_valid, True_Profit = v_Calculate_Profit(default_0, default_0))
head(test_h20_valid)
sum(test_h20_valid$True_Profit)

test_h20_valid <- mutate(test_h20_valid, H2o_Profit = v_Calculate_Profit(default_0, predict))
head(test_h20_valid)
sum(test_h20_valid$H2o_Profit)




df_q3 <- read.csv("E:/MMA2020/5 MMA 867 Predictive modelling/Team Assignment 2/Q3_2_final_data.csv")



results_df_q3 <- data.frame(threshold = c(""),p0withoutsex=c(""),
                            credit_without_sex=c(""),p0withsex=c(""),
                            credit_with_sex=c(""),
                            ID=c(""),SEX=c(""))


for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)) {
  print(threshold)
  credit_without_sex <- ifelse(df_q3$p0withoutsex>threshold,1,0)
  credit_with_sex <- ifelse(df_q3$p0withsex>threshold,1,0)
  results_df_q3 <- rbind(results_df_q3,
                         data.frame(threshold = c(as.character(paste(threshold))),
                                    p0withoutsex=c(paste(df_q3$p0withoutsex)),
                                    credit_without_sex=c(paste(credit_without_sex)),
                                    p0withsex=c(paste(df_q3$p0withsex)),
                                    credit_with_sex=c(paste(credit_with_sex)),
                                    ID=c(paste(df_q3$ID)),SEX=c(paste(df_q3$SEX)))
                      )
}

test <- results_df_q3 %>% dplyr::filter(ID==2)
results_df_q3 <- results_df_q3[-1,]


results_df_q3$credit_without_sex <- as.double(results_df_q3$credit_without_sex)
results_df_q3$credit_with_sex <- as.double(results_df_q3$credit_with_sex)


results_df_q3_agg <- results_df_q3 %>% group_by(threshold,SEX) %>% summarise(credit_without_sex=sum(credit_without_sex),credit_with_sex=sum(credit_with_sex))






