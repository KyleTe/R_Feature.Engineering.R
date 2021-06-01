###

###Import Data & Setup
library(data.table)
airbnb_train_x <- fread("C:\\Users\\kyle8\\Documents\\airbnb_train_x.csv")
airbnb_train_y <- fread("C:\\Users\\kyle8\\Documents\\airbnb_train_y.csv")
airbnb_test_x <- fread("C:\\Users\\kyle8\\Documents\\airbnb_test_x.csv")
###Exploratory Data Analysis/Feature Engineering

#(1) First, let's toss out some variables. Reasons included too many NaN in that column(and we think
#it's also hard to replace it with other made-up value) or based on our domain knowledge, our group 
#thinks it will has tiny effects on our final predictions. 
#exclude txt for now
#26 variables!

library(tidyverse)
airbnb_train_x2=airbnb_train_x %>% select(3,4,5,6,7,8,9,11,12,13,14,15,17,21,23,29,33,34,36,39,42,47,48,49,55,56)


#(2) Let's pick out the numeric data and see the correlation!
temp_corr=airbnb_train_x %>% select(3,5,6,7,8,9,11,12,21,23,29,33,36,48,49,55)
library(tidyr)

# drop the NA here to facilitate our exploration
temp_corr=temp_corr %>% drop_na()

#data manipulation1 (response_rate change to numerical)
temp_corr$host_response_rate=as.character(temp_corr$host_response_rate)
temp_corr$host_response_rate=as.numeric(gsub("\\%", "",temp_corr$host_response_rate))
#Make it to be numeric value of percentage
temp_corr$host_response_rate=temp_corr$host_response_rate/100


#drop the NA caused by gsub()
temp_corr=temp_corr %>% drop_na()


#data manipulation2 (extra people, price) remove $
temp_corr$extra_people=as.character(temp_corr$extra_people)
temp_corr$extra_people=as.numeric(gsub("\\$", "",temp_corr$extra_people))


temp_corr$price=as.character(temp_corr$price)
temp_corr$price=as.numeric(gsub("\\$", "",temp_corr$price))


temp_corr=temp_corr %>% drop_na()


#Trun all the datatype to numeric
temp_corr=as.data.frame(sapply(temp_corr, as.numeric))
class(temp_corr$availability_30)

corr=as.data.frame(cor(temp_corr, use='pairwise.complete.obs', method='pearson' ))


#From the corr dataframe we can see that, bedrooms, bathrooms & beds have high correlation with the price.
#Put it simply, as bedrooms/bathrooms/beds numbers goes up, the price will go up as well.

#(3)Data Cleansing!

#Combinr train_y & train_x together.
y=(airbnb_train_y$high_booking_rate)
df_xy1 = data.frame((y),airbnb_train_x2)

#Rename the column
colnames(df_xy1)[1] <-"Booking_Rate"

#Deal with the missing value & NA value,first fill empty cell with NA as well
df_xy1[df_xy1==""]<-NA


#From below codes, we saw that host_response_rate & host_response_time each has relatively high NA values
#compared to others,which are 15794, accounts for around 15% of our data. We will drop the NA in which those NA only accounts
#for less than 2% in that specific columns.
summary(df_xy1)

#Cleaning host_response_rate 
#Fill the NA values with median
df_xy1$host_response_rate=as.character(df_xy1$host_response_rate)
df_xy1$host_response_rate=as.numeric(gsub("\\%", "",df_xy1$host_response_rate))
df_xy1$host_response_rate=df_xy1$host_response_rate/100
df_xy1$host_response_rate[is.na(df_xy1$host_response_rate)] <- median(df_xy1$host_response_rate, na.rm=TRUE)

#Cleaning host_response_time
#Fill the NA values with the mode
summary(df_xy1$host_response_time)# mode: within an hour 
df_xy1$host_response_time[is.na(df_xy1$host_response_time)] <- 'within an hour'


#Cleaning Price, extrapeople($)
df_xy1$extra_people=as.character(df_xy1$extra_people)
df_xy1$extra_people=as.numeric(gsub("\\$", "",df_xy1$extra_people))

df_xy1$price=as.character(df_xy1$price)
df_xy1$price=as.numeric(gsub("\\$", "",df_xy1$price))


#Drop the rest NA cell in the dataset
df_xy1=df_xy1 %>% drop_na()

#Make all numerical data to be numerical not factor
df_xy1$accommodates=as.numeric(df_xy1$accommodates)
df_xy1$availability_30=as.numeric(df_xy1$availability_30)
df_xy1$availability_365=as.numeric(df_xy1$availability_365)
df_xy1$availability_60=as.numeric(df_xy1$availability_60)
df_xy1$availability_90=as.numeric(df_xy1$availability_90)
df_xy1$bathrooms=as.numeric(df_xy1$bathrooms)
df_xy1$bedrooms=as.numeric(df_xy1$bedrooms)
df_xy1$beds=as.numeric(df_xy1$beds)
df_xy1$extra_people=as.numeric(df_xy1$extra_people)
df_xy1$guests_included=as.numeric(df_xy1$guests_included)
df_xy1$host_listings_count=as.numeric(df_xy1$host_listings_count)
df_xy1$host_response_rate=as.numeric(df_xy1$host_response_rate)
df_xy1$host_total_listings_count=as.numeric(df_xy1$host_total_listings_count)
df_xy1$maximum_nights=as.numeric(df_xy1$maximum_nights)
df_xy1$minimum_nights=as.numeric(df_xy1$minimum_nights)
df_xy1$price=as.numeric(df_xy1$price)

#Now we need to purify the amenities column into five category
#(1)Indoor fireplace, Heating
#(2)Air conditioning
#(3)Wireless Internet, Wifi, Internet
#(4)TV, Cable TV
#(5)Free parking on premises, Free street parking


# regexpr(pattern, x)
#extract(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE
df_xy1=extract(df_xy1,'amenities',into = 'Indoor fireplace',regex='(Indoor fireplace)',remove=FALSE)
df_xy1=extract(df_xy1,'amenities',into = 'Heating',regex='(Heating)',remove=FALSE)

df_xy1=extract(df_xy1,'amenities',into = 'Air conditioning',regex='(Air conditioning)',remove=FALSE)

df_xy1=extract(df_xy1,'amenities',into = 'Wireless Internet',regex='(Wireless Internet)',remove=FALSE)
df_xy1=extract(df_xy1,'amenities',into = 'Wifi',regex='(Wifi)',remove=FALSE)
df_xy1=extract(df_xy1,'amenities',into = 'Internet',regex='(Internet)',remove=FALSE)

df_xy1=extract(df_xy1,'amenities',into = 'TV',regex='(TV)',remove=FALSE)
df_xy1=extract(df_xy1,'amenities',into = 'Cable TV',regex='(Cable TV)',remove=FALSE)

df_xy1=extract(df_xy1,'amenities',into = 'Free parking on premises',regex='(Free parking on premises)',remove=FALSE)
df_xy1=extract(df_xy1,'amenities',into = 'Free street parking',regex='(Free street parking)',remove=FALSE)

#fill all NA value in Dataframe with 0 first
df_xy1[is.na(df_xy1)] <- 0

#replace the value in above column with 1
df_xy1$`Free street parking`[df_xy1$`Free street parking` =='Free street parking'	] <- 1
df_xy1$`Free parking on premises`[df_xy1$`Free parking on premises` =='Free parking on premises'] <- 1
df_xy1$`Cable TV`[df_xy1$`Cable TV` =='Cable TV'	] <- 1
df_xy1$TV[df_xy1$TV =='TV'	] <- 1 
df_xy1$Internet[df_xy1$Internet =='Internet'	] <- 1
df_xy1$Wifi[df_xy1$Wifi =='Wifi'	] <- 1
df_xy1$`Wireless Internet`[df_xy1$`Wireless Internet`=='Wireless Internet'	] <- 1
df_xy1$`Air conditioning`[df_xy1$`Air conditioning`=='Air conditioning'	] <- 1
df_xy1$Heating[df_xy1$Heating=='Heating'	] <- 1
df_xy1$`Indoor fireplace`[df_xy1$`Indoor fireplace`=='Indoor fireplace'] <- 1

#Now combine similar columns
df_xy1$`Free street parking`=as.numeric(df_xy1$`Free street parking`)
df_xy1$`Free parking on premises`=as.numeric(df_xy1$`Free parking on premises`)
df_xy1$Parking <- c(df_xy1$`Free street parking`+df_xy1$`Free parking on premises`)
df_xy1$Parking<-ifelse(df_xy1$Parking>=1,1,0)
df_xy1$`Free street parking`<- NULL
df_xy1$`Free parking on premises`<- NULL
df_xy1$Parking=as.factor(df_xy1$Parking)


df_xy1$`Cable TV`=as.numeric(df_xy1$`Cable TV`)
df_xy1$TV=as.numeric(df_xy1$TV)
df_xy1$Television <- c(df_xy1$`Cable TV`+df_xy1$TV)
df_xy1$Television<-ifelse(df_xy1$Television>=1,1,0)
df_xy1$`Cable TV`<- NULL
df_xy1$TV<- NULL
df_xy1$Television=as.factor(df_xy1$Television)



df_xy1$Internet=as.numeric(df_xy1$Internet)
df_xy1$Wifi=as.numeric(df_xy1$Wifi)
df_xy1$`Wireless Internet`=as.numeric(df_xy1$`Wireless Internet`)
df_xy1$WIFI <- c(df_xy1$Internet+df_xy1$Wifi+df_xy1$`Wireless Internet`)
df_xy1$WIFI <-ifelse(df_xy1$WIFI >=1,1,0)
df_xy1$Internet<- NULL
df_xy1$Wifi<- NULL
df_xy1$`Wireless Internet`<- NULL
df_xy1$WIFI=as.factor(df_xy1$WIFI)

df_xy1$`Air conditioning`=as.factor(df_xy1$`Air conditioning`)


df_xy1$Heating=as.numeric(df_xy1$Heating)
df_xy1$`Indoor fireplace`=as.numeric(df_xy1$`Indoor fireplace`)
df_xy1$Heater <- c(df_xy1$Heating+df_xy1$`Indoor fireplace`)
df_xy1$Heater<-ifelse(df_xy1$Heater>=1,1,0)
df_xy1$`Indoor fireplace`<- NULL
df_xy1$Heating<- NULL
df_xy1$Heater=as.factor(df_xy1$Heater)

#Drop the amenities column, since we have extracted valuable data
df_xy1$amenities<- NULL


#examine all columns data type again and change it if necessary
df_xy1$Booking_Rate=as.factor(df_xy1$Booking_Rate)
#Now we only have numerical & factor data type in our dataset!

#Add two variables: price per person; shared bathroom
df_xy1$price_per_person <- df_xy1$price/df_xy1$accommodates
bb <- df_xy1$bathrooms/df_xy1$bedrooms
df_xy1$shared_bathroom <- ifelse(bb<1, 1, 0)
df_xy1$shared_bathroom <- as.factor(df_xy1$shared_bathroom)
## delete price var
df_xy1$price = NULL

#(4) Do a simple kmeans-clustering on our numerical data
temp_num=df_xy1 %>% select(2,4,5,6,7,8,9,10,15,16,17,18,20,24,25,26,32)

#Subtracting the mean and dividing by the standard deviation makes individual data points comparable.
temp_num=scale(temp_num)

km.out = kmeans(x=temp_num,centers=4,nstart=10) 
#do with k=4, since...
kmean_df_xy_1=cbind(km.out$cluster,df_xy1)
kmean_df_xy_1$`km.out$cluster`=as.factor(kmean_df_xy_1$`km.out$cluster`)
colnames(kmean_df_xy_1)[1] <- "cluster"
colnames(kmean_df_xy_1)[4]<-"Air_conditioning"
     
kmean_df_xy_1$city = NULL
kmean_df_xy_1$city_name= NULL
kmean_df_xy_1$country =NULL
#crate dummyies for categorical data
#use library fasedummy
library(fastDummies)

cancel_dummy = model.matrix(~cancellation_policy+0, data=kmean_df_xy_1)

host_response_dummy = fastDummies::dummy_cols(kmean_df_xy_1$host_response_time)
host_response_dummy$.data = host_response_dummy$.data_ = host_response_dummy$.data_f = NULL

instantbook_dummy = fastDummies::dummy_cols(kmean_df_xy_1$instant_bookable)
instantbook_dummy = data.frame(instantbook_dummy$.data_f,instantbook_dummy$.data_t)

islocation_dummy = fastDummies::dummy_cols(kmean_df_xy_1$is_location_exact)
islocation_dummy$.data=islocation_dummy$.data_=NULL

market_dummy = fastDummies::dummy_cols(kmean_df_xy_1$market)
market_dummy$.data=market_dummy$.data_=market_dummy$`.data_$1,100.00`=market_dummy$`.data_$2,999.00`=market_dummy$`.data_$750.00`=NULL

property_dummy = fastDummies::dummy_cols(kmean_df_xy_1$property_type)
property_dummy$.data=property_dummy$.data_=NULL

kmean_df_xy_1$cancellation_policy = kmean_df_xy_1$host_response_time = kmean_df_xy_1$instant_bookable=kmean_df_xy_1$is_location_exact=kmean_df_xy_1$market=kmean_df_xy_1$property_type = NULL

kmean_df_xy_1=data.frame(kmean_df_xy_1,cancel_dummy,host_response_dummy,instantbook_dummy,islocation_dummy,market_dummy,property_dummy)
  

#(5) Do train_test_split
#Set the seed to 12345 and randomly partition the data into 30% testing data 
#and the remaining 70% data as training data.
set.seed(54321)
test_insts=sample(nrow(kmean_df_xy_1),0.3*nrow(kmean_df_xy_1))
bnb_Test=kmean_df_xy_1[test_insts,]
bnb_Train=kmean_df_xy_1[-test_insts,]

#(6) Now we can train our model!

library(ISLR)
library(randomForest)
library(gbm)

rf.mod <- randomForest(Booking_Rate~.,data=bnb_Train,mtry=4,ntree=1000,importance=TRUE,na.action=na.exclude)

##
rf_preds <- predict(rf.mod,newdata=bnb_Test)
preds1= as.data.frame.numeric(rf_preds)
rf_acc <- sum(ifelse(preds1$rf_preds==test_y$bnb_Test.Booking_Rate,1,0),na.rm=T)/test_size


boost_bnb<-gbm(Booking_Rate~.,data=bnb_Train,distribution="bernoulli",n.trees=10,interaction.depth=4,na.action = na.pass)
boost_preds <- predict(boost_bnb,newdata=bnb_Test,type='response',n.trees=10)#
View(boost_preds)



#xgb
library(xgboost)

# numeric only for train
bnb_Train[] <- lapply(bnb_Train, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(bnb_Train, class)
# numeric only for test
bnb_Test[] <- lapply(bnb_Test, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(bnb_Test, class)


dtrain = xgb.DMatrix(data = as.matrix(bnb_Train),
                     label = bnb_Train$Booking_Rate)

dtest = xgb.DMatrix(data = as.matrix(bnb_Test),
                    label = bnb_Test$Booking_Rate)

xgb.params = list(
  colsample_bytree = 0.9,                    
  subsample = 0.9,                      
  booster = "gbtree",
  max_depth = 10,           
  eta = 0.03,
  eval_metric = "rmse",                      
  objective = "reg:linear",
  gamma = 0)      

cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,    
  nrounds=200,   
  early_stopping_rounds = 30, 
  print_every_n = 20
) 

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

best.nrounds = cv.model$best_iteration 
best.nrounds

xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

xgb_y = predict(xgb.model, dtest)
mean((xgb_y - bnb_Test$Booking_Rate)^2)

py=ifelse(xgb_y>0.5,1,0)
act1=as.numeric(bnb_Test$Booking_Rate)
sum(ifelse(py==act1,1,0))/29531

set.seed(123)
xgb <- xgboost(data = data.matrix(dtrain), 
               label = bnb_Train$Booking_Rate,
               objective = "multi:softprob",
               num_class = 2,
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 123,
               eval_metric = "merror",
               nthread = 3
)



dv <- data.matrix(dfTest)
y_pred = predict(xgb,dv)
y_pred_1 <- y_pred[c(FALSE, TRUE)]
predictedDf <- data.frame(dfTest$id,y_pred_1)
predictedDf<-predictedDf%>%
  rename(
    'id' = dfTest.id ,
    'high_booking_rate'= y_pred_1
  )
predictedDf
