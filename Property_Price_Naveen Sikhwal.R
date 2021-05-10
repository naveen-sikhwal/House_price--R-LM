library(dplyr)
library(lattice)
library(car)
library(caret)
library(lmtest)
library(corrplot)
library(ggplot2)
library(e1071)
library(Metrics)
library(dummies)
library(predictmeans)

# Read Data #
Train <- read.csv("Property_Price_Train.csv")
Test <- read.csv("Property_Price_Test.csv")

# Check Summary Train
summary(Train)
str(Train)
# 1459 rows 81 col

# Check Summary Test
summary(Test)
str(Test)
# 1459 rows 80 col

# Merge test and train for data cleaning / preparation

Test$Sale_Price <- c(rep(0,nrow(Test)))
length(Test$Sale_Price)
property <- rbind(Train, Test)
str(property)
summary(property)

# check for NA's
sum(is.na(property)) ## 13960 NA's value
sort(colSums(is.na(property)))

# check for Duplicated data
sum(duplicated(property)) 
# no  duplicated data


# check blanks
colSums(property == "" | property == " ", na.rm = T)
# no blank space

# check catagorical and continious variables

sum(sapply(property,is.factor))
sum(sapply(property, is.numeric))

names(property)[which(sapply(property,is.factor))]
names(property)[which(sapply(property,is.numeric))]

# convert Building class, Overall_ material & House condition to factor
property$Building_Class <- as.factor(property$Building_Class)
property$Overall_Material <- as.factor(property$Overall_Material)
property$House_Condition <- as.factor(property$House_Condition)

## Treating NA's in numeric variables

summary(property[which(sapply(property, is.numeric))])

# removing columns with NA values greater than 80%

property <- subset(property, select= -c(Fence_Quality,Lane_Type,Miscellaneous_Feature,Pool_Quality))


#1. Lot_Extent

sum(is.na(property$Lot_Extent))
# 486 NA's
summary(property$Lot_Extent)
hist(property$Lot_Extent)
quantile(property$Lot_Extent, seq(0,1,0.02), na.rm = T)
# since the distribution is almost normal (only 2-4% outliers)
# we will replace the NA's with Median
property$Lot_Extent[which(is.na(property$Lot_Extent))] <- median(property$Lot_Extent, na.rm = T)
summary(property$Lot_Extent)

#2 Brick_Veneer_Area

summary(property$Brick_Veneer_Area)
hist(property$Brick_Veneer_Area)
quantile(property$Brick_Veneer_Area, seq(0,1,0.02), na.rm = T)
table(property$Brick_Veneer_Type)
table(property$Brick_Veneer_Type[which(property$Brick_Veneer_Area ==0)])
summary(property$Brick_Veneer_Type[which(is.na(property$Brick_Veneer_Area))])
table(property$Brick_Veneer_Area[which(property$Brick_Veneer_Type =="None")])
## Brick_Veneer_Type is unknown and area is also unknown
# lets temporarily replace NA's with mean

property$Brick_Veneer_Area[which(is.na(property$Brick_Veneer_Area))] <- mean(property$Brick_Veneer_Area, na.rm=T)
summary(property$Brick_Veneer_Area)                                


#3.BsmtFinSF1 & BsmtFinSF2 & BsmtUnfSF & Total_Basement_Area

sum(is.na(property$BsmtFinSF1));sum(is.na(property$BsmtFinSF2))
sum(is.na(property$BsmtUnfSF)); sum(is.na(property$Total_Basement_Area))
property[is.na(property$BsmtFinSF1), ]
# the NA's in BsmtFinSF1 & BsmtFinSF2 & BsmtUnfSF & Total_Basement_Area 
# is becoz of NO Basement hence replace it with 0

property$BsmtFinSF1[which(is.na(property$BsmtFinSF1))] <- 0
property$BsmtFinSF2[which(is.na(property$BsmtFinSF2))] <- 0
property$BsmtUnfSF[which(is.na(property$BsmtUnfSF))] <- 0
property$Total_Basement_Area[which(is.na(property$Total_Basement_Area))] <- 0

summary(property$BsmtFinSF1)
summary(property$BsmtFinSF2)
summary(property$BsmtUnfSF)
summary(property$Total_Basement_Area)

# 4.  Underground_Full_Bathroom & Underground_Half_Bathroom

sum(is.na(property$Underground_Full_Bathroom));sum(is.na(property$Underground_Half_Bathroom))
# check data of other variables corresponding to NA value of UFB & UHB
property[is.na(property$Underground_Full_Bathroom), ]

# both the NA are for property with No Basement
# hence lets replace them with 0
property$Underground_Full_Bathroom[which(is.na(property$Underground_Full_Bathroom))] <- 0
property$Underground_Half_Bathroom[which(is.na(property$Underground_Half_Bathroom))] <- 0

summary(property$Underground_Full_Bathroom)
summary(property$Underground_Half_Bathroom)

# 5. Garage_Built_Year & Garage_Size & Garage_Area 

sum(is.na(property$Garage_Built_Year))
# 159 NA's

View(property[is.na(property$Garage_Built_Year), ])
table(property[is.na(property$Garage_Built_Year), "Garage"])
# since 99% of NA's in garage built year is becoz of no garage and 1 % in detached
# lets replace it with 0
table(property[is.na(property$Garage_Size), "Garage"])
table(property[is.na(property$Garage_Area), "Garage"])
## Na's in Garage area and Garage Size is also for No Garage
# lets replace them too with 0

property$Garage_Built_Year[which(is.na(property$Garage_Built_Year))] <- 0
property$Garage_Size[which(is.na(property$Garage_Size))] <- 0
property$Garage_Area[which(is.na(property$Garage_Area))] <- 0

summary(property$Garage_Built_Year)
summary(property$Garage_Size)
summary(property$Garage_Area)


### Treating NA's for catagorical data
# converting factors to character
property <- mutate_if(property,is.factor, as.character)

summary(property[which(sapply(property, is.character))])
colSums(is.na(property[which(sapply(property, is.character))]))

#1. Lane_Type
sum(is.na(property$Lane_Type))
table(property$Lane_Type)

# replacing NA's with No_alley - as per description

property$Lane_Type[which(is.na(property$Lane_Type))] <- "No_alley"
summary(property$Lane_Type)
sum(is.na(property$Lane_Type))

# 2. Utility_Type

sum(is.na(property$Utility_Type))
table(property$Utility_Type)    
# since all utility type except 1 fall in All Pub
# lets replace the NA with AllPub
property$Utility_Type[which(is.na(property$Utility_Type))] <- "AllPub"

sum(is.na(property$Utility_Type))
table(property$Utility_Type)

# 3. Exterior1st & Exterior2nd

sum(is.na(property$Exterior1st)); sum(is.na(property$Exterior2nd))
table(property$Exterior1st); table(property$Exterior2nd)

property$Exterior1st[which(is.na(property$Exterior1st))] <- "VinylSd"
property$Exterior2nd[which(is.na(property$Exterior2nd))] <- "VinylSd"

sum(is.na(property$Exterior1st)); sum(is.na(property$Exterior2nd))
table(property$Exterior1st); table(property$Exterior2nd)

#4. Zoning_Class
table(property$Zoning_Class)
sum(is.na(property$Zoning_Class))

property$Zoning_Class[which(is.na(property$Zoning_Class))] <- "RLD"

table(property$Zoning_Class)
sum(is.na(property$Zoning_Class))

#5.Brick_Veneer_Type

table(property$Brick_Veneer_Type)
sum(is.na(property$Brick_Veneer_Type))
property[is.na(property$Brick_Veneer_Type),"Brick_Veneer_Area"]
# check the BV area of NA's
# all (except 1 NA) has area of 102.23

table(property[(property$Brick_Veneer_Area >102 & property$Brick_Veneer_Area < 103 ), "Brick_Veneer_Type"])

# since max. property with area between 102 and 103 fall under "None" type
# and Na's in BV type has area 102.23, so lets replace the NA's with "None"

property$Brick_Veneer_Type[which(is.na(property$Brick_Veneer_Type))] <- "None"

table(property$Brick_Veneer_Type)
sum(is.na(property$Brick_Veneer_Type))

# 6. Basement_Height & Basement_Condition & Exposure_Level 
#    & BsmtFinType1 & BsmtFinType2
# all NA's in above variables are for property with 
# "No Basement" as per description
# hence replace the NA's with "No_Bsmt"

property$Basement_Height[which(is.na(property$Basement_Height))] <- "No_Bsmt"
property$Basement_Condition[which(is.na(property$Basement_Condition))] <- "No_Bsmt"
property$Exposure_Level[which(is.na(property$Exposure_Level))] <- "No_Bsmt"
property$BsmtFinType1[which(is.na(property$BsmtFinType1))] <- "No_Bsmt"
property$BsmtFinType2[which(is.na(property$BsmtFinType2))] <- "No_Bsmt"

sum(is.na(property$Basement_Height))
sum(is.na(property$Basement_Condition))
sum(is.na(property$Exposure_Level))
sum(is.na(property$BsmtFinType1))
sum(is.na(property$BsmtFinType2))

table(property$Basement_Height)
table(property$Basement_Condition)
table(property$Exposure_Level)
table(property$BsmtFinType1)
table(property$BsmtFinType2)

# 7. Electrical_System

table(property$Electrical_System)
sum(is.na(property$Electrical_System))

# lets replace the single NA with "SBrkr"
property$Electrical_System[which(is.na(property$Electrical_System))] <- "SBrkr"

table(property$Electrical_System)
sum(is.na(property$Electrical_System))

# 8. Kitchen_Quality 

table(property$Kitchen_Quality)
sum(is.na(property$Kitchen_Quality))
property[is.na(property$Kitchen_Quality), "Kitchen_Above_Grade"]

# check corresponding no. of kitchen for NA value in kitchen quality
table(property$Kitchen_Above_Grade)
table(property$Kitchen_Quality[property$Kitchen_Above_Grade == 1])

# since max quality for No. of kitchen "1" is TA
# lets replace the single na with TA
property$Kitchen_Quality[which(is.na(property$Kitchen_Quality))] <- "TA"

table(property$Kitchen_Quality)
sum(is.na(property$Kitchen_Quality))

# 9. Functional_Rate

table(property$Functional_Rate)
sum(is.na(property$Functional_Rate))

# replace with TF as per description
property$Functional_Rate[which(is.na(property$Functional_Rate))] <- "TF"

table(property$Functional_Rate)
sum(is.na(property$Functional_Rate))

# 10. Fireplace_Quality 

sum(is.na(property$Fireplace_Quality))
# as per description, NA is No fireplace
# lets replace the NA's with " No_FP"

property$Fireplace_Quality[which(is.na(property$Fireplace_Quality))] <- "No_FP"

table(property$Fireplace_Quality)
sum(is.na(property$Fireplace_Quality))

# 11. Garage & Garage_Finish_Year & Garage_Quality & Garage_Condition

sum(is.na(property$Garage)); sum(is.na(property$Garage_Finish_Year))
sum(is.na(property$Garage_Quality)); sum(is.na(property$Garage_Condition))

# replacing the NA's with No_Garage as per description

property$Garage[which(is.na(property$Garage))] <- "No_Garage"
property$Garage_Finish_Year[which(is.na(property$Garage_Finish_Year))] <- "No_Garage"
property$Garage_Quality[which(is.na(property$Garage_Quality))] <- "No_Garage"
property$Garage_Condition[which(is.na(property$Garage_Condition))] <- "No_Garage"

sum(is.na(property$Garage)); sum(is.na(property$Garage_Finish_Year))
sum(is.na(property$Garage_Quality)); sum(is.na(property$Garage_Condition))

# 12. Sale Type

sum(is.na(property$Sale_Type))
table(property$Sale_Type)
property[is.na(property$Sale_Type), "Sale_Condition"]
table(property[property$Sale_Condition =="Normal", "Sale_Type"])

# since sale cond. for NA is "Normal"
# and max. Sale Type catagory for "Normal" sale condition is "WD"
# hence replace the NA with "WD"
property$Sale_Type[which(is.na(property$Sale_Type))] <- "WD"

sum(is.na(property$Sale_Type))
table(property$Sale_Type)

## check all na values have been treated

sum(is.na(property))

# convert characters to factors
property <- mutate_if(property, is.character, as.factor)

str(property)
summary(property)


# 90% data as 0 or Null
table(property$Underground_Half_Bathroom)
table(property$Three_Season_Lobby_Area)
table(property$Screen_Lobby_Area)
table(property$Pool_Area)

# transform Sales price using log

property$Sale_Price <- log(property$Sale_Price)

# Create dummy variables for catagorical data

property_factor1 <- names(property[which(sapply(property, is.factor))])
property_dummy <- dummy.data.frame(property, names = property_factor1, sep=".")
colnames(property_dummy)
sort(sapply(property_dummy, mean))# mean should not be 0

# check correlation using only Sale_Price of Train data

cor_property <- cor(property_dummy[1:1459, ], use = "complete.obs")

cor_prop_sort <- as.matrix(sort(cor_property[,'Sale_Price'], decreasing = TRUE))

quantile(abs(cor_prop_sort), seq(0,1,0.02))

# check variables with correlation greater than 0.4

cor_prop_sort1 <- names(cor_prop_sort[which(abs(cor_prop_sort)>=0.4), ] )
cor_prop_sort1

# we will remove values with co-relation less tha 0.4

property_dummy_new<- property_dummy[ ,cor_prop_sort1]
summary(property_dummy_new)


# splitting train and test data again

Train_new <- property_dummy_new[1:1459,]
Test_new <- property_dummy_new[1460:2918,]

sum(is.na(Train_new))

## Model Building using LR

Model_1 <- lm(Sale_Price~., data = Train_new)
summary(Model_1)
# check multicolinerity using vif
sort(vif(Model_1))

# lets use step function for best model using values of AIC

Model_2 <- step(Model_1, direction = "both")
summary(Model_2)

sort(vif(Model_2))
# vif of "Kitchen_Quality.Gd" is much higher 
# lets remove and check the model

Model_3 <- lm(formula = Sale_Price ~ Grade_Living_Area + Garage_Size + Total_Basement_Area + 
                First_Floor_Area + Construction_Year + Remodel_Year + Rooms_Above_Grade + 
                Fireplaces + Heating_Quality.Ex + Basement_Height.Ex + BsmtFinType1.GLQ + 
                Overall_Material.8 + Garage.Attchd + Kitchen_Quality.Ex + 
                Fireplace_Quality.No_FP + Kitchen_Quality.TA, 
              data = Train_new)
summary(Model_3)
sort(vif(Model_3))

# Fireplaces vif is still higher than 5
# lets remove and remodel
Model_4 <- lm(formula = Sale_Price ~ Grade_Living_Area + Garage_Size + Total_Basement_Area + 
                First_Floor_Area + Construction_Year + Remodel_Year + Rooms_Above_Grade + 
                Heating_Quality.Ex + Basement_Height.Ex + BsmtFinType1.GLQ + 
                Overall_Material.8 + Garage.Attchd + Kitchen_Quality.Ex + 
                Fireplace_Quality.No_FP + Kitchen_Quality.TA, data = Train_new)
summary(Model_4)
sort(vif(Model_4))
# now, vif of remaining variable is below 5.

# lets check p-value (remove variables with p-value>0.05)
summary(Model_4)

# p value of "Rooms_Above_Grade" & "First_Floor_Area" are > 0.05
# lets remove the two variable and remodel

Model_5 <- lm(formula = Sale_Price ~ Grade_Living_Area + Garage_Size + Total_Basement_Area + 
                Construction_Year + Remodel_Year + 
                Heating_Quality.Ex + Basement_Height.Ex + BsmtFinType1.GLQ + 
                Overall_Material.8 + Garage.Attchd + Kitchen_Quality.Ex + 
                Fireplace_Quality.No_FP + Kitchen_Quality.TA, data = Train_new)
summary(Model_5)
sort(vif(Model_5))

# All variables have vif below "5" and p-value <0.05

# lets evaluate our model

par(mfrow=c(2,2))
plot(Model_5)

# assumption test

# check auto corelation
durbinWatsonTest(Model_5)  
## good since D_W statistics is less than 2, i.e no auto corelation

# normality of error (to check normality of error)
hist(residuals(Model_5))

# homosedasticity
plot(Train_new$Sale_Price, residuals(Model_5))

## check cooks distance 
cooksd=CookD(Model_5)

# here 2 data points are impacting our model # 524 & 1299
# lets remove the 2 data points and remodel

Train_new <- Train_new[-c(524,1299),]

# lets remodel using latest train data

Model_6 <- lm(formula = Sale_Price ~ Grade_Living_Area + Garage_Size + Total_Basement_Area + 
                Construction_Year + Remodel_Year + 
                Heating_Quality.Ex + Basement_Height.Ex + BsmtFinType1.GLQ + 
                Overall_Material.8 + Garage.Attchd + Kitchen_Quality.Ex + 
                Fireplace_Quality.No_FP, data = Train_new)
summary(Model_6)
sort(vif(Model_6))

# # lets re- evaluate our model
par(mfrow=c(2,2))
plot(Model_6)

# assumption test

# check auto corelation
durbinWatsonTest(Model_6) 
# D-W STats = 1.95 which is < 2
# i.e no auto co-relation


# normality of error (to check normality of error)
hist(residuals(Model_6))

# homosedasticity
plot(Train_new$Sale_Price, residuals(Model_6))

## check cooks distance 
cooksd=CookD(Model_6)

# Final Model 
Model_final <- Model_6

# lets validate our model

Train_new_check <- predict(Model_final, newdata = Train_new, type = "response")

# check correlation
cor(Train_new_check, Train_new$Sale_Price) 
# 0.92
cor(Train_new_check, Train_new$Sale_Price)^2
# or alternate method
cat(summary(Model_final)$r.squared)

# R-squared =  0.85

## RMSE check

RMSE(Train_new_check, Train_new$Sale_Price)
# or alternate method
sqrt(mean((Train_new$Sale_Price - Train_new_check)^2))

# RMSE= 0.15

# Predict Sale_Price of Test Data

Test_new$Sale_Price <- predict(Model_final, newdata = Test_new, type = "response")

# converting the transformed Sale_Price back to original
Train_new$Sale_Price <- exp(Train_new$Sale_Price)
Test_new$Sale_Price <-  exp(Test_new$Sale_Price)

head(Train_new$Sale_Price)
head(Test_new$Sale_Price)

## save the data as per your convenient name and location ##