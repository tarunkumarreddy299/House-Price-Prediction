###--------------------------------------------------------------------------------------####
##-------------------Predicting Sale price of a house using Regression-------------------####
###--------------------------------------------------------------------------------------####

## Reading Data
data = read.csv("D:/College Stuff/Academics SaiU/Stats for Data Science/Stats For Data Science/Stats for DS Project/House Prices Data.csv")
str(data) ##Structure of training data set 
summary(data) ##Summary of the data
colnames(data) ##Column names

##---------------------Cleaning the data-----------------------------------------------###
data = subset(data, select = -c(Id)) 

## Identifying the null values in the data set 
for(i in names(data)){
  cat(i,":", sum(is.na(data[[i]])),"\n")
}

## Dropping columns that has a lot of null values
data = subset(data, select = 
                -c(PoolQC,Fence,MiscFeature,Alley,FireplaceQu))

## Dropping rows with null values
data = na.omit(data)

## Fetching non-numeric-columns from the data frame
non_numeric_columns = names(data)[sapply(data, function(x) !is.numeric(x))]

## Printing non-numeric-columns and count of unique values 
for(i in non_numeric_columns){
  cat(i,":", length(unique(data[[i]])),"\n")
}

## Dropping columns with zero or one category 
data = subset(data, select = 
                -c(Utilities))


##---------------------Modeling probability of sales price--------------------------------###

## Plotting sales price
library(ggplot2)
ggplot(data = data) +
  geom_histogram(mapping = aes(x = SalePrice))


## Fitting Gamma
hist(data$SalePrice, col = "yellow", probability = T, 
     main = "Probability of Sales Price" )
alpha = (mean(data$SalePrice^2))/var(data$SalePrice)
theta = var(data$SalePrice)/mean(data$SalePrice)
salesprice = seq(min(data$SalePrice),max(data$SalePrice))
density_sales = dgamma(salesprice,shape=alpha,scale = theta)
lines(salesprice,density_sales,col="blue", lwd=2)

## Fitting Normal
hist(data$SalePrice, col = "yellow", probability = T, 
     main = "Probability of Sales Price" )
mu = mean(data$SalePrice)
std = sd(data$SalePrice)
salesprice = seq(min(data$SalePrice),max(data$SalePrice))
density_sales = dnorm(salesprice,mean = mu,sd = std)
lines(salesprice,density_sales,col="red", lwd=2)



##---------------------------Exploratory Data Analysis-----------------------------------------##

## Box plots of target variable(Sales price) across the categorical variables 
boxplot(data$SalePrice ~ data$MSSubClass,
        col= 2:16, xlab = "Sub class of the house", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$MSZoning,
        col= 2:6, xlab = "General zoning classification", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Street, col= 2:4, 
        xlab = "Type of road access to property", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$LotShape, col= 2:5, 
        xlab = "General shape of property", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$LotShape, col= 2:5, 
        xlab = "General shape of property", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$LandContour, col= 2:5, 
        xlab = "Flatness of the property", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$LotConfig, col= 2:6, 
        xlab = "Lot configuration", 
        ylab = "Sale Price of the house")


boxplot(data$SalePrice ~ data$LandSlope, col= 2:4, 
        xlab = "Slope of property", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Neighborhood, col= 2:26, 
        xlab = "Physical locations within Ames city limits", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Condition1, col= 2:10, 
        xlab = "Proximity to various conditions", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Condition2, col= 2:10, 
        xlab = "Proximity to various conditions (if more than one is present)", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$BldgType, col= 2:5, 
        xlab = "Type of dwelling", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$HouseStyle, col= 2:5, 
        xlab = "Style of dwelling", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$OverallQual, col= 2:11,
        xlab = "Rates the overall material and finish of the house", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$OverallCond, col= 2:11,
        xlab = "Rates the overall condition of the house", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$YearBuilt, col= 2:100,
        xlab = "Year Built", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$YearRemodAdd, col= 2:100,
        xlab = "Year Remodeled", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$RoofStyle, col= 2:7,
        xlab = "Type of roof", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$RoofMatl, col= 2:8,
        xlab = "Roof material", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Exterior1st, col= 2:15,
        xlab = "Exterior covering on house", 
        ylab = "Sale Price of the house")

boxplot(data$SalePrice ~ data$Exterior2nd, col= 2:17,
        xlab = "Exterior covering on house (if more than one material)", 
        ylab = "Sale Price of the house")


## Plotting scatter plots between Sale price and numeric columns  
plot(log(data$LotArea), log(data$SalePrice), 
     xlab = "Lot Area", ylab = "Sale Price")

ggplot(data=data, aes(x=data$LotArea, y=data$SalePrice)) + geom_point()+
  labs(title = "Lot Area vs Sale Price on normal scale", x="Lot Area", y = "Sales Price") 

ggplot(data=data, aes(x=log(data$LotArea), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Lot Area vs Sale Price on Log Scale", x="Lot Area", y = "Sales Price")

ggplot(data=data, aes(x=log(data$LotFrontage), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Lot Frontage vs Sale Price on Log Scale", x="Lot Area", y = "Sales Price")

## Calculating correlation matrix
correlation_matrix = cor(numeric_data)
cor_long <- reshape2::melt(correlation_matrix, na.rm = TRUE)

## Plotting Heat Map
ggplot(data = cor_long, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

##Plotting Scatter plots with high correlation 
ggplot(data=data, aes(x=log(data$TotalBsmtSF), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Total square feet of basement area vs Sale Price on Log Scale", x="Total square feet of basement area", y = "Sales Price")

ggplot(data=data, aes(x=log(data$GrLivArea), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Living area square feet vs Sale Price on Log Scale", x="Living area square feet", y = "Sales Price")

ggplot(data=data, aes(x=log(data$GarageYrBlt), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Year garage was built vs Sale Price on Log Scale", x="Year garage was built", y = "Sales Price")

ggplot(data=data, aes(x=log(data$GarageArea), 
                      y=log(data$SalePrice)))+ geom_point()+
  labs(title = "Year garage was built vs Sale Price on Log Scale", x="Year garage was built", y = "Sales Price")


##-----------------------Preparing the Data for modelling-----------------------------------------##

## Encoding the categorical values using Label Encoding
## Fetching non-numeric-columns from the data frame
non_numeric_columns = names(data)[sapply(data, function(x) !is.numeric(x))]

## Printing non-numeric-columns and count of unique values 
for(i in non_numeric_columns){
  cat(i,":", length(unique(data[[i]])),"\n")
}

## Converting non-numeric-columns to numeric columns (Label encoding)
for (col in non_numeric_columns) {
  data[[col]] = as.integer(factor(data[[col]]))
}


## Splitting the data into train, test data
library(caret)

set.seed(42)  
train_indices <- createDataPartition(data$SalePrice, p = 0.8, list = FALSE)

# Create the training set
train_data <- data[train_indices, ]

# Create the testing set
test_data <- data[-train_indices, ]
nrow(train_data)
nrow(test_data)



##--------------------------Building the Regression model---------------------------------###
lm_model <- lm(SalePrice ~ ., data = train_data)
summary(lm_model)

actual_values <- test_data$SalePrice
test_predictions <- predict(lm_model, newdata = test_data)
residuals <- actual_values - test_predictions
sse <- sum(residuals^2)
sst <- sum((actual_values - mean(actual_values))^2)
r_squared <- 1 - (sse / sst)


##--------------------------Building the Step Regression model---------------------------------###

lm_model <- step(lm(log(SalePrice) ~ ., data = data))
summary(lm_model)
