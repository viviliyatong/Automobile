#load libraries
library(gbm)
library(randomForest)
library(tree) 
library(rpart) 
library(rpart.plot)
library(dplyr)
library(ggfortify)
library(moments)
library(caret)
library(ggplot2)
library(car)
require(psych)
library(corrplot)
require(lmtest)
require(plm)
library(boot)
library(caTools)
library(stringr)
library(RColorBrewer)

# Load dataset
automobile_df = read.csv("C:/Users/liyat/Downloads/Dataset 5 — Automobile data.csv")
attach(automobile_df)

##Data Pre-processing
#overview
table(normalized.losses)
summary(normalized.losses)
table(price)
summary(price)
table(bore)
summary(bore)
table(num.of.doors)
summary(num.of.doors)
table(stroke)
summary(stroke)
table(horsepower)
summary(horsepower)
table(peak.rpm)
summary(peak.rpm)
table(num.of.cylinders)
summary(num.of.cylinders)

#handle "?" value
automobile_df=mutate(automobile_df, normalized.losses=ifelse(normalized.losses == "?", NA, normalized.losses))
automobile_df=mutate(automobile_df, price=ifelse(price == "?", NA, price))
automobile_df=mutate(automobile_df, bore=ifelse(bore == "?", NA, bore))
automobile_df=mutate(automobile_df, num.of.doors=ifelse(num.of.doors == "?", NA, num.of.doors))
automobile_df=mutate(automobile_df, stroke=ifelse(stroke == "?", NA, stroke))
automobile_df=mutate(automobile_df, horsepower=ifelse(horsepower == "?", NA, horsepower))
automobile_df=mutate(automobile_df, peak.rpm=ifelse(peak.rpm == "?", NA, peak.rpm))

#check number of null values in df
print(colSums(is.na(automobile_df)))

#drop null values - except normalized.losses
automobile_df_dropped=automobile_df[complete.cases(automobile_df$num.of.doors, automobile_df$bore, 
                                                automobile_df$stroke, automobile_df$horsepower,
                                                automobile_df$peak.rpm, automobile_df$price), ]
print(colSums(is.na(automobile_df_dropped)))

#convert normalized.losses, bore, stroke, horsepower, peak.rpm, price into numerical variables
automobile_df_dropped$normalized.losses = as.numeric(automobile_df_dropped$normalized.losses)
automobile_df_dropped$bore = as.numeric(automobile_df_dropped$bore)
automobile_df_dropped$stroke = as.numeric(automobile_df_dropped$stroke)
automobile_df_dropped$horsepower = as.numeric(automobile_df_dropped$horsepower)
automobile_df_dropped$peak.rpm = as.numeric(automobile_df_dropped$peak.rpm)
automobile_df_dropped$price = as.numeric(automobile_df_dropped$price)

#num.of.cylinders as numerical
automobile_df_dropped$num.of.cylinders = case_when(
  automobile_df_dropped$num.of.cylinders == "two" ~ 2,
  automobile_df_dropped$num.of.cylinders == "three" ~ 3,
  automobile_df_dropped$num.of.cylinders == "four" ~ 4,
  automobile_df_dropped$num.of.cylinders == "five" ~ 5,
  automobile_df_dropped$num.of.cylinders == "six" ~ 6,
  automobile_df_dropped$num.of.cylinders == "eight" ~ 8,
  automobile_df_dropped$num.of.cylinders == "twelve" ~ 12)

attach(automobile_df_dropped)

#################
###Histogram#####
#################
# Set up theme for the plot to avoid repetitive customization for each element
custom_theme <- theme_classic(base_family = "Calibri") +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Create histogram - symboling
ggplot(automobile_df_dropped, aes(x = symboling)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 1, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(-2, 3, 1),labels = seq(-2, 3, 1), name = "Symboling") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Symboling")+custom_theme

# Create histogram - normalized.losses
ggplot(automobile_df_dropped, aes(x = normalized.losses)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 10, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
  breaks = seq(0, 260, 50),labels = seq(0, 260, 50), name = "Normalized Loss") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Normalized Loss")+custom_theme

# Create histogram - wheel.base
ggplot(automobile_df_dropped, aes(x = wheel.base)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 2, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 120, 5),labels = seq(0, 120, 5), name = "Wheel Base") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Wheel Base")+custom_theme

# Create histogram - length
ggplot(automobile_df_dropped, aes(x = length)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 5, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 210, 20),labels = seq(0, 210, 20), name = "Length") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Length")+custom_theme

# Create histogram - width
ggplot(automobile_df_dropped, aes(x = width)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 1, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 75, 5),labels = seq(0, 75, 5), name = "Width") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Width")+custom_theme

# Create histogram - height
ggplot(automobile_df_dropped, aes(x = height)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 1, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 60, 5),labels = seq(0, 60, 5), name = "Height") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Height")+custom_theme

# Create histogram - curb.weight
ggplot(automobile_df_dropped, aes(x = curb.weight)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 200, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 4100, 500),labels = seq(0, 4100, 500), name = "Curb Weight") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Curb Weight")+custom_theme

# Create histogram - horsepower
ggplot(automobile_df_dropped, aes(x = horsepower)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 15, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 270, 50),labels = seq(0, 270, 50), name = "Horsepower") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Horsepower")+custom_theme

# Create histogram - city.mpg
ggplot(automobile_df_dropped, aes(x = city.mpg)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 3, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 50, 10),labels = seq(0, 50, 10), name = "City MPG") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of City MPG")+custom_theme

# Create histogram - highway.mpg
ggplot(automobile_df_dropped, aes(x = highway.mpg)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 3, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 55, 10),labels = seq(0, 55, 10), name = "Highway MPG") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Highway MPG")+custom_theme

# Create histogram - price
ggplot(automobile_df_dropped, aes(x = price)) + geom_histogram(
  aes(y = ..count.., fill = ..count..),binwidth = 2000, color = "white",show.legend = FALSE) +
  scale_fill_gradient(low = "#ADD8E6", high = "#4169E1") + scale_x_continuous(
    breaks = seq(0, 45500, 5000),labels = seq(0, 45500, 5000), name = "Price") +
  scale_y_continuous(name = "Frequency")+ggtitle("Frequency Distribution of Price")+custom_theme


#################
####BoxPlots#####
#################
automobile_df_dropped_2=automobile_df_dropped[!is.na(automobile_df_dropped$normalized.losses), ]

# Set up theme for the plot to avoid repetitive customization for each element
custom_theme2 <- theme_classic(base_family = "Avenir") +
  theme(plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(size = 9))

# Boxplot - num.of.doors
colorsets = c("#ADD8E6","#4169E1") 
ggplot(automobile_df_dropped, aes(x=num.of.doors, y=symboling, fill=num.of.doors)) + 
  geom_boxplot(alpha=0.8) + theme_classic() +
  labs(fill='Number of Doors') +
  scale_fill_manual(values = colorsets)+
  ggtitle("Symboling vs Number of Doors")+ 
  ylab("Symboling") + custom_theme2
ggplot(automobile_df_dropped_2, aes(x=num.of.doors, y=normalized.losses, fill=num.of.doors)) +
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colorsets)+  
  labs(fill='Number of Doors') +
  ggtitle("Normalized Loss vs Number of Doors") +
  ylab("Normalized Loss") +
  custom_theme2

# Boxplot - body.style
colorsets = c("#ADD8E6","#9FCBE2","#82ADD9","#4169E1","#2C45BA") 
ggplot(automobile_df_dropped, aes(x=body.style, y=symboling, fill=body.style)) +
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colorsets)+  
  labs(fill='Body Style') +
  ggtitle("Symboling vs Body Style") +
  ylab("Symboling") +
  custom_theme2
ggplot(automobile_df_dropped_2, aes(x=body.style, y=normalized.losses, fill=body.style)) +
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colorsets)+  
  labs(fill='Body Style') +
  ggtitle("Normalized Loss vs Body Style") +
  ylab("Normalized Loss") +
  custom_theme2

# Boxplot - drive.wheels
colorsets = c("#ADD8E6","#82ADD9","#4169E1") 
ggplot(automobile_df_dropped, aes(x=drive.wheels, y=symboling, fill=drive.wheels)) + 
  geom_boxplot(alpha=0.8) + theme_classic() +
  labs(fill='Drive Wheels') +
  scale_fill_manual(values = colorsets)+
  ggtitle("Symboling vs Drive Wheels")+ 
  ylab("Symboling") + custom_theme2
ggplot(automobile_df_dropped_2, aes(x=drive.wheels, y=normalized.losses, fill=drive.wheels)) +
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colorsets)+  
  labs(fill='Drive Wheels') +
  ggtitle("Normalized Loss vs Drive Wheels") +
  ylab("Normalized Loss") +
  custom_theme2

###Model Development
##Predict Symboling
#factorize 'symboling'
automobile_df_dropped$symboling=as.factor(automobile_df_dropped$symboling)
attach(automobile_df_dropped)
## Random Forest
#original model...starting with all relevant variables
set.seed(1)
myforest_1=randomForest(symboling~num.of.doors+body.style+drive.wheels+wheel.base+length+width
                        +height+curb.weight+horsepower+city.mpg+highway.mpg+price,ntree=500,
                        data=automobile_df_dropped,importance=TRUE,na.action=na.omit)
myforest_1
importance(myforest_1)

#adjust model based on feature importance
set.seed(1)
myforest_2=randomForest(symboling~num.of.doors+body.style+wheel.base+length+width+height+curb.weight
                        +horsepower+city.mpg+price,ntree=500,data=automobile_df_dropped,importance=TRUE,
                        na.action=na.omit)
myforest_2

#fine tune model
for (i in seq(100, 1000, by=100))  {
  set.seed(1)
  rfm=randomForest(symboling~num.of.doors+body.style+wheel.base+length+width+height+curb.weight
                   +horsepower+city.mpg+price,ntree=i,data=automobile_df_dropped,importance=TRUE,
                   na.action=na.omit)
  oob_error=rfm$err.rate[nrow(rfm$err.rate), "OOB"]
  print(paste("ntree =", i, ", OOB Error Rate =", oob_error))
}

#final model
set.seed(1)
myforest_symboling=randomForest(symboling~num.of.doors+body.style+wheel.base+length+width+height+curb.weight
                                +horsepower+city.mpg+price,ntree=500,data=automobile_df_dropped,importance=TRUE,
                                na.action=na.omit)
myforest_symboling #12.44% error rate
importance(myforest_symboling)

##Predict Normalized Loss
#######
##PCA##
#######
#select numerical column to run pca
numerical_columns=automobile_df_dropped[sapply(automobile_df_dropped, is.numeric)]

#explore data correlations with pca
attach(numerical_columns) 
num_labels=numerical_columns[,c(1)] 
num_vars=numerical_columns[,c(1:16)] 
pca_num=prcomp(na.omit(num_vars), scale=TRUE) 
autoplot(pca_num, data = na.omit(num_vars), loadings = TRUE, col="grey", loadings.label = TRUE)

#Model Selection
#Random Forest
#original model...starting with all relevant variables
set.seed(1) 

rfm_1=randomForest(normalized.losses~num.of.doors+body.style+drive.wheels+wheel.base+length+height+horsepower+city.mpg
                   +highway.mpg+price+width+curb.weight,ntree=500,data=automobile_df_dropped,importance=TRUE,na.action=na.omit) 
rfm_1

#Gradient Boosting
automobile_df_dropped_2$num.of.doors=as.factor(automobile_df_dropped_2$num.of.doors)
automobile_df_dropped_2$body.style=as.factor(automobile_df_dropped_2$body.style)
automobile_df_dropped_2$drive.wheels=as.factor(automobile_df_dropped_2$drive.wheels)
attach(automobile_df_dropped_2)
#original model...starting with all relevant variables
set.seed(1) 
gbm_1=gbm(normalized.losses~num.of.doors+body.style+drive.wheels+wheel.base+length+height+horsepower+city.mpg
          +highway.mpg+price+width+curb.weight,data=automobile_df_dropped_2,distribution="gaussian",n.trees=10000,interaction.depth=4) 
summary(gbm_1)
predicted_score=predict(gbm_1, newdata=automobile_df_dropped_2, n.trees=10000) 
mean((predicted_score-normalized.losses)^2)

#adjust model based on relative influence and pca result
set.seed(1) 
gbm_2=gbm(normalized.losses~num.of.doors+body.style+wheel.base+length+height+horsepower+city.mpg+highway.mpg+price
          +width+curb.weight,data=automobile_df_dropped_2,distribution="gaussian",n.trees=10000,interaction.depth=4) 
summary(gbm_2)
predicted_score=predict(gbm_2, newdata=automobile_df_dropped_2, n.trees=10000) 
mean((predicted_score-normalized.losses)^2)

#fine tuning
min_mse=Inf
min_i=0
min_j=0
for (i in seq(500, 10000, by=500)) {
  for (j in 1:10) {
    set.seed(1)
    gbm=gbm(normalized.losses~num.of.doors+body.style+wheel.base+length+height+horsepower+city.mpg+highway.mpg+price
            +width+curb.weight,data=automobile_df_dropped_2,distribution="gaussian",n.trees=i,interaction.depth=j)
    #check model performance
    predicted_score=predict(gbm, newdata=automobile_df_dropped_2, n.trees=i) 
    mse=mean((predicted_score-normalized.losses)^2)
    if (mse < min_mse) {
      min_mse = mse
      min_i = i
      min_j = j
    }
  }
}

min_mse
min_i
min_j

#final model
set.seed(1) 
mygbm_loss=gbm(normalized.losses~num.of.doors+body.style+wheel.base+length+height+horsepower+city.mpg+highway.mpg+price
               +width+curb.weight,data=automobile_df_dropped_2,distribution="gaussian",n.trees=8000,interaction.depth=5) 
summary(mygbm_loss)
predicted_score=predict(mygbm_loss, newdata=automobile_df_dropped_2, n.trees=8000) 
mean((predicted_score-normalized.losses)^2)

#Prediction

predicted_symboling=predict(myforest_symboling,data.frame(num.of.doors='four', body.style='sedan', wheel.base=116.5,
                                                          length=196, width=77, height=56.5, curb.weight=4600, 
                                                          horsepower=362, city.mpg=88, price=57400))

predicted_loss=predict(mygbm_loss,data.frame(num.of.doors='four', body.style='sedan', wheel.base=116.5,length=196, 
                                             height=56.5, horsepower=362, city.mpg=88, highway.mpg=90, price=57400,
                                             width=77, curb.weight=4600))

predicted_symboling
predicted_loss
