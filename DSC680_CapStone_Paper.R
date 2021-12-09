---
title: "DSC680_CapStone_Paper"
author: "Sonam Chhabra"
date: "11/14/2021"
output:
  pdf_document: default
  html_document: default
  word_document: default
---
# Importing the Data -  

```{r}
OC_Preprocessed_Data <- read.delim("~/UTICA/DSC680-Capstone/Files/OC_Preprocessed_DataFile.txt")
df<- OC_Preprocessed_Data
dim(df)
```

# Preprocessing the Data
```{r}
library(dplyr)
df<- df %>%
  filter(ROLL_SECTION==1, VAL_USEABLE==1,ARMS_LENGTH==1)
dim(df)
```


```{r}
missing_value <- any(is.na(df[]))
if (missing_value == FALSE) {
  print("There are No missing values")
  } else {print("There are missing values")
  }
class(df)
dim(df)
```


```{r}
library(tidyverse)
library(dplyr)
#View(df_clean)
df_clean <- df %>%
  select(SWIS_CO,SWIS_VG,SWIS_TOWN,PRINT_KEY,TOTAL_AV,SCH_CODE,SALE_PRICE,AV_SP_RATIO,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_HALF_BATHS,NBR_FULL_BATHS,LOC_ST_NBR,MAIL_ST_NBR,YR_BUILT)
View(df_clean)
dim(df_clean)
str(df_clean)
```



```{r}
#df_clean<- na.omit(df)
#missing_value <- any(is.na(df_clean[]))
#if (missing_value == FALSE) {
#  print("There are No missing values")
 # } else {print("There are missing values")
#  }
#dim(df_clean)
```
```{r}
library(stringr)
df_clean$SWIS_INFO <- str_c(df_clean$SWIS_CO,df_clean$SWIS_TOWN,df_clean$SWIS_VG)
View(df_clean)
```

```{r}
df_clean$SCH_CODE<- as.character(df_clean$SCH_CODE)
df_clean$SCH_CODE[df_clean$SCH_CODE==306801]<- "Westmoreland"
df_clean$SCH_CODE[df_clean$SCH_CODE==213803]<- "Poland"
df_clean$SCH_CODE[df_clean$SCH_CODE==307002]<- "Yorkville"
df_clean$SCH_CODE[df_clean$SCH_CODE==212402]<- "Newport"
df_clean$SCH_CODE[df_clean$SCH_CODE==301300]<- "Rome"
df_clean$SCH_CODE[df_clean$SCH_CODE==307001]<- "Oriskany"
df_clean$SCH_CODE[df_clean$SCH_CODE==302601]<- "Boonville"
df_clean$SCH_CODE[df_clean$SCH_CODE==305801]<- "Holland Patent"
df_clean$SCH_CODE[df_clean$SCH_CODE==306000]<- "Verona"
df_clean$SCH_CODE[df_clean$SCH_CODE==304001]<- "Clinton"
df_clean$SCH_CODE[df_clean$SCH_CODE==304801]<- "New Hartford"
df_clean$SCH_CODE[df_clean$SCH_CODE==305201]<- "Remsen"
df_clean$SCH_CODE[df_clean$SCH_CODE==303001]<- "Camden"
df_clean$SCH_CODE[df_clean$SCH_CODE==251200]<- "Oneida"
df_clean$SCH_CODE[df_clean$SCH_CODE==301600]<- "Utica"
df_clean$SCH_CODE[df_clean$SCH_CODE==304804]<- "New York Mills"
df_clean$SCH_CODE[df_clean$SCH_CODE==215401]<- "Old Forge"
df_clean$SCH_CODE[df_clean$SCH_CODE==353201]<- "Central Square"

View(df_clean)
```


```{r}
df_clean$LOC_ST_NBR<- as.character(df_clean$LOC_ST_NBR)
df_clean$MAIL_ST_NBR<- as.character(df_clean$MAIL_ST_NBR)
df_clean$RENTAL_PROPERTY <- ifelse((df_clean$LOC_ST_NBR != df_clean$MAIL_ST_NBR),0 ,1)
View(df_clean)
```

```{r}
df_clean$NBR_FULL_BATHS<- as.character(df_clean$NBR_FULL_BATHS)
df_clean$NBR_HALF_BATHS <- as.character(df_clean$NBR_HALF_BATHS)
df_clean$NBR_FULL_BATHS<- as.numeric(df_clean$NBR_FULL_BATHS)
df_clean$NBR_HALF_BATHS <- as.numeric(df_clean$NBR_HALF_BATHS)
df_clean$NBR_BATHS <- df_clean$NBR_FULL_BATHS + df_clean$NBR_HALF_BATHS
dim(df_clean)
```

```{r}
library(tidyverse)
library(dplyr)
#View(df_clean)
df_clean <- df_clean %>%
  select(SWIS_INFO,PRINT_KEY,TOTAL_AV,SCH_CODE,SALE_PRICE,AV_SP_RATIO,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_BATHS,RENTAL_PROPERTY,YR_BUILT)
View(df_clean)
```


```{r}
dim(df_clean)
```

```{r}
str(df_clean)
```

```{r}
head(df_clean)
```

# Partitioning data into train and test

```{r}
set.seed(1234)
attach(df_clean)
pd<- sample(2,nrow(df_clean),replace=TRUE,prob=c(0.8,0.2))
training <- df_clean[pd==1,]
testing <- df_clean[pd==2,]
View(training)
View(testing)
```
# Correlation Table

```{r}
library(psych)
library(tidyverse)
library(dplyr)
#View(df_clean)
training_cor <- training %>%
  select(TOTAL_AV,SALE_PRICE,AV_SP_RATIO,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_BATHS)
#View(training_cor)
#dim(training_cor)
#str(training_cor)

round(cor(training_cor),2)

```



#Bar Plots - single categorical 
```{r}
df_clean%>%
  ggplot(aes(fct_infreq(SCH_CODE)))+geom_bar(fill="orange")+coord_flip( )+ 
   labs(x="School District",
       y="Count of observations for each school district",
       title="Count of observations by school districts")
```

#Histogram - single numerical by school districts

```{r}
df_clean %>%
  ggplot(aes(SALE_PRICE/1000))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Sale Price in 100k dollars",
       y="Count of observations for sale price by school district",
       title="Count of observations for sale price by school districts")
#scale_y_continuous(labels = comma)

```
```{r}
df_clean %>%
  ggplot(aes(AV_SP_RATIO))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Assessed value to sale price ratio",
       y="Count of observations for the ratio by school district",
       title="Count of observations for assessed value to sale price ratio by school districts")
#scale_y_continuous(labels = comma)

```




```{r}
df_clean %>%
  ggplot(aes(NBR_BEDROOMS))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Number of Bedrooms",
       y="Count of observations for number of bedrooms for parcels",
       title="Count of observations for number of bedrooms for parcels by school district")
#scale_y_continuous(labels = comma)

```


```{r}
df_clean %>%
  ggplot(aes(NBR_BATHS))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Number of Baths",
       y="Count of observations for number of bathrooms for parcels",
       title="Count of observations for number of bathrooms for parcels by school district")
#scale_y_continuous(labels = comma)

```

```{r}
df_clean %>%
  ggplot(aes(KITCHEN_QUALITY))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Kitchen quality for the parcels",
       y="Count of observations for the kitchen quality for parcels",
       title="Count of observations for the kitchen quality of the parcels by school district")
#scale_y_continuous(labels = comma)

```


```{r}
df_clean %>%
  ggplot(aes(OVERALL_COND))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Overall condition of the parcels",
       y="Count of observations for the overall condition of the parcels",
       title="Count of observations for the overall condition of the parcels by school district")
#scale_y_continuous(labels = comma)

```

# Dot plots for 2 or more numerical variables 

```{r}
options(scipen = 999)
p1<-  ggplot(data=df_clean,aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Total assessed value of the parcel",
       y="Sale price of the parcel",
       title="Plot for total assessed value and sale price by number of bedrooms")+
  theme_minimal()
p1

```

```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price of the parcel",
       title="Plot for land area and sale price by number of bedrooms")+
  theme_minimal()
 # scale_x_continuous(labels = comma)
```

```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=NBR_BATHS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Total assessed value of the parcel",
       y="Sale price",
       title="Plot for assessed value and sale price by number of bathrooms")+
  theme_minimal()

```


```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BATHS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price",
       title="Plot for land area and sale price by number of bathrooms")+
  theme_minimal()

```
```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=OVERALL_COND))+
  geom_smooth(method = lm,se=F)+
  labs(x="Total assessed value of the parcel",
       y="Sale price",
       title="Plot for assessed value and sale price by overall condition")+
  theme_minimal()

```


```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=OVERALL_COND))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price",
       title="Plot for land area and sale price by overall condition")+
  theme_minimal()

```

```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=KITCHEN_QUALITY))+
  geom_smooth(method = lm,se=F)+
  labs(x="Total assessed value of the parcel",
       y="Sale price",
       title="Plot for assessed value and sale price by kitchen quality")+
  theme_minimal()

```

```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=KITCHEN_QUALITY))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price",
       title="Plot for land area and sale price by kitchen quality")+
  theme_minimal()

```




# Scatter Plots 

```{r}
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggrepel)
ggplot(data=training,aes(x=OVERALL_COND,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_grid(~SCH_CODE)
ggplot(data=training,aes(x=NBR_BATHS,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_grid(~SCH_CODE)
ggplot(data=training,aes(x=NBR_BEDROOMS,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_grid(~SCH_CODE)
q<- ggplot(data=training,aes(x=KITCHEN_QUALITY,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_wrap(~SCH_CODE,nrow = 4,scales = "free_x")
q + scale_y_continuous(labels = comma)
P1<- ggplot(data=training,aes(x=TOTAL_AV,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_wrap(~SCH_CODE,nrow = 7,scales = "free_x")
P1 + scale_x_continuous(labels = comma)
P2<- ggplot(data=training,aes(x=AV_SP_RATIO,y=SALE_PRICE,color=SCH_CODE))+geom_point()+facet_wrap(~SCH_CODE,nrow = 7,scales = "free_x")
P2 + scale_x_continuous(labels = comma)
```



# Box and Whisker plots 
```{r}
options(scipen = 999)
df_clean %>%
  ggplot(aes(SCH_CODE,SALE_PRICE))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School district",
       y="Sale price",
       title="Plot for the sale price by school district")
```


```{r}
options(scipen = 999)
df_clean %>%
  ggplot(aes(SCH_CODE,AV_SP_RATIO))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School district",
       y="Assessed value to sale price ratio",
       title="Plot for the assesssed value to sale price ratio by school district")

```
```{r}
options(scipen = 999)
df_clean %>%
  ggplot(aes(SCH_CODE,SFLA))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School district",
       y="Land area",
       title="Plot for the land area by school district")

```



# Identifying Outliers

```{r}
library(dplyr)
attach(training)
training_withoutoutlier<- filter(training,SALE_PRICE < 300000 & AV_SP_RATIO < 2 & SFLA < 4000)
View(training_withoutoutlier)
training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,SALE_PRICE))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School District",
       y="Sale price",
       title="Plot for sale price by school district")
 #  scale_x_continuous(labels = comma)
training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,AV_SP_RATIO))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()
training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,SFLA))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()

dim(training)
dim(training_withoutoutlier)

```

# Stepwise Regression Model
library(caret)
library(klaR)
library(rpart)
library(rpart.plot)

```{r}
library(MASS)
library(car)
# Fit the full model 
full.rgmodel <- lm(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND, data = training)
# Stepwise regression model
rg.model <- stepAIC(full.rgmodel, direction = "both", 
                      trace = FALSE)
summary(rg.model)
vif(rg.model)
```
## Multivariate Stepwise Regression Model without outliers

```{r}
library(MASS)
library(car)
# Fit the full model 
full.rgmodel <- lm(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND, data = training_withoutoutlier)
# Stepwise regression model
rg.model_withoutoutliers <- stepAIC(full.rgmodel, direction = "both", 
                      trace = FALSE)
summary(rg.model_withoutoutliers)
vif(rg.model_withoutoutliers)

```



# Decision Tree Model


```{r}
library(caret)
library(klaR)
library(rpart)
library(rpart.plot)
library(MASS)
attach(training)
attach(training_withoutoutlier)
options(scipen = 999)
DT3.model<- rpart(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training,method = "anova")
DT3.model
rpart.plot(DT3.model)
printcp(DT3.model)
plotcp(DT3.model)
p<- predict(DT3.model,training)
sqrt(mean((training$SALE_PRICE-p)^2))
#(cor(training$SALE_PRICE,p))^2

DT4.model<- rpart(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training_withoutoutlier,method = "anova")
DT4.model
rpart.plot(DT4.model)
printcp(DT4.model)
plotcp(DT4.model)
p1<- predict(DT4.model,training_withoutoutlier)
sqrt(mean((training_withoutoutlier$SALE_PRICE-p1)^2))
#(cor(training_withoutoutlier$SALE_PRICE,p1))^2

```

# Random forest Model

```{r}
library(caret)
library(randomForest)
library(rpart)
#library(rpart.plot)
attach(training)
attach(training_withoutoutlier)
RF.cv <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,
                      data=training,proximity=TRUE, na.action=na.roughfix)
print(RF.cv)
#attributes(RF.cv)
#RF.cv$rsq
#RF.cv$mse
#plot(RF.cv)

RF.cv_withoutoutliers <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,
                      data=training_withoutoutlier,proximity=TRUE, na.action=na.roughfix)
print(RF.cv_withoutoutliers)
```

# Run the chosen model - Random forest for testing data

```{r}
RF.cv <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,
                      data=testing,proximity=TRUE, na.action=na.roughfix)
print(RF.cv)
```
