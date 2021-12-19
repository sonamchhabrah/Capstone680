---
title: "DSC680_CapStone_NewCode"
author: "Sonam Chhabra"
date: "12/17/2021"
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
  select(SWIS_CO,SWIS_VG,SWIS_TOWN,PRINT_KEY,SCH_CODE,SALE_PRICE,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_HALF_BATHS,NBR_FULL_BATHS,LOC_ST_NBR,MAIL_ST_NBR,YR_BUILT)
View(df_clean)
dim(df_clean)
str(df_clean)
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
df_clean <- df_clean %>%
  select(SWIS_INFO,PRINT_KEY,SCH_CODE,SALE_PRICE,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_BATHS,RENTAL_PROPERTY,YR_BUILT)
View(df_clean)
```



```{r}
dim(df_clean)
```


```{r}
library(psych)
describe(df_clean,na.rm = TRUE,trim = 0.1,check = FALSE)
```


```{r}
str(df_clean)
```



```{r}
head(df_clean)
```

# summary of the data

```{r}
summary(df_clean)
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
#Bar Plots - single categorical 
```{r}
df_clean%>%
  ggplot(aes(fct_infreq(SCH_CODE)))+geom_bar(fill="orange")+coord_flip( )+ 
   labs(x="School District",
       y="Count of observations for each school district" 
      # title="Count of observations by school districts"
       )
```

#Histogram - single numerical by school districts

```{r}
df_clean %>%
  ggplot(aes(SALE_PRICE/1000))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Sale Price in 100k dollars",
       y="Count of observations for sale price by school town"
    #   title="Count of observations for sale price by school districts"
       )
#scale_y_continuous(labels = comma)

```
```{r}
df_clean %>%
  ggplot(aes(NBR_BEDROOMS))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Number of Bedrooms",
       y="Count of observations for number of bedrooms for parcels"
     #  title="Count of observations for number of bedrooms for parcels by school district"
       )
#scale_y_continuous(labels = comma)

```

```{r}
df_clean %>%
  ggplot(aes(NBR_BATHS))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Number of Baths",
       y="Count of observations for number of bathrooms for parcels"
      # title="Count of observations for number of bathrooms for parcels by school district"
       )
#scale_y_continuous(labels = comma)

```

```{r}
df_clean %>%
  ggplot(aes(KITCHEN_QUALITY))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Kitchen quality for the parcels",
       y="Count of observations for the kitchen quality for parcels"
      # title="Count of observations for the kitchen quality of the parcels by school district"
       )
#scale_y_continuous(labels = comma)

```


```{r}
df_clean %>%
  ggplot(aes(OVERALL_COND))+
  geom_histogram(fill="orange")+
  facet_wrap(~SCH_CODE)+
  labs(x="Overall condition of the parcels",
       y="Count of observations for the overall condition of the parcels"
     #  title="Count of observations for the overall condition of the parcels by school district"
       )
#scale_y_continuous(labels = comma)

```

# Dot plots for 2 or more numerical variables 


```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price of the parcel"
      # title="Plot for land area and sale price by number of bedrooms"
       )+
  theme_minimal()
 # scale_x_continuous(labels = comma)
```



```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BATHS))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price"
     #  title="Plot for land area and sale price by number of bathrooms"
       )+
  theme_minimal()

```




```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=OVERALL_COND))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price"
      # title="Plot for land area and sale price by overall condition"
       )+
  theme_minimal()

```



```{r}
options(scipen = 999)
df_clean %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=KITCHEN_QUALITY))+
  geom_smooth(method = lm,se=F)+
  labs(x="Land area of the parcel",
       y="Sale price"
     #  title="Plot for land area and sale price by kitchen quality"
       )+
  theme_minimal()

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
       y="Sale price"
       #       title="Plot for the sale price by school district"
       )
x_out_rm ï¼- x[!x %in% boxplot.stats(x)$out] 

```


```{r}
options(scipen = 999)
df_clean %>%
  ggplot(aes(SCH_CODE,SFLA))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School district",
       y="Land area"
      # title="Plot for the land area by school district"
       )

```

# Identifying Outliers

```{r}
library(dplyr)
attach(training)
summary(training)
outlier_saleprice<- 160000+1.5*IQR(SALE_PRICE,na.rm = TRUE)
outlier_saleprice
outlier_sfla<- 1960+1.5*IQR(SFLA,na.rm = TRUE)
outlier_sfla
outlier_bath<- 2.00+1.5*IQR(NBR_BATHS,na.rm = TRUE)
outlier_bath
outlier_bedroom<- 4.00+1.5*IQR(NBR_BEDROOMS,na.rm = TRUE)
outlier_bedroom
outlier_kitchen<- 3.00+1.5*IQR(KITCHEN_QUALITY,na.rm = TRUE)
outlier_kitchen
outlier_overall<- 3.00+1.5*IQR(OVERALL_COND,na.rm = TRUE)
outlier_overall

training_withoutoutlier<- filter(training,SALE_PRICE <= 160165 & SFLA <= 3172 & NBR_BATHS <= 3.5 & NBR_BEDROOMS <= 5.5 & KITCHEN_QUALITY <= 3 & OVERALL_COND <= 3)
View(training_withoutoutlier)

training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,SALE_PRICE))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School town",
       y="Sale price"
      # title="Plot for sale price by school district"
      )
 #  scale_x_continuous(labels = comma)

training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,SFLA))+
  geom_boxplot(color="orange")+
  coord_flip()+
  theme_bw()+
  labs(x="School town",
       y="Land Area"
  )

dim(training)
dim(training_withoutoutlier)

```


# Stepwise Regression Model

```{r}
library(MASS)
library(car)
# Fit the full model 
full.rgmodel <- lm(SALE_PRICE~SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND, data = training)
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
full.rgmodel <- lm(SALE_PRICE~SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND, data = training_withoutoutlier)
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
DT3.model<- rpart(SALE_PRICE~SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training,method = "anova")
DT3.model
rpart.plot(DT3.model)
printcp(DT3.model)
plotcp(DT3.model)
p<- predict(DT3.model,training)
sqrt(mean((training$SALE_PRICE-p)^2))
#(cor(training$SALE_PRICE,p))^2

```

```{r}
library(caret)
library(klaR)
library(rpart)
library(rpart.plot)
library(MASS)
attach(training)
attach(training_withoutoutlier)
options(scipen = 999)

DT4.model<- rpart(SALE_PRICE~SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training_withoutoutlier,method = "anova")
DT4.model
rpart.plot(DT4.model)
printcp(DT4.model)
plotcp(DT4.model)
p1<- predict(DT4.model,training_withoutoutlier)
sqrt(mean((training_withoutoutlier$SALE_PRICE-p1)^2))
#(cor(training_withoutoutlier$SALE_PRICE,p1))^2

```
# Run the chosen model - Decision Tree for testing data

```{r}
library(caret)
library(klaR)
library(rpart)
library(rpart.plot)
library(MASS)
attach(training)
attach(training_withoutoutlier)
options(scipen = 999)
DT6.model<- rpart(SALE_PRICE~SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=testing,method = "anova")
DT6.model
rpart.plot(DT6.model)
printcp(DT6.model)
plotcp(DT6.model)
p<- predict(DT6.model,testing)
sqrt(mean((testing$SALE_PRICE-p)^2))
```

