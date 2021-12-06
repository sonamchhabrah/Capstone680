---
title: "DSC680_CapStone_Paper"
author: "Sonam Chhabra"
date: "11/14/2021"
output:
  pdf_document: default
  html_document: default
  word_document: default
---
# Importing the Data 

```{r}
OC_Preprocessed_Data <- read.delim("~/UTICA/DSC680-Capstone/Files/OC_Preprocessed_DataFile.txt")
df<- OC_Preprocessed_Data
View(df)
```

# Preprocessing the Data
```{r}
library(dplyr)
df<- df %>%
  filter(ROLL_SECTION==1, VAL_USEABLE==1,ARMS_LENGTH==1)
View(df)
```


```{r}
missing_value <- any(is.na(df[]))
if (missing_value == FALSE) {
  print("There are No missing values")
  } else {print("There are missing values")
  }
class(df)
View(df)
```


```{r}
df_clean<- na.omit(df)
missing_value <- any(is.na(df_clean[]))
if (missing_value == FALSE) {
  print("There are No missing values")
  } else {print("There are missing values")
  }
View(df_clean)
```
```{r}
library(stringr)
df_clean$SWIS_INFO <- str_c(df_clean$SWIS_CO,df_clean$SWIS_TOWN,df_clean$SWIS_VG)
View(df_clean)
```

```{r}
df_clean$SCH_CODE<- as.character(df_clean$SCH_CODE)
df_clean$SCH_CODE[df_clean$SCH_CODE==251200]<- "Vernon"
df_clean$SCH_CODE[df_clean$SCH_CODE==301300]<- "Rome NY"
df_clean$SCH_CODE[df_clean$SCH_CODE==302601]<- "Boonville"
df_clean$SCH_CODE[df_clean$SCH_CODE==303001]<- "Vienna"
df_clean$SCH_CODE[df_clean$SCH_CODE==304001]<- "Kirkland"
df_clean$SCH_CODE[df_clean$SCH_CODE==304801]<- "New Hartford"
df_clean$SCH_CODE[df_clean$SCH_CODE==304804]<- "Rome NY"
df_clean$SCH_CODE[df_clean$SCH_CODE==305201]<- "Remsen"
df_clean$SCH_CODE[df_clean$SCH_CODE==305801]<- "Trenton"
df_clean$SCH_CODE[df_clean$SCH_CODE==306000]<- "Rome NY"
df_clean$SCH_CODE[df_clean$SCH_CODE==306801]<- "Whitestown"
df_clean$SCH_CODE[df_clean$SCH_CODE==307001]<- "Whitestown"
df_clean$SCH_CODE[df_clean$SCH_CODE==307002]<- "Whitestown"
df_clean$SCH_CODE[df_clean$SCH_CODE==353201]<- "Vienna"
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
View(df_clean)
```

```{r}
library(tidyverse)
library(dplyr)
#View(df_clean)
df_clean <- df_clean %>%
  select(SWIS_INFO,PRINT_KEY,TOTAL_AV,SCH_CODE,SALE_PRICE,AV_SP_RATIO,NBR_BEDROOMS,SFLA,KITCHEN_QUALITY,
         OVERALL_COND,NBR_BATHS,RENTAL_PROPERTY,TAX,YR_BUILT)
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
# Correlation

```{r}
library(corrplot)
corr_simple <- function(data=training,sig=0.5){
df_cor <- training %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  corr <- cor(df_cor)
corrplot(corr)
corr[lower.tri(corr,diag=TRUE)] <- NA 
corr[corr == 1] <- NA 
corr <- as.data.frame(as.table(corr))
corr <- na.omit(corr) 
 corr <- subset(corr, abs(Freq) > sig)
 corr <- corr[order(-abs(corr$Freq)),] 
 print(corr)
 }
corr_simple()
```

#Bar Plots - single categorical 
```{r}
training%>%
  ggplot(aes(fct_infreq(SCH_CODE)))+geom_bar(fill="#97B3C6")
```


#Histogram - single numerical
```{r}
training%>%
  ggplot(aes(x=NBR_BEDROOMS))+geom_histogram(binwidth=1,fill="#97B3C6")+theme_bw()
 training%>%
   ggplot(aes(x=NBR_BATHS))+geom_histogram(binwidth=1,fill="#97B3C6")+theme_bw()
 training%>%
   ggplot(aes(x=OVERALL_COND))+geom_histogram(binwidth=1,fill="#97B3C6")+theme_bw()

training%>%
   ggplot(aes(x=KITCHEN_QUALITY))+geom_histogram(binwidth=1,fill="#97B3C6")+theme_bw()


```

# Dot plots for 2 or more numerical variables 

```{r}

training %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="TOTAL_AV",
       y="SALE_PRICE",
       title="Plot for TOTAL_AV and SALE_PRICE")+
  theme_minimal()
 # scale_x_continuous(labels = comma)
```

```{r}

training %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="SFLA",
       y="SALE_PRICE",
       title="Plot for SFLA and SALE_PRICE")+
  theme_minimal()
 # scale_x_continuous(labels = comma)
```

```{r}
training %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=NBR_BATHS))+
  geom_smooth(method = lm,se=F)+
  labs(x="TOTAL_AV",
       y="SALE_PRICE",
       title="Plot for TOTAL_AV and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous()
```

```{r}
training %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=OVERALL_COND))+
  geom_smooth(method = lm,se=F)+
  labs(x="TOTAL_AV",
       y="SALE_PRICE",
       title="Plot for TOTAL_AV and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)
```
```{r}
training %>% 
  ggplot(aes(TOTAL_AV,SALE_PRICE))+
  geom_point(aes(color=KITCHEN_QUALITY))+
  geom_smooth(method = lm,se=F)+
  labs(x="TOTAL_AV",
       y="SALE_PRICE",
       title="Plot for TOTAL_AV and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)
```
```{r}
training %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=KITCHEN_QUALITY))+
  geom_smooth(method = lm,se=F)+
  labs(x="SFLA",
       y="SALE_PRICE",
       title="Plot for SFLA and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)

training %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=OVERALL_COND))+
  geom_smooth(method = lm,se=F)+
  labs(x="SFLA",
       y="SALE_PRICE",
       title="Plot for SFLA and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)



training %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BATHS))+
  geom_smooth(method = lm,se=F)+
  labs(x="SFLA",
       y="SALE_PRICE",
       title="Plot for SFLA and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)


training %>% 
  ggplot(aes(SFLA,SALE_PRICE))+
  geom_point(aes(color=NBR_BEDROOMS))+
  geom_smooth(method = lm,se=F)+
  labs(x="SFLA",
       y="SALE_PRICE",
       title="Plot for SFLA and SALE_PRICE")+
  theme_minimal()+
  scale_x_continuous(labels = comma)
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
training %>%
  ggplot(aes(SCH_CODE,SALE_PRICE))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()
```


```{r}
training %>%
  ggplot(aes(SCH_CODE,AV_SP_RATIO))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

```



# Histogram Plot

```{r}

training %>%
  ggplot(aes(SALE_PRICE))+
  geom_histogram()+
  facet_wrap(~SCH_CODE)+
  theme_bw()+
  scale_y_continuous(labels = comma)

```





# Identifying Outliers

```{r}
library(dplyr)
attach(training)
training_withoutoutlier<- filter(training,SALE_PRICE < 250000 & AV_SP_RATIO < 2)
View(training_withoutoutlier)
training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,SALE_PRICE))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()
 #  scale_x_continuous(labels = comma)
training_withoutoutlier %>%
  ggplot(aes(SCH_CODE,AV_SP_RATIO))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

dim(training)
dim(training_withoutoutlier)

```

# Regression Model


```{r}
library(caret)
library(klaR)
library(rpart)
library(rpart.plot)
attach(training)
reg.cv<- lm(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training)
reg.cv
summary(reg.cv)

library(caret)
library(klaR)
library(rpart)
library(rpart.plot)
attach(training_withoutoutlier)
reg1.cv<- lm(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training_withoutoutlier)
reg1.cv
summary(reg1.cv)


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

DT3.model<- rpart(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training,method = "anova")
DT3.model
rpart.plot(DT3.model)
printcp(DT3.model)
plotcp(DT3.model)
p<- predict(DT3.model,training)
sqrt(mean((training$SALE_PRICE-p)^2))
(cor(training$SALE_PRICE,p))^2

DT4.model<- rpart(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training_withoutoutlier,method = "anova")
DT4.model
rpart.plot(DT4.model)
printcp(DT4.model)
plotcp(DT4.model)
p1<- predict(DT4.model,training_withoutoutlier)
sqrt(mean((training_withoutoutlier$SALE_PRICE-p1)^2))
(cor(training_withoutoutlier$SALE_PRICE,p1))^2

```

# Random forest Model

```{r}
library(caret)
library(randomForest)
library(rpart)
#library(rpart.plot)
attach(training)
attach(training_withoutoutlier)
RF.cv <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training)
print(RF.cv)
attributes(RF.cv)
RF.cv$rsq
RF.cv$mse
plot(RF.cv)

RF2.cv <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training,proximity=TRUE,ntree=60)
RF2.cv
RF1.cv <- randomForest(SALE_PRICE~TOTAL_AV+SFLA+NBR_BATHS+NBR_BEDROOMS+KITCHEN_QUALITY+OVERALL_COND,data=training_withoutoutlier,proximity=TRUE)
RF1.cv
```



# Run the chosen model - DT model

```{r}
library(caret)
library(klaR)
library(rpart)
DT5.model<- rpart(SALE_PRICE~TOTAL_AV,data=training,method = "anova")
DT5.model
rpart.plot(DT5.model)
printcp(DT5.model)
plotcp(DT5.model)
p<- predict(DT5.model,testing)
sqrt(mean((training$SALE_PRICE-p)^2))
(cor(testing$SALE_PRICE,p))^2
```


# Geospatial Representation

```{r}
library(rgdal)
library(ggplot2)
library(purrr)
library(sf)
library(data.table)
library(dplyr)
library(sp)
library(tmap)
options(scipen = 999)
shp <- readOGR(dsn="C:/Users/15512/OneDrive/Documents/UTICA/DSC680-Capstone/OC_Parcels_April2021",layer="OC_Parcels_April2021")
#df_and_map<- sp::merge(x=shp,y=df,by="PRINT_KEY",duplicateGeoms = TRUE)
#shp_p<- tm_shapeand_map)+
 # tm_polygons("SCH_CODE",id="PRINT_KEY",palette="Greens")
plot(shp,col="yellow",bg="pink")
#ggplot(shp,col="yellow",bg="pink")+
 #geom_polygon(color="black")+
 # coord_map(polyconic)+
  #guides(fill=FALSE)

```



```{r}
library(rgdal)
library(ggplot2)
library(purrr)
library(dplyr)
options(scipen = 999)
shp_new <- shp %>% filter(SCH_CODE =303001)
ggplot(shp_new)+
  geom_polygon(color='black')
```
