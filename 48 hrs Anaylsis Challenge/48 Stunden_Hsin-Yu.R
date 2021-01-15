### ----
setwd("/Users/ElinaKu/GD(jade)/2019_GD only/Master Life/ＷinSem 19/Software/48 Studen") 
md<- read.csv("NY.csv")
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(xts)
library(anytime)
library(devtools)
library(ggbiplot) 

glimpse(md)
class(md)
head(md,10) 

### DATE/WEEKDAY change----
md$DATE <- anytime(md$DATE)
class(md$DATE)
glimpse(md)

md$WEEKDAY <- ordered(md$WEEKDAY,levels = c("Monday","Tuesday","Wednesday","Thursday",
                                            "Friday","Saturday","Sunday"))
factor(md$WEEKDAY)

md$MONTH



### Questions----
##### What are the variables that affect Taxi trips the most?
### Notes ----

## Outcome is Continuous
#Explanatory Variable is Categorical >>> (Factorial) ANOVA
#Explanatory Variable is Continuous >>> Regression

##### GLM
# vif() / RsqGLM() / NagelkerkeR2() 
#library: AICmodavg, fmsb, modEvA, MuMIn 
#step(), mdl.avg(), aictab(), bictab(), NagelkerkeR2()

## Independence of error terms (Durbin-Watson test) 1<x<3
# dwt(reg)
## No multicolinearity
# vif(reg)<10

### ANOVA ----
levels(md$PRCP_LVL)
md$PRCP_LVL <- ordered(md$PRCP_LVL,levels=c("None","Slight","Moderate","Heavy"))
class(md$PRCP_LVL)

  ## Bike + PRCP_LVL----
md %>% 
  ggplot(aes(x=PRCP_LVL, y=BIKE))+
  geom_boxplot()+
  labs(x="levles of percipitation", y="Number of City-Bike Trips", 
       title="Number of City-Bike Trips",
       subtitle="under different level of percipitation", caption="")+
  theme( plot.title = element_text(hjust=0.5),
         plot.subtitle = element_text(hjust=0.5))

table(md$PRCP_LVL)
library(car)
aov_BIKE <- aov(BIKE ~ PRCP_LVL, data=md)
# summary(aov_TAXI)
Anova(aov_BIKE,type="III")  # type III for unbalanced design

TukeyHSD(aov_BIKE)  
# They are differences affeecting BIKE ride betwen following 4 groups of percipitation :
 # Slight-None, Moderate-None, Heavy-None, Moderate-Slight

 # pairwise.t.test(md$BIKE, md$PRCP_LVL,p.adjust.method = "BH") 

## Diagnoses
 ## homogeneity of variance
  plot(aov_BIKE, 1)
  leveneTest(BIKE ~ PRCP_LVL, data = md)
  # p-value is not less than the significance level of 0.05. 
    # This means that there is no evidence to suggest that 
    # the variance across groups is statistically significantly different. 
    #Thus, we can assume the homogeneity of variances in the different groups.
  
  ## Normality
  plot(aov_BIKE, 2)
  shapiro.test(resid(aov_BIKE)) # Not N.D
    # a non-parametric alternative to one-way ANOVA
  kruskal.test(BIKE ~ PRCP_LVL, data = md) 

  ## TAXI + PRCP_LCL----
md %>% 
  ggplot(aes(x=PRCP_LVL, y=TAXI))+
  geom_boxplot()+
  labs(x="Levles of Percipitation", y="Number of TAXI Trips", 
                     title="Number of Lumousine Trips",
                     subtitle="under different level of percipitation")+
  theme( plot.title = element_text(hjust=0.5),
         plot.subtitle = element_text(hjust=0.5))

aov_TAXI <- aov(TAXI ~ PRCP_LVL, data=md)
# summary(aov_TAXI)
Anova(aov_TAXI,type="III")

  ## GREEN + PRCP_LCL----

md %>% 
  ggplot(aes(x=PRCP_LVL, y=GREEN))+
  geom_boxplot()

aov_3 <- aov(GREEN ~ PRCP_LVL, data=md)
summary(aov_3)
Anova(aov_3,type="III")

md %>% 
  ggplot(aes(x=PRCP_LVL, y=GREEN))+
  geom_boxplot()

  ## TRAFFIC + PRCP_LCL----
md %>% 
  ggplot(aes(x=PRCP_LVL, y=TRAFFIC))+
  geom_boxplot()

aov_4 <- aov(TRAFFIC ~ PRCP_LVL, data=md)
#summary(aov_4)
Anova(aov_4,type="III")

### PCA shows relations of numerics ----
num_W <- md %>% 
  select(AWND,PRCP,SNOW,SNWD,TAVG,TMAX,TMIN,WSF2,WSF5,WDF2,WDF5)
num_T <- md %>% 
  select(BIKE,TAXI,GREEN,TRAFFIC,ACCIDENTS)

num_Total_na <-na.omit(cbind(num_W,num_T))
num_Total_na # The first 2 rows are not in sequence with the rest
length(num_Total_na$AWND) #505 rows with the first 2 rows not continue
num_Total_na <-num_Total_na[-c(1,2),] 
num_Total_na 
glimpse(md[657,])  #num_Total_na starts from 2015.4.18 (row 257)

t.PCA <- prcomp(num_Total_na, center = TRUE, scale. = TRUE)
summary(t.PCA) # 41% ecplained by PC1+2 

ggbiplot(t.PCA, obs.scale =1 , var.scale=1, labels ="")+ 
  xlim(-3, 2)+ylim(-3, 3)+
  labs(title="PC1 & PC2 of all numeric variables" ,
       subtitle ="showing correlations between variables")+
  theme(plot.title = element_text(hjust=0.5,face="bold"),
        plot.subtitle =element_text(hjust=0.5))

t.PCA$rotation
# +: AWND, WSF2, WSF5, WDF2, WDF5 (SNOW, PRCP)
# +: SNWD, GREEN, TRAFFIC, (TAXI) --> explore --> not significant
# -: TAVG, TMANX, TMIN
# -: (ACCIDENT,BIKE) --> explore

  ## model ----
mdl_1 <- lm(GREEN~SNWD,data=md)
summary(mdl_1) # not sign.

mdl_1.1 <- lm(GREEN~SNWD + TAVG,data=md)
summary(mdl_1.1) # hotter = less  Limousine trips, super small R^2

mdl_2 <- lm(TRAFFIC~SNWD,data=md)
summary(mdl_2) # not sign.

mdl_2.1 <- lm(TRAFFIC~SNWD + TAVG,data=md)
summary(mdl_2.1) # not sign.

mdl_3 <- lm(TAXI~SNWD,data=md)
summary(mdl_3) # not sign.

mdl_3.1 <- lm(TAXI~ SNWD + TAVG,data=md)
summary(mdl_3.1) # hotter, more snow depth = less Taxi, R^2=5%


mdl_BIKE_0 <- lm(BIKE ~ TAVG,data=md)
summary(mdl_BIKE_0) # more snow depth, colder = less Bike, R^2=52%

mdl_BIKE <- lm(BIKE ~ SNWD + TAVG,data=md)
summary(mdl_BIKE) # more snow depth, colder = less Bike, R^2=52%

shapiro.test(resid(summary(lm(BIKE ~ SNWD + TAVG + SNOW,data=md))))

mdl_BIKE <- lm(BIKE ~ SNWD + TAVG,data=md)
summary(mdl_BIKE) # more snow  depth,colder = less Bike, R^2=52%
#install.packages("waveslim")
#library(waveslim)
#dwt(mdl_BIKE)
#install.packages("regclass")
#library(regclass)
# vif(mdl_BIKE) # both=1.1 < 4

## Diagnosis
 #par(mfrow = c(2, 2))
 #plot(mdl_BIKE)

 ## Linearity of the data
  plot(mdl_BIKE,1) # linear relationship between predictors and dep. variables.
 ## Normality of residuals
  plot(resid(mdl_BIKE))
  plot(mdl_BIKE,2)
  shapiro.test(mdl_BIKE$residuals) # not N.D
 ## Homogeneity of variance
  plot(mdl_BIKE,3)
  
summary(glm(BIKE ~ SNWD + TAVG + SNOW, data=md, poisson(link=log)))
vif(glm(BIKE ~ SNWD + TAVG + SNOW, data=md, poisson(link=log)))
mdl_BIKE_glm <- glm(BIKE ~ SNWD + TAVG, data=md, poisson(link=log))
summary(mdl_BIKE_glm)
vif(mdl_BIKE_glm)  # =1.1 <4
#install.packages("AICcmodavg")
#install.packages("raster")
#install.packages("Rcpp")
#library(Rcpp)
#library(raster)
#library(AICcmodavg)
#c_hat(mdl_BIKE_glm)

summary(aov(BIKE ~ PRCP_LVL , data=md))
summary(aov(BIKE ~ SNWD + TAVG + SNOW + PRCP_LVL +  , data=md))


mdl_BIKE_glm_W1 <- glm(BIKE ~ TAVG + WT01 , data=md, poisson(link=log))
summary(mdl_BIKE_glm_W1)
vif(mdl_BIKE_glm_W1)
summary(aov(BIKE ~ TAVG + WT01 , data=md))

mdl_BIKE_glm_W2 <- glm(BIKE ~ TAVG + WT02 , data=md, poisson(link=log))
summary(mdl_BIKE_glm_W2)
summary(aov(BIKE ~ SNWD + TAVG + WT02 , data=md))

mdl_BIKE_glm_W8 <- glm(BIKE ~ SNWD + TAVG + WT08 , data=md, poisson(link=log))
summary(mdl_BIKE_glm_W8)
summary(aov(BIKE ~ SNWD + TAVG + WT08 , data=md))

mdl_BIKE_glm_W128 <- glm(BIKE ~ WT01 + WT02 + WT08, data=md, poisson(link=log))
summary(mdl_BIKE_glm_W128)

summary(aov(BIKE ~ WT01 + WT02 + WT08 , data=md))
summary(glm(BIKE ~ PRCP_LVL + WT01 + WT02 + WT03 + WT04 + WT06 + WT08 + WT09, data=md, poisson(link=log)))


md %>% 
  ggplot(aes(x=WT02, y=BIKE))+
  geom_boxplot()+
  labs(x="WT2", y="Number of City-Bike Trips",
       title="Number of city-bike trips with or without fog")+
  theme( plot.title = element_text(hjust=0.5),
         plot.subtitle = element_text(hjust=0.5))

md %>% 
  ggplot(aes(x=WT08, y=BIKE))+
  geom_boxplot()+
  labs(x="WT1", y="Number of City-Bike Trips",
       title="Number of city-bike trips with or without thunders")+
  theme( plot.title = element_text(hjust=0.5),
         plot.subtitle = element_text(hjust=0.5))





mdl_5 <- lm(ACCIDENTS ~ TAVG , data=md)
summary(mdl_5) # hotter = more accid., R^2=3%

mdl_5.1 <- lm(ACCIDENTS ~ SNWD + TAVG,data=md)
summary(mdl_5.1) # hotter = more accid., R^2=3%

## PC3 & PC4 for all numeric variables
ggbiplot(t.PCA, obs.scale =1 , var.scale=1, labels ="",choices = c(3,4))+ 
  xlim(-3, 2)+ylim(-3, 3)+
  labs(title="PC3 & PC4 of all numeric variables" ,
       subtitle ="showing correlations between variables")+
  theme( plot.title = element_text(hjust=0.5,face="bold"),
         plot.subtitle =element_text(hjust=0.5))

## PCA for part of the numeric variables(without BIKE & TRAFFIC)----
num_pT <- md %>% 
  select(BIKE,TAXI,GREEN,ACCIDENTS)

num_pTotal_na <-na.omit(cbind(num_W,num_pT))
num_pTotal_na
length(num_pTotal_na$AWND) 
num_pTotal_na <-num_pTotal_na[-1,] 
glimpse(md[36,]) #2013.08.5

p.PCA  <- prcomp(num_pTotal_na, center = TRUE, scale. = TRUE)
# summary(p.PCA)
p.PCA$rotation
ggbiplot(p.PCA, obs.scale =1 , var.scale=1, labels ="")+ 
  xlim(-3, 2.5)+ylim(-2, 3)+
  labs(title="PC1 & PC2 of all numeric variables", 
       subtitle ="without TRAFFIC")+
  theme( plot.title = element_text(hjust=0.5,face="bold"),
         plot.subtitle =element_text(hjust=0.5))

mdl_1.2 <- lm(GREEN ~ AWND + PRCP + WSF2, data=md, )
summary(mdl_1.2) # wind = more  Limousine trips, super small R^2

mdl_5.2 <- lm(ACCIDENTS ~ SNWD,data=md)
summary(mdl_5) # not sign.

mdl_1.3 <- glm(GREEN ~ AWND + PRCP + WSF2, data=md, family=poisson(link=log))
summary(mdl_1.3) # wind = more  Limousine trips, super small R^2

### normality----
install.packages("ggpubr")
library("ggpubr")
shapiro.test(md$AWND)
shapiro.test(md$PRCP)
shapiro.test(md$SNOW)

ggqqplot(md$AWND, ylab = "AWND")
ggqqplot(md$PRCP, ylab = "PRCP")
ggqqplot(md$SNOW, ylab = "SNOW")

### GLM----
mdl <- glm(BIKE ~ SNOW + SNWD + WDF2 + WDF5 + TAVG, data=md, Gamma(link=log))
summary(mdl)
plot(mdl$residual)
boxplot(mdl$residual)
shapiro.test(mdl$residual)

### plot try----
SB <- md %>% 
  select(DATE,BIKE,SNOW) %>% 
  ggplot(aes(x=SNOW,group=BIKE))+
  geom_boxplot()

SB

AWND
plot(AWND)

by_holiday <- md %>% 
  group_by(HOLIDAY) %>% 
  summarize(mean(ACCIDENTS,na=T))

by_holiday

by_weekday <- md %>%
  group_by(WEEKDAY)%>%
  summarize(mean_B = mean(BIKE,na=T),mean_T=mean(TAXI,na=T),mean_G=mean(GREEN,na=T),
            mean_Tra=mean(TRAFFIC,na=T),mean_A=mean(ACCIDENTS,na=T)) %>% 
  mutate(wd_num =c(5,1,6,7,4,2,3)) %>% 
  arrange(wd_num)

by_weekday

factor(by_weekday$WEEKDAY, ordered=T, levels=c("Monday","Tuesday","Wednesday","Thursday",
                                       "Friday","Saturday","Sunday"))

md %>% 
  ggplot(aes(x=HOLIDAY,y=ACCIDENTS,color=WEEKDAY))+
  geom_boxplot()

by_weekday %>%
  ggplot(aes(x=WEEKDAY))+
  geom_boxplot()

by_weekday_holiday <- md %>%
  group_by(WEEKDAY,HOLIDAY)%>%
  summarize(mean_B = mean(BIKE,na=T),mean_T=mean(TAXI,na=T),mean_G=mean(GREEN,na=T),
            mean_Tra=mean(TRAFFIC,na=T),mean_A=mean(ACCIDENTS,na=T))

by_weekday_holiday %>%
  ggplot(aes(x=WEEKDAY))+
  geom_bar()+facet_wrap(~WEEKDAY)
### Ohter ----
a<-c("Mon","Fri","Tues","Tues","Mon","Fri","Tues","Mon","Tues","Fri","Tues")
a
factor(a)
a <- ordered(a,levels=c("Mon","Tues","Fri"))
a
factor(a)

origin_a <- factor(a)
arrange_a <- factor(a, ordered=T, levels=c("Mon","Tues","Fri"))
levels(origin_a) 
levels(arrange_a)
