#installing the packages
install.packages("dplyr")
library(dplyr)
install.packages("Formula")
install.packages("plm")
library(Formula)
library(plm)

#loading the data for the data
bounce <- read.csv(file = "C://Users//Harsh//Desktop//Folder to be sent//Northeastern//Research Practicum//Semrush//bounce.csv")


#finding the records where the share of mobile is more but the bounce rate is less, there are 223 observation where this happened
x <- bounce[bounce$mobile.share > bounce$desktop.share & bounce$mobile.bounce.rate < bounce$desktop.bounce.rate,]

summary(bounce)

#fomatting the data
category <- bounce %>%
  group_by(Ticker) %>%
  summarise(category = unique(Category))

write.csv(category, file = "C://Users//Harsh//Desktop//Folder to be sent//Northeastern//Research Practicum//Semrush//category.csv")

#summarize the data to obtain the values for each category and year
df <- bounce %>%
  group_by(Ticker, Year) %>%
  summarise(average_mobile_share = round(mean(mobile.share,na.rm=TRUE),2),average_mobile_bounce = round(mean(mobile.bounce.rate,na.rm=TRUE),2),
            average_desktop_share = round(mean(desktop.share,na.rm=TRUE),2),average__desktop_bounce = round(mean(desktop.bounce.rate,na.rm=TRUE),2))

#linear regression
plot(df$average_mobile_share, df$average_mobile_bounce)
abline(lm(df$average_mobile_bounce ~ df$average_mobile_share, df))
summary(lm(df$average_mobile_bounce ~ df$average_mobile_share, df))

plot(df$average_desktop_share, df$average__desktop_bounce)
abline(lm(df$average__desktop_bounce ~ df$average_desktop_share, df))
summary(lm(df$average__desktop_bounce ~ df$average_desktop_share, df))

# performing bptest
library('quantmod')
library('lmtest')

bptest(average_mobile_bounce ~ Ticker + Year + average_mobile_share, data = df)

# performing Hausman test
install.packages("Formula")
install.packages("plm")
library(Formula)
library(plm)

fit.within <- plm(df$average_mobile_bounce ~ df$average_mobile_share, index =c("Ticker","Year"), model ="within",df) 
fit.random <- plm(df$average_mobile_bounce ~ df$average_mobile_share, index =c("Ticker","Year"), model ="random",df)

phtest(fit.within,fit.random)

#analyzing the effect of categories on bounce rate and mobile share

library(sjPlot)
library(sjmisc)
library(sjlabelled)

df_model <- plm(df$average_mobile_bounce ~ df$average_mobile_share, index =c("Ticker","Year"), model ="within",df)
tab_model(df_model)
summary(df_model)

#getting coefficients for each category to see the effect of mobile share on mobile bounce rate
b <- fixef(df_model)
require(data.table)
DT3 = as.data.table(b,keep.rownames = TRUE)
colnames(DT3)[1] <- "Ticker" 

#analyzing the effect of categories on bounce rate and desktop share
df1_model <- plm(df$average__desktop_bounce ~ df$average_desktop_share, index =c("Ticker","Year"), model ="within",df)

summary(df1_model)

fixef(df1_model)

#getting coefficients for each category to see the effect of desktop share on desktop bounce rate
a <- fixef(df1_model)
require(data.table)
DT2 = as.data.table(a,keep.rownames = TRUE)
colnames(DT2)[1] <- "Ticker" 

him <- unique(bounce[c("Domain", "Category","Ticker")])

final <- him %>%
  inner_join(DT2) %>%
  inner_join(DT3) %>%
  select(Domain,a,Ticker, Category, b)

colnames(final)[2] <- "Coefficient of desktop" 
colnames(final)[5] <- "Coefficient of mobile"

write.csv(final,"C://Users//Harsh//Desktop//Folder to be sent//Northeastern//Research Practicum//Semrush//final.csv")

#fixed effect modelling for each domain in case of mobile and desktop

#creating panel data where values are changing for each year and for each domain
bounceplm <- plm.data(bounce, c("Domain","Year"))

#model for mobile bounce rate
ffe_1 <- plm(bounceplm$mobile.bounce.rate ~ bounceplm$mobile.share, model="within", data = bounceplm)
summary(ffe_1)
fixef(ffe_1)

#model for desktop bounce rate
ffe_2 <- plm(bounceplm$desktop.bounce.rate ~ bounceplm$desktop.share, model="within", data = bounceplm)
summary(ffe_2)
fixef(ffe_2)

a <- fixef(ffe_1)
c <- fixef(ffe_2)

coef1 <- as.data.table(a,keep.rownames = TRUE)
coef2 <- as.data.table(c,keep.rownames = TRUE)
