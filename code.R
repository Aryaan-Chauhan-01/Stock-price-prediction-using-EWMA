#reading the entire dataset
df=read.csv("all_stocks_1yrdata.csv")

#data cleaning
na.omit(df)
df

#taken five companies
c_googl<-subset(df,Name == "GOOGL")
c_googl
C_amzn<-subset(df,Name == "AMZN")
C_amzn
C_ibm<-subset(df,Name == "IBM")
C_ibm
C_msft<-subset(df,Name == "MSFT")
C_msft
c_nike<-subset(df,Name == "NKE")
c_nike
#Data Overview
#Google
summary(c_googl)
str(c_googl)
#Amazon
summary(C_amzn)
str(C_amzn)
#IBM
summary(C_ibm)
str(C_ibm)
#Microsoft
summary(C_msft)
str(C_msft)
#Nike
summary(c_nike)
str(c_nike)

#Data Cleaning
df[is.na(df)] <- 0
df
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
summary(df)
str(df)

#the ewma work
library(ggplot2)
library(pracma)
c_googl$EMAClose<-movavg(c_googl$Close,n=2,type='e')
ggplot(c_googl, aes(Date,group=1)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Google stocks")

C_amzn$EMAClose<-movavg(C_amzn$Close,n=2,type='e')
ggplot(C_amzn, aes(Date,group=2)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Amazon stocks")

C_ibm$EMAClose<-movavg(C_ibm$Close,n=2,type='e')
ggplot(C_ibm, aes(Date,group=3)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for IBM stocks")

C_msft$EMAClose<-movavg(C_msft$Close,n=2,type='e')
ggplot(C_msft, aes(Date,group=4)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Microsoft stocks")

c_nike$EMAClose<-movavg(c_nike$Close,n=2,type='e')
ggplot(c_nike, aes(Date,group=5)) +geom_line(aes(y=Close),colour ="red")+geom_line(aes(y=EMAClose),colour ="green")+ggtitle("Actual(red colour) v/s Predicted(green colour) Closing values for Nike stocks")

#closer the corelation to one better the prediction
cor(c_googl$Close,c_googl$EMAClose)
cor(C_amzn$Close,C_amzn$EMAClose)
cor(C_ibm$Close,C_ibm$EMAClose)
cor(C_msft$Close,C_msft$EMAClose)
cor(c_nike$Close,c_nike$EMAClose)
