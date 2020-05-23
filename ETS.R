#install.packages("tiddyverse")
#install.packages("lubridate")
#install.packages("fable")
#install.packages("datetime")
library(WDI)
library(forecast)
library(lubridate)
library(fable)
library(tsibble)
library(datetime)
setwd("D:/Hackathon/Time series")
df <- read.csv('mptimeseriescf.csv', stringsAsFactors = FALSE)
#df <- read.csv("D:/Hackathon/Time series/Maharashtra.csv", stringsAsFactors = FALSE)
x = subset(df, select = -c(Date))
df$Date<-as.factor(df$Date)
df$Date<-strptime(df$Date,format="%d-%m-%Y") #defining what is the original format of your date
df$Date<-as.Date(df$Date,format="%Y-%m-%d")#defining what is the desired format of your date
head(df)
#df$Date <- mdy(df$Date,quiet = TRUE)
z = ncol(df)-1
#y = nrow(df)
dfoutput <- data.frame(matrix(ncol = z, nrow = 30))
dist_name <- colnames(df)
dist_name <- dist_name[-c(1)]
names(dfoutput) <- as.vector(dist_name)
for (i in (2:ncol(df))){
  df1 <- df[c(1,i)]
  df1 <- df1[df1[,2]!=0,]
  df1$Date<- strftime(df1$Date, format = "%j")
  timeser = ts(df1[,2],frequency = 365, start = as.numeric(df$Date))
  plot.ts(timeser)
  pi = ets(timeser,model = "ZZZ")
  summary(pi)
  q = forecast(pi,h=30)
  head(q)
  dfoutput[,(i-1)] <- q$upper
  pi$coef
}
write.table(dfoutput, 'mpets.csv')

