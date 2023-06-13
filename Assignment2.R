library(imager)
library(tidyverse)
library(rvest)
install.packages("rvest")
#part a
data(iris)
plot(iris$Sepal.Length,iris$Petal.Length,xlab='sepal length',ylab = "petal length",pch=25
    ,main="plot",col=iris$Species)
#part b
library(imager)
flip <- function(img)
{
  img.mat <- as.array(img[,,1,])
  z <- dim(img.mat)
  cat <- array(0,dim=z)
  for(i in 1:z[1])
  {
    for(j in 1:z[2])
    {
      cat[i,j,] <- img.mat[z[1]-i+1,j,]
    }
    return(cat)
  }
}
#part c
library(MASS)

data <- ships

dataA <- subset(data,data$type == 'A')
dataB <- subset(data,data$type == 'B')
dataC <- subset(data,data$type == 'C')
dataD <- subset(data,data$type == 'D')
dataE <- subset(data,data$type == 'E')

plot(dataA$service,dataA$incidents,type = "l",col = "yellow", xlab = "service", ylab = "incidents", main = "ships data",xlim = c(0,45000),ylim = c(0,60))
lines(dataB$service,dataB$incidents,type = "l",col = "red")
lines(dataC$service,dataC$incidents,type = "l",col = "blue")
lines(dataD$service,dataD$incidents,type = "l",col = "green")
lines(dataE$service,dataE$incidents,type = "l",col = "pink")
co = 0
co[1] = cor(dataA$service,dataA$incidents)
co[2] = cor(dataB$service,dataB$incidents)
co[3] = cor(dataC$service,dataC$incidents)
co[4] = cor(dataD$service,dataD$incidents)
co[5] = cor(dataE$service,dataE$incidents)

### here by observing this graph we can't say b is least trustworthy ship as it has done so many months of service 
## hence given statement is disproved


#i disprove, we cannot say that B is not trustworthy by looking at the incidents itself
#we have to also consider the services

#part d
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
h <- html %>% html_elements(".s-post-summary--content .s-post-summary--content-title .s-link")%>%html_text()
title
answers <- html %>% html_elements(".s-post-summary--stats-item-number")%>%html_text()
a <- 0
b <- 0
c <- 0
for(i in 1:15)
{
  a[i] <- answers[3*i-2]
  b[i] <- answers[3*i-1]
  c[i] <- answers[3*i]
}
data <- data.frame(
  title <- h,
  No_of_votes <- a,
  No_of_answers <- b,
  No_of_views <- c
)
#part e
set.seed(42)
tablets <- numeric(100)
for(i in 1:100){
  tablets[i]=1
}
days <- 0
avg <- 0
for(i in 1:1000)
{
  while(TRUE)
  {
    a <- sample(tablets,size=1)
    if(a == 1){
      tablets[days+1] = 0.5
    }else{
      break
    }    
    days <- days+1
  }
  print(days)
  avg[i] <- days
  days <- 0
  tablets <- rep(1,100)
}
mean(avg)
