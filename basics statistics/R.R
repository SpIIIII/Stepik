setwd('~/Programming/Stepik/basics statistics')

library(ggplot2)
library(dplyr)

t.test.fromSummaryStats <- function(mu,n,s) {
  -diff(mu) / sqrt( sum( s^2/n ) )
}

mu <- c(45,34)
n <- c(100,100)
s <- c(9,10)
t.test.fromSummaryStats(mu,n,s)

install.packages('BSDA')
library(BSDA)
tsum.test(45,9,100,34,10,100)
one <- c(rnorm(100,45,9))
two <- c(rnorm(100,34,10))
t.test(one, two, paired = T)

# 2.4.11
data <- read.csv('genetherapy.csv')
shapiro.test(data[data$Therapy == 'A',]$expr)
shapiro.test(data[data$Therapy == 'C',]$expr)
shapiro.test(data[data$Therapy == 'B',]$expr)
shapiro.test(data[data$Therapy == 'D',]$expr)

gb_f <- group_by(data,Therapy)

se_data <- summarise(gb_f,mean_e = mean(expr),
                     y_min = mean(expr)-1.96*sd(expr)/sqrt(length(expr)),
                     y_max = mean(expr)+1.96*sd(expr)/sqrt(length(expr)))


ggplot(se_data, aes(factor(Therapy), mean_e))+
         geom_errorbar(aes(ymin = y_min, ymax = y_max),width=0.2, lwd=1)+
          geom_point(shape=21, size=3, fill='white')



# 2.6.2 ~
data <- read.csv('atherosclerosis.csv')
str(data)
data$age <- factor(data$age)
str(data)
fit <- aov(expr~age*dose,data)
summary(fit)

data_birds <- read.csv('birds.csv')
str(data_birds)
fit <- aov(var4 ~ hormone*sex,data_birds)
summary(fit)

# 3.1.-1
a = c(4,5,2,3,1)
b = c(2,1,4,3,5)
my_df <- data.frame(a,b)
cor.test(a,b)
ggplot(my_df,aes(a,b))+
  geom_point()
