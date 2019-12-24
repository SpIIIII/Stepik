setwd('~/Programming/Stepik/R')
library(ggplot2)
library(psych)

?AirPassengers
str(AirPassengers)
air_pas <- AirPassengers
for (i in 2:length(AirPassengers))
  if (AirPassengers[i-1]<AirPassengers[i])
    result<-c(result,AirPassengers[i])
x<-c(1,2,3,4,5,6,7,8,9,10)
mean(x)
moving_average = c()
for (i in 10:length(AirPassengers))
  moving_average <-c(moving_average,mean(AirPassengers[(i-9):i]))
moving_average <- sapply(1:135, function(x) print(x))

df <- mtcars
str(df)
df$vs <- factor(df$vs, labels = c("V","S"))
df$am <-factor(df$am , labels = c("Auto","Manual"))
median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)
mean(df$mpg[df$cyl == 6])
mean(df$mpg[df$cyl == 6 & df$vs == "V"])
sd(df$hp[df$cyl != 3 & df$am == "Auto"])
mean_hp_by_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

  colnames(mean_hp_by_vs)
colnames(mean_hp_by_vs) <- c("VS","Mean HP")
aggregate(hp ~ vs,df, mean)
aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list(df$vs,df$am), FUN = mean)
aggregate( x = df[,-c(8,9)],by = list(df$am), FUN = median)
aggregate(df[,c(1,3)],by = list(df$am,df$vs), FUN = sd)
aggregate(cbind(mpg,disp) ~ am + vs, df, sd)
cbind(df$mpg,df$disp)
aggregate(cbind(hp,disp)~am,df,sd)


?describe
describe(x=df[,-c(8,9)])
desk = describeBy(x = df[,-c(8,9)],group = df$vs, mat = TRUE,digits = 1)
desk_f = describeBy(x = df[,-c(8,9)],group = df$vs, mat = TRUE,digits = 1, fast = TRUE)
describeBy(df$qsec, group = list(df$vs, df$am), mat = TRUE, digits = 1, fast = TRUE)
sum(is.na(df))
df$mpg[1:10] <- NA
mean(df$mpg, na.rm = TRUE)
aggregate(mpg ~ am, df, sd)
x <- subset(airquality, Month %in% c(7,8,9))
aggregate(Ozone ~ Month, x, length,na.action = na.exclude)
subset(airquality, Month %in% c(7,8,9))
subset(airquality,Month == c(7,8))

?describeBy
str(airquality)
describeBy(airquality,group = airquality$Month)

describe(iris[iris$Species %in% "virginica",] )
iris[iris$Species %in% "virginica"]
iris
x <-c(23, 10, 16, 19, 23, 22, 16, 21, 24, 20, 22, 21, 19, 25, 22, 14, 22, 14, 16, 15, NA, 24, NA, NA, NA, 23, 15, 21, 24, 
      NA, NA, NA, 18, 21, 18, NA, 17, 20, 17, NA)

# graphics

hist(df$mpg,breaks = 20, xlab = "MPG")
boxplot(mpg ~ am, df)
plot(df$mpg,df$hp)

library(ggplot2)
ggplot(df,aes(x = mpg)) + geom_histogram(fill = "white", col = "black", binwidth = 2)

ggplot(df,aes(x = mpg, fill = am)) + geom_dotplot()
ggplot(df,aes(x = mpg, fill = am)) + geom_density(alpha = 0.2)
ggplot(df, aes(x=am, y = hp, col = vs))+geom_boxplot()
my_plot <- ggplot(df, aes(x=mpg, y=hp, col = vs, size = qsec))+geom_point()
my_plot2 <-ggplot(df,aes(x = mpg, fill = am))
my_plot2+geom_dotplot()
ggplot(airquality,aes(x=as.factor(Month), y=Ozone)) + geom_boxplot()
plot1 <- ggplot(mtcars,aes(x=mpg, y=disp, col=hp)) + geom_point()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species),alpha = 0.7) 

write.csv(airquality,'airq.csv')
getwd()

## Analise nominative

df <- read.csv("grants.csv")
str(df)

df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")
df$status <- factor(df$status, labels = c("Not Ffunded", "Funded"))
 
t1 <- table(df$status)
t1
dim(t1)
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
t2

dim(t2)
prop.table(t2, 1)
prop.table(t2, 2)
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3
dim(t3)

dimnames(HairEyeColor)
sum(HairEyeColor[,'Blue','Male'])
prop.table(HairEyeColor[ , ,'Male'],2)
sum(HairEyeColor[,"Green","Female"])

barplot(t2, legend.text = T, args.legend = list(x = "topright"), beside = T)

mosaicplot(t2)
 
mydata <- as.data.frame(HairEyeColor[,,"Female"])
library(ggplot2)

mydata$Hair
obj <- ggplot(mydata, aes(x = Hair, y = Freq)) + 
  geom_bar(stat="identity", position = position_dodge(),aes(fill = Eye))+
  scale_fill_manual(values = c("Brown","Blue", "Darkgrey","Dark"))

obj

#   Binomial Test

binom.test(x=5, n=20, p=0.5)
binom.test(t1)

# Chi-Square
t1
chisq.test(t1)

chi<- chisq.test(t1)
chi$exp
chi$obs

# Fisher's Exact Test
fisher.test(t2)
chisq.test(HairEyeColor["Brown",,"Female"])
library(ggplot2)
t1 <- xtabs(~cut+color, data=diamonds)
t2 <- chisq.test(t1)
print(t2[1])
xtabs(~cut+color, data=diamonds)
diamonds[1:5,'carat']
diamonds$factor_carat <- (diamonds$carat >= mean(diamonds$carat)) *1
diamonds$factor_price <- (diamonds$price >= mean(diamonds$price))*1 
chisq.test(xtabs(~factor_price+factor_carat,diamonds))[1]


# 2.1.15
xt_macars <- xtabs(~am+vs,mtcars)

tbl = table(mtcars$am, mtcars$vs) 
xt_macars
tbl
fisher.test(xt_macars)[1]
chisq.test(mtcars$am, mtcars$vs)[1]


# =========== 2.2 t-test (comparison of two groops)  ==============#

# 2.2.2 - 2.2.9
?iris
df <- iris
str(df)

df1 <- subset(df, Species != 'setosa')
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)
ggplot(df1,aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)
ggplot(df1, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])
bartlett.test(Sepal.Length ~ Species, df1)

test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

t.test(Sepal.Length ~ Species, df1, var.equal = T)
t.test(df1$Sepal.Length, my = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)


# 2.2.10

ToothGrowth
tooth_oj_05 = subset(ToothGrowth, supp == "OJ" & dose == 0.5)
tooth_vc_2 = subset(ToothGrowth, supp == "VC" & dose == 2)
length(tooth_oj_05$len)
tooth_vc_2
t.test(tooth_oj_05$len, tooth_vc_2$len)[1]

correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)
correct_data
xtabs(len ~ supp, correct_data)

# 2.2.11

lekarst <- read.csv("lekarstva.csv")
str(lekarst)
t.test(lekarst$Pressure_before,lekarst$Pressure_after,paired = T)[1]
ggplot(lekarst,aes(x=Pressure_before,fill = 'red'))+
  geom_density()

# 2.2.12-2.2._
ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1)

test2 <- wilcox.test(Petal.Length ~ Species, df1)  
test2$p.value
ggplot(df1,aes(Species, Petal.Length))+
  geom_boxplot()
wilcox.test(df1$Petal.Length, df1$Petal.Width, paired =T)


# 2.2.15
dataset <- read.table('dataset_11504_15.txt')
str(dataset)
bart_test <- bartlett.test(V1~V2,dataset)
if (bart_test$p.value >= 0.05){
  t_test <- t.test(V1~V2,dataset,var.equal = TRUE)
  print(t_test$p.value)
  }else{
    wil_test <-wilcox.test(V1~V2,dataset) 
  print(wil_test$p.value)}

# 2.2.16
ds_16 <-read.csv("dataset_11504_16.txt")
ds_16




# =========== 2.3 analysis of variance ==============#

# 2.3.5 Диспрсионный анализ
shops_ds <- read.csv('shops.csv')
boxplot(price~origin,data = shops_ds)
ggplot(shops_ds,aes(x = origin, y = price))+
  geom_boxplot()
fit <- aov(price ~ origin, data = shops_ds)
summary(fit)


# 2.3.6
fit2 <- aov(price ~ origin + store, shops_ds)
summary(fit)
ggplot(shops_ds, aes(x = store, y = price))+
  geom_boxplot()
model.tables(fit2,"means")
describe(shops_ds$price[shops_ds$store == 'supermarket'])

# 2.3.7
pd = position_dodge(0.1)
ggplot(shops_ds, aes(x = store, y = price, color = origin, group = origin))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, lwd = 0.8, position = pd)+
  stat_summary(fun.data = mean_cl_boot, geom = "line", size = 1.5, position = pd)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size= 5, position = pd, pch = 15)+
  theme_bw()
fit3 <- aov(price ~ origin + store + origin:store, shops_ds)
summary(fit3)
fit4 <- aov(price ~ origin * store, shops_ds)
summary(fit4)

#2.3.8 - 2.3.9
p_to_n <- aov(yield~(N+P+K)^2, npk)
summary(p_to_n)

#2.3.10  
ggplot(shops_ds, aes(x = food, y= price))+
  geom_boxplot()
fit5 <- aov (price ~ food, shops_ds)
summary(fit5)
TukeyHSD(fit5)  
   
# 2.3.11
iris_aov <- aov(Sepal.Width ~ Species,iris)
TukeyHSD(iris_aov)  
ggplot(iris, aes(x = Species, y = Sepal.Width))+
  geom_boxplot()

# 2.3.12
therapy_db <- read.csv("therapy_data.csv")
therapy_db$subject <- as.factor(therapy_db$subject)
fit1 <- aov(well_being ~ therapy, therapy_db)
fit1_er <- aov(well_being ~ therapy + Error(subject/therapy), therapy_db)
summary(fit1_er)

fit2 <- aov(well_being ~ therapy*price, therapy_db)
summary(fit2)
ggplot(therapy_db, aes(x = therapy, y = well_being))+
  geom_boxplot()

fit2_er <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)),therapy_db)
summary(fit2_er)
ggplot(therapy_db, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject)

#  2.3.13
tabletki <- read.csv("Pillulkin.csv")
str(tabletki)
tabletki$patient <- as.factor(tabletki$patient)
fit <- aov(temperature ~ pill + Error(patient/pill), tabletki)
summary(fit)

#  2.3.14
tabletki <- read.csv("Pillulkin.csv")
str(tabletki)
tabletki$patient <- as.factor(tabletki$patient)
fit <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), tabletki)
summary(fit)

# 2.3.15
ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))



# =========== 2.4 Functions ==============#

# 2.4.3
my_calc <- function(x,y){
  s <- x + y 
  d <- x - y
  return (c(s,d))
}
my_calc(x = 5,10)

# 2.4.4
distr1 <- rnorm(100)
distr1[1:30] <- NA
distr1[is.na(distr1)] <- mean(distr1, na.rm = T)
distr1
hist(distr1)
 my_na_rm <- function(x){
   x[is.na(x)] <- mean(x, na.rm = T)
   return(x)
 }

distr1 <- my_na_rm(distr1)
source("my_calc_func.R")
my_calc(1,2)

# 2.4.7 
d <- rnorm(10)
d[1:3] <- NA
which(is.na(d))

# 2.4.8
d <- rnorm(10)
d[1:3] <- NA
sum(is.na(d))

# 2.4.9
dir(pattern = "*.csv")
grants <- data.frame()
for (i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind (temp_df, grants)
}

read_data <- function(){
  df <- data.frame()
  n <- 0 
  for (i in dir(pattern = "*.csv")){
    temp_df <- read.csv(i)
    df <- rbind (temp_df, df)
    n <- n+1
  }
  print(paste(as.character(number),"files were combined"))
  return (df)
}


# 2.4.10
x <- c(-3, 10, NA, -1, 1, 0, 4, -3, 2, -2, -3, 3, -3, 0)
sum(x[x>0],na.rm=T)

# 2.4.11
x <- c(-0.87, 1, -9.77, 0.06, 0.38, -0.12, 0.59, -3.11, 24.56, -0.53, 
       1.48, 13.03, -41.17, 5.46, 1.98, 102.76, -3.5, 1.38, -0.12, -0.86, 
       0.61, 1.6, -0.85, 0.88, 0.36, -0.19, 0.22, 1.26, 1.75, -2.83)
qn = quantile(x, probs = c(0.25, 0.75))
iq = IQR(x)
x[ x<qn[2]+1.5*iq & x>qn[1]-1.5*iq]

# example of solve
q <- quantile(x, 0.25) + quantile(x, 0.75) 
mean(x) 
q
(x[abs(x - q/2) <= 2*IQR(x)])



# ========== 3.1 Corelation ========== #

#  3.1.2 - 3.1.4
df <- mtcars
fit <- cor.test(x = df$mpg, y = df$hp)
str(fit)
cor.test(~ mpg + hp, df)

plot(x =df$mpg, y = df$hp)
ggplot(df, aes(mpg, hp, col = factor(cyl)))+
  geom_point()

df
df[,c(1,3:7)]
df_numeric <- df[,c(1,3:7)]
pairs(df_numeric)
cor(df_numeric)
fit <- corr.test(df_numeric)
fit$r
fit$p

# 3.1.5
x<-iris[,1:2]
x[,1]
corr.calc <- function(x){
  cor <- cor.test(x[,1],x[,2])
  return(c(cor$estimate,cor$p.value))
}

# 3.1.6
test_df <-read.csv('step6.csv')
str(test_df)
library(psych)
filtered.cor <- function(x){
  fit <- corr.test(dplyr::select_if(x,is.numeric))
  diag(fit$r) <- 0 
  max_index <- which.max(abs(fit$r))
  return (fit$r[max_index])
}
filtered.cor(test_df)

# 3.1.7

smart_cor <- function(x){
  shapiro.test(x)
}











