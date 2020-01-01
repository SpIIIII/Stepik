setwd('~/Programming/Stepik/R')
library(ggplot2)
library(psych)
library(gvlma)

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

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
test_data
shapiro.test(test_data[,1])
smart_cor <- function(x){
  norm <- c(shapiro.test(x[,1])$p.value,shapiro.test(x[,2])$p.value)
  if (any(norm<0.05)){
    res = cor.test(x[,1],x[,2],method ='spearman')
  }else{
    res = cor.test(x[,1],x[,2])
  }
  return (res$estimate)
}
smart_cor(test_data)

# 3.1.8 - 3.1.11
fit <- lm(mpg~hp,df)
summary(fit)
ggplot(df, aes(hp,mpg))+
  geom_smooth(method = 'lm', se = F)+
  facet_grid(.~cyl)


fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)
fitted_values_mpg

new_hp <- data.frame(hp = c(100,150,129,300))
new_hp$mpg <- predict(fit,new_hp)

my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, label = c('four', 'six', 'eight'))

fit <- lm(mpg ~ cyl,my_df)
summary(fit)
ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  theme (axis.text = element_text(size = 25),
         axis.title = element_text (size = 25, face = 'bold'))
aggregate(mpg~cyl,my_df,mean)


# 3.1.12
test_data <- read.csv('dataset_11508_12.txt', sep = ' ')
fit <- lm(test_data[,1] ~ test_data[,2],test_data)
summary(fit)
fit$coefficients

# 3.1.13
ideal_diamonds <- diamonds[diamonds$cut == 'Ideal' & diamonds$carat == 0.46,]
res <- lm(price~depth, ideal_diamonds)
res$coefficients

# 3.1.14
my_df = iris[,1:2]
cor.test(my_df[,1],my_df[,2])$p.value
test_data <- as.data.frame(list(V1 = c(4.61, 5.5, 4.15, 4.49, 5.52, 5.23, 4.01, 3.65, 4.35, 5.26), V2 = c(4.03, 5.77, 3.47, 4.83, 5.6, 4.65, 3.89, 1.44, 3.05, 5.42)))
fit_lm <- lm(test_data[,1] ~ test_data[,2], test_data)
regr.calc <- function(x){
  if (cor.test(x[,1], x[,2])$p.value<0.05){
    fit <- lm(x[,1] ~ x[,2], x)
    x$fit <- predict(fit,x[1])
    return(x)
  }else {
    return ("There is no sense in prediction")    
  }   
}
regr.calc(test_data)

# 3.1.15
test_data

# 3.2.3 - 3.2.4
?swiss
swiss
str(swiss)
hist (swiss$Fertility)
fit <- lm(Fertility ~ Examination + Catholic, swiss)
summary(fit)
fit2 <- lm(Fertility ~ Examination * Catholic, swiss)
summary(fit2)
confint(fit2)
ggplot(swiss, aes(x = Catholic, y = Fertility)) + 
  geom_point() + geom_smooth(method = 'lm') 
  
# 3.2.5
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
test_data
fit <- lm(test_data[,3]~test_data[,1]+test_data[,2],test_data,na.action = na.exclude)
predict(fit)
fit$titted.values
fill_na <- function(x){
  fit <- lm(x[,3]~x[,1]+x[,2],x)
  x$y_full <- ifelse(is.na(x$y),predict(fit,x[3]),x$y)
  return (x)
} 
fill_na(test_data)

# 3.2.6
df <- mtcars[,c('wt','mpg','disp','drat','hp')]
fit <- lm(wt ~ mpg+disp,df)
summary(fit)
cor(Filter(is.numeric,df))

# 3.2.7 - 3.2.12
summary(lm(rating ~ complaints*critical,attitude))

swiss$religious <- ifelse(swiss$Catholic>60, 'Lots','Few')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()+
  geom_smooth(method = 'lm')
ggplot(swiss, aes(x = Catholic, y = Fertility)) + 
  geom_point()+
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, swiss)
summary(fit5)

# 3.2.13 - 3.2.14

mtcars$am <- factor(mtcars$am, labels = c('auto','manual'))
mtcars
ggplot(mtcars, aes(x = mpg, y = wt, col = am))+
  geom_point()+
  geom_smooth(method = 'lm')
summary(lm(wt~am*mpg,mtcars))

# 3.3.2 - 3.3.3
swiss
fit_all <- lm(Fertility~.,swiss)
summary(fit_all)
fit_red_1 <- lm(Fertility~Examination+Education+Catholic+Infant.Mortality,swiss)
summary(fit_red_1)
fit_red_2 <- lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality,swiss)
summary(fit_red_2)
anova(fit_all, fit_red_1)
anova(fit_all, fit_red_2)

optimal_fit <- step(fit_all, direction = 'backward')
summary(optimal_fit)

# 3.3.4
model_full <- lm(rating ~ ., data = attitude) 
ideal_model <- step(model_full,direction = 'backward')

# 3.3.5
anova(model_full,ideal_model)

# 3.4.6
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 
               0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 
               0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 
               0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 
               0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
hist(1/my_vector)

# 3.4.7
test_data <- as.data.frame(list(x1 = c(10, 10, 9, 11, 10, 11, 5, 8, 10, 8, 8, 12, 12, 8, 11, 11, 10, 11, 6, 8, 12, 11, 11, 12, 12, 6, 9, 11, 14, 12), 
                                y1 = c(13, 10, 9, 12, 13, 4, 16, 15, 11, 15, 17, 14, 15, 9, 11, 7, 7, 14, 13, 17, 15, 14, 8, 9, 17, 12, 17, 12, 15, 14)))
# Правильный ответ:
#  0,-0.14

beta.coef <- function(x){
  y <- scale(x)
  fit <- lm(y[,1] ~ y[,2],data.frame(y))
  return (summary(fit)$coefficients[,1])
}
beta.coef(test_data)

# 3.4.9
normality.test  <- function(x){
  answer <- sapply(x,shapiro.test)
  return (answer['p.value',])
  }
normality.test(mtcars)

# 3.5.2 - 3.5.4
ggplot(swiss, aes(Examination_sqr, Education))+
  geom_point()+
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)
swiss$Examination_sqr <- (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_sqr,swiss)
summary(lm2)
anova(lm1,lm2)
swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_num <- 1:nrow(swiss)
ggplot(swiss,aes(x = Examination, y = Education))+
  geom_point()+
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd = 1)+
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point()+ 
  geom_hline(yintercept =  0, col = 'red', lwd =1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid))+
  geom_point()+ 
  geom_hline(yintercept =  0, col = 'red', lwd =1)

ggplot(swiss, aes(x = obs_num, y = lm1_resid))+
  geom_point()+ geom_smooth()
ggplot(swiss, aes(x = obs_num, y = lm2_resid))+
  geom_point()+ geom_smooth()
    # homoscedasticity

#  3.5.5
ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid))+
  geom_point()+ geom_smooth()
x <- read.csv('homosc.csv')
fit <- lm(DV ~ IV, x)
summary(gvlma(fit))

# 3.5.6
ggplot(swiss, aes(x = lm1_resid))+
  geom_histogram(binwidth = 2, fill = 'white', col = 'black')
qqnorm(lm1$residuals)
qqline(lm1$residuals)
shapiro.test(lm1$residuals)
par(mfrow=c(2,2)) 
plot(fit)

# 3.5.7
resid.norm  <- function(fit){
  x <- fit$residuals
  is_norm <- shapiro.test(x)$p.value
  if (is_norm > 0.05){
    res <- ggplot(data.frame(x), aes(x = x))+
      geom_histogram(fill = 'green')
  }else{
    res <- ggplot(data.frame(x), aes(x = x))+
      geom_histogram(fill = 'red')
  }
  return (res)
}
fit <- lm(wt~mpg,mtcars)
resid.norm(fit)

# 3.5.8
x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
high.corr <- function(x){
  cor_ <- abs(round(cor(x),digits = 3))
  y <- which(cor_ == 1,arr.ind = T)
  return (rownames(y))
}
high.corr(my_df)

cor_ <- abs(round(cor(my_df),digits = 3))
y <- which(cor_ == 1,arr.ind = T)
y

# 3.6.3 - 3.6.4
my_df <- read.csv('train.csv', sep = ';')
str(my_df)
ggplot(my_df, aes(read, math, col = gender))+
  geom_point()+
  facet_grid(.~hon)+
  theme(axis.text = element_text(size = 25),
        axis.title=element_text(size=25, face = 'bold'))

fit <- glm(hon~ read+math+gender, my_df, family = "binomial")
summary(fit)

head(predict(object = fit, type = 'response'))
my_df$prob <- predict(object = fit, type = 'response')

# 3.6.5
log_coef <- glm(am~disp+vs+mpg, mtcars,family = "binomial")
log_coef$coefficients

# 3.6.6
ggplot(data = ToothGrowth, aes(x = supp, y = len, fill = factor(dose)))+
  geom_boxplot()

# 3.6.7 - 3.6.8
library(ROCR)
pred_fit <- prediction(my_df$prob,my_df$hon)
perf_fit <- performance(pred_fit, 'tpr', 'fpr')
plot(perf_fit, colorize =T, print.cutoffs.at = seq(0,1,by=0.1))
auc <- performance(pred_fit, measure = 'auc')
str(auc)
perf3 <- performance(pred_fit, x.measure = 'cutoff', measure = 'spec')
perf4 <- performance(pred_fit, x.measure = 'cutoff', measure = 'sens')
perf5 <- performance(pred_fit, x.measure = 'cutoff', measure = 'acc')

plot(perf3, col = 'red',lwd =2)
plot(add=T, perf4, col = 'green',lwd =2)
plot(add = T, perf5, lwd=2)

abline (v=0.225, lwd =2)

my_df$pred_resp <- factor(ifelse(my_df$prob > 0.225, 1, 0),labels = c('N','Y'))
my_df$correct <- ifelse(my_df$pred_resp == my_df$hon,1,0)
 
ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_boxplot()+
  theme(axis.text = element_text(size=25),
        axis.title=element_text(size=25,face='bold'))

mean(my_df$correct)
test_df <- read.csv('test.csv',sep = ';')
test_df$hon <- NA

test_df$hon <- predict(fit, newdata = test_df, type = 'response')
View(test_df)

# 3.6.9
data <- read.csv('data.csv')
str(data)
fit <- glm(admit ~ gpa*rank, data, family = "binomial")

data_na <- data[is.na(data$admit),]
data_na$admit <- predict(fit,data_na[,c(2,3,4)],type = 'response')
nrow(data_na[data_na$admit > 0.4,])

# 3.7.2






