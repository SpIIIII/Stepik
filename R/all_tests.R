setwd('~/Programming/Stepik/R')
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

library(psych)
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










