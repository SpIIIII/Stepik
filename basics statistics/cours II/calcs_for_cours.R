setwd('~/Programming/Stepik/basics statistics/cours II')

df = data.frame(c(10,6),c(5,15))
df
res <- chisq.test(df)
str(res)
res$expected
df = data.frame(c(6,0),c(3,3))
chisq.test(df)
df = data.frame(c(20,15),c(11,12))
chisq.test(df)
           


     
# 1.9
c1 <- c(NaN, NaN, 9, 5, NaN, 2, NaN, NaN)
c2 <-c(1, 7, NaN, 8, NaN, NaN, NaN, 6)
NA_position <- function(x, y){
  A = which(is.na(x))
  B = which(is.na(y))
  if (length(A) == length(B)){
    if(A==B){
    return (T)}
  }
  return (F)
}
A = which(is.na(x))
B = which(is.na(y))
NA_position(c1,c2)


df <- as.data.frame.matrix(table(mtcars[1:20,c("am", "vs")]))
smart_test <-  function(x){
  df = as.data.frame.matrix(table(x))
  if (min(df)<5){
    return (fisher.test(df)$p.value)}
  fit = chisq.test(df)
  return (c(fit$statistic, fit$parameter, fit$p.value))
}



# ========
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

most_significant_my <-  function(x){
  chisquered <- sapply(x,  function(x){
    t = table(x)
    ch = chisq.test(t)
    return (ch$p.value)
  } )
  names <- rownames(as.matrix(chisquered))
  ret <- names[which(chisquered==min(chisquered))]
  return (ret)
}
most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
  min_p  <- which(chisq_tests == min(chisq_tests))    
  return(colnames(test_data)[min_p])
}




# ========
means <- colMeans(iris[,1:4])
ir <- iris[,1:4]
iris$important_cases <- ifelse((ir[1]>means[1]) + (ir[2]>means[2]) + (ir[3]>means[3]) + (ir[4]>means[4]) >=3, 'yes', 'no')

str(as.factor(iris$important_cases))

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))



# ========
test_data <- data.frame(V1 = c(16, 21, 18), 
                          V2 = c(17, 7, 16), 
                          V3 = c(25, 23, 27), 
                          V4 = c(20, 22, 18), 
                          V5 = c(16, 17, 19))
get_important_cases_my <- function(x){
  means <- colMeans(test_data)
  bigger <- rowSums(t(t(test_data)>means))
  x$important_cases <- as.factor(ifelse(bigger> ncol(test_data)/2, "Yes", "No"))
  levels(x$important_cases) <- c("Yes", "No")
  return (x)
}

get_important_cases <- function(x){
  x$important_cases <- factor(colSums(t(x) > colMeans(x)) > ncol(x)/2, c(F,T), c("No","Yes"))
  x
}

get_important_cases(test_data)



# ========
v <- c(9,19,11,18,18,10,16,19,19,3,11,18,1)
stat_mode <- function(x){
  y <- table(x)
  z <- y[which(y == max(y))]
  return (as.numeric(colnames(t(z))))
}

y <- table(v)




#================
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
max_resid <- function(x){
  resud <- chisq.test(table(x))$stdres
  rows_ <- rownames(resud)
  columns_ <- colnames(resud)
  indx <- which(resud==max(resud), arr.ind=T)
  return (c(rows_[indx[1]], columns_[indx[2]]))
}



#================
library(ggplot2)

ggplot(diamonds, aes(x=color, fill=cut))+
  geom_bar(position = 'dodge')


#================
# 2.8.2
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data2 <- as.data.frame(list(x = c(2, 2, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2), y = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2)))


get_coefficients <- function(dataset){
  data <- transform(dataset, x=as.factor(x), y=as.factor(y))
  print(str(data))
  fit <- glm(y~x, data, family = 'binomial')
  coef <- summary(fit)$coefficients
  print(coef)
  exp(coef[,'Estimate'])
}
get_coefficients(test_data2)


#================
# 2.8.3

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")
test_data[,'X4'] - mean(test_data[,'X4'])
centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], function(x) x-mean(x))
}

centered(test_data,var_names)


#================
# 2.8.4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
str(test_data)

get_features <- function(dataset){
  fit <- glm(is_prohibited ~ weight+ length+ width+ type, dataset, family='binomial')
  an <- anova(fit,test = "Chisq")
  r <- rownames(an)[an$`Pr(>Chi)`<0.05]
  r <- r[!is.na(r)]
  if (length(r) == 0 ){
    return ('Prediction makes no sense')
  }
  return (r)
}
get_features(test_data)



#================
# 2.8.5

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

str(test_data)
str(data_for_predict)

most_suspicious <- function(dataset, data_for_predict){
  fit <- glm(is_prohibited ~ weight+ length+ width+ type, dataset, family='binomial')
  tested_data <- data_for_predict[!names(data_for_predict)=='passengers']
  prediction <- predict(fit, tested_data)
  return(data_for_predict[which.max(prediction),5])
}
most_suspicious(test_data, data_for_predict)



#================
# 2.8.6

test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

normality_test <- function(dataset){
  nums <- Filter(is.numeric, dataset)
  res <- sapply(nums, function (x) shapiro.test(x)$p.value)
  return (res)
}

normality_test(test)




#================
# 2.8.7

library(dplyr)
library(reshape2)
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
test_data <- as.data.frame(list(x = c(-0.54, 0.83, 0.28, -0.63, 0.25, 0.45, 0.19, -0.31, 2.83, -0.19, -0.18, -0.84, -0.22, -0.87, 0.31, 0.26, -0.66, 0.3, -1.1, -1.35, 0.1, 1.02, 2.12, 0.01, -0.08, 1.76, -0.84, 0.19, -0.11, 0.04), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)))
test_data$y <-  factor(test_data$y, labels = c('A', 'B', 'C'))
smart_anova <- function(test_data){
  A = test_data[test_data$y=='A',]
  B = test_data[test_data$y=='B',]
  C = test_data[test_data$y=='C',]
  colnames(A) <- 'A'
  A <- A[,'A', drop=T]
  colnames(B) <- 'B'
  B <- B[,'B', drop=T]
  colnames(C) <- 'C'
  C <- C[,'C', drop=T]
  DF = data.frame(A,B,C)
  shtest <- sapply(DF, function(x) shapiro.test(x)$p.value)
  bartest <- bartlett.test(x~y, test_data)$p.value
  if (any(shtest<0.05) || bartest<0.05 ){
    k <- kruskal.test(x~y,test_data)$p.value
    names(k) <- "KW"
    return (k)
  }else{
    a <- aov(x~y,test_data)
    res <- summary(a)[[1]]$'Pr(>F)'[1]
    names(res) <- 'ANOVA'
    return (res)
  }
}

x <- smart_anova(test_data)
y <- smart_anova(test_data2)



#================
# 2.8.8
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
normality_by <- function(test){
  return (aggregate(x~y+z, test_data, function(x){shapiro.test(x)$p.value}))
}

















































































