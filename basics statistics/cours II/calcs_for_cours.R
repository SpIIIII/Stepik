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


test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

tables <- apply(test_data, 2, table)
squeres <- apply(tables, 2, chisq.test)
squeres$p.value
lmap(squeres, )

most_significant <-  function(x){
  chisquered <- sapply(x,  function(x){
    t = table(x)
    ch = chisq.test(t)
    return (ch$p.value)
  } )
  return (min(chisquered))
}






