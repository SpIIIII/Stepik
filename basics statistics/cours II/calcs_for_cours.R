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
                
