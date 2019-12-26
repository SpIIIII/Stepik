x <- mtcars[,c('wt','mpg')]

x <- scale(x)
lm(wt ~ mpg,data = x)
?lm
x

plot(scale(x))

summary(scale(x))

?scale
