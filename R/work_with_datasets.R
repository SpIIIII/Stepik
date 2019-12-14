data(mtcars)
head(mtcars)
names(mtcars)
mtcars$even_gear <- mtcars$even_gear <- 1-mtcars$gear%%2
mpg_4 <- mtcars[mtcars$cyl == 4,"mpg"]
mini_mtcars <- mtcars[c(3,7,10,12,sum(lengths(mtcars$mpg))),]
