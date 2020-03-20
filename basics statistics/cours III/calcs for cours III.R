#================
# 1.8.1

NA_position  <- function(x, y){
  all(is.na(x) == is.na(y))
}



#================
# 1.8.2
df <- mtcars

hetero_test <-  function(test_data){
  nams <- names(test_data)
  pred <- nams[1]
  rest <- nams[-1]
  
  f <- as.formula(paste(pred,'~',paste(rest, collapse='+')))
  fit <- lm(f,test_data)
  resid <- fit$residuals^2

  fit1 <- lm(resid~., test_data[-1])
  res <- summary(fit1)$r.squared
  return (res)
}

hetero_test(df)




#================
# 1.8.3
df <- mtcars
nams <- names(mtcars)[-1]




VIF <-  function(df){
  nams <- names(df)[-1]
  res <- NULL
  for (i in 1:length(nams)){
    one <- nams[i]
    rest <- nams[-i]
    f <- as.formula(paste(one,'~',paste(rest,collapse = '+')))
    fit <- lm(f, df)
    val <- summary(fit)$r.squared
    val <- 1/(1-val)
    names(val) <- one
    res <- c(res, val)
  } 
  return(res)
}

VIF(mtcars)



























