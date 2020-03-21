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

res <- VIF(mtcars)



#================
# 1.8.4
test_data <- as.data.frame(list(y = c(3.92, 6.11, 5.2, 5.22, 4.42, 4.24, 4.64, 4.41, 4.54, 5.11, 7.77, 3.69, 3.7, 5.27, 5.23, 5.38, 3.95, 5.44, 5.22, 4.09, 3.3, 5.41, 5.86, 4.72, 5.77, 4.65, 4.99, 3.72, 2.93, 4.5), 
                                x1 = c(5.33, 4.74, 4.99, 4.67, 6.41, 3.51, 5.06, 4.03, 4.36, 4.78, 3.95, 5.78, 4.14, 4.41, 4.55, 5.4, 4.02, 4.08, 6.84, 5.76, 6.13, 3.64, 5.25, 5.1, 2.81, 5.14, 5.81, 4.29, 5.44, 4.31), 
                                x2 = c(28.44, 22.5, 24.92, 21.79, 41.14, 12.34, 25.55, 16.22, 19.05, 22.82, 15.59, 33.41, 17.16, 19.41, 20.68, 29.16, 16.15, 16.67, 46.85, 33.12, 37.54, 13.28, 27.55, 26.02, 7.9, 26.37, 33.81, 18.41, 29.64, 18.59)))
df <- mtcars


smart_model <-  function(dframe){
  one <- names(dframe)[1]
  rest <- names(dframe)[-1]
  
  
  sub <- function(df){
    nams <- names(df)
  res <- NULL
  for (i in 1:length(nams)){
    one <- nams[i]
    rest <- nams[-i]
    f <- as.formula(paste(one,'~',paste(rest,collapse = '+')))
    fit <- lm(f, df)
    val <- summary(fit)$r.squared
    val <- 1/(1-val)
    names(val) <- one
    res <- c(res, round(val, digits=5))
  } 

  if (max(res)>10){
    new_nams <- names(res[res != max(res)])
    if (length(res)==2 && length(new_nams)==0){
      return(res[-1])
    }
    sub(df[new_nams])
  }else{
    return(res)
  }}
  
  res_names <- names(sub(dframe[rest]))
  f <- as.formula(paste(one,'~',paste(res_names,collapse = '+')))
  fit = lm(f,dframe)
  return (fit$coefficients)
}
smart_model(df)














