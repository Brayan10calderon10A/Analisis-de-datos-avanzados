rm(list = ls())
library(ISLR2)
library(dplyr)
library(splines)
library(mgcv)
library(boot)
library(gamreg)
library(mvtnorm)

set.seed(1234)

df <- Auto
head(df)

N <- dim(df)[1]
n <- ceiling(N*0.9)

t_muestra <- sample(N, n)

entrenamiento <- df[t_muestra,]
head(entrenamiento)

test <- df[-t_muestra,]
head(test)

####

train <- sample(nrow(df), nrow(df)*0.9)
test <- setdiff(1:nrow(df), train)

train.data <- df[train,]
test.data <- df[test,]

spline.reg <- function(k){
  model <- lm(mpg ~ ns(horsepower, knots = k), data = train.data)
  pred <- predict(model, newdata = test.data)
  return(mean(test.data$mpg - pred)**2)
}


k.values <- 1:10
mse <- sapply(k.values, spline.reg)

mean.mse <- sapply(k.values, function(k) mean(mse[k.values == k]))
plot(k.values, mean.mse, type = "l", xlab = "Numero de knots", ylab = "MSE")

k.optimo <- k.values[which.min(mean.mse)]

cat("Numero optimo de knots:", k.optimo)

## segundo punto:

polinomio.model <- lm(mpg ~ poly(horsepower, 2), data = train.data)
summary(polinomio.model)


spline.modelo <- gam(mpg ~ s(horsepower), data = train.data)
summary(spline.modelo)


modelo.reg.spline <- lm(mpg ~ ns(horsepower, knots = k.optimo), data = train.data)
summary(modelo.reg.spline)


polinomio.cv <- cv.glm(train.data, polinomio.model, K = 10)
spline.cv <- cv.glm(train.data, spline.modelo, K = 10)

spline.suavizado.cv <- cv.gam(train.data, modelo.reg.spline,fold = 10)

?cv.glm

polinomio.mse <- mean(polinomio.cv$delta**2)
spline.mse <- mean(spline.cv$delta**2)
spline.suavizado.mse <- mean(spline.suavizado.cv$delta**2)


## tercer punto:

locfit.model0 <- loess(mpg ~ horsepower, data = train.data, degree = 0)

locfit.model1 <- loess(mpg ~ horsepower, data = train.data, degree = 1)

locfit.model2 <- loess(mpg ~ horsepower, data = train.data, degree = 2)

train.data$pred0 <- predict(locfit.model0, train.data)
train.data$pred1 <- predict(locfit.model1, train.data)
train.data$pred2 <- predict(locfit.model2, train.data)

mse0 <- mean((train.data$mpg - train.data$pred0)^2)
mse1 <- mean((train.data$mpg - train.data$pred1)^2)
mse2 <- mean((train.data$mpg - train.data$pred2)^2)

if (mse0 < mse1 & mse0 < mse2) {
  cat("El modelo de regresión local con kernel gaussiano de grado 0 es el mejor modelo.\n")
} else if (mse1 < mse0 & mse1 < mse2) {
  cat("El modelo de regresión local con kernel gaussiano de grado 1 es el mejor modelo.\n")
} else {
  cat("El modelo de regresión local con kernel gaussiano de grado 2 es el mejor modelo.\n")
}


bs.model <- lm(mpg ~ bs(horsepower, knots = 4), data = train.data)

locfit.model <- loess(mpg ~ horsepower, data = train.data, degree = 2)

poly.model <- lm(mpg ~ horsepower + I(horsepower^2), data = train.data)

test.data$pred_bs <- predict(bs.model, test.data)
test.data$pred_locfit <- predict(locfit.model, test.data)
test.data$pred_poly <- predict(poly.model, test.data)

mse_bs <- mean((test.data$mpg - test.data$pred_bs)^2)
mse_locfit <- mean((test.data$mpg - test.data$pred_locfit)^2)
mse_poly <- mean((test.data$mpg - test.data$pred_poly)^2)

cat("El ECM de prueba para el modelo basado en base de funciones es", round(mse_bs, 3), "\n")
cat("El ECM de prueba para el modelo basado en regresión local es", round(mse_locfit, 3), "\n")
cat("El ECM de prueba para el polinomio global de grado dos es", round(mse_poly, 3), "\n")

