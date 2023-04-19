rm(list = ls())
library(readr)
library(readxl)
library(corrplot)

setwd("C:/Users/braya/Desktop/Maestria/Segundo semestre/Analisis avanzado de datos")

taller1 <- read.csv("C:/Users/braya/Desktop/Maestria/Segundo semestre/Analisis avanzado de datos/taller1.txt")
df <- data.frame(read.table("taller1.txt", sep = ",", header = 1))
head(df)
View(df)

df[,1:10]


# Hay multicolinealidad en las variables:
corr <- cor(df)
names(corr[,1:10])

valores = vector("list", dim(df)[2])

for(i in 1:dim(df)[2]){
  frame <- data.frame(corr[,i][(corr[,i] > 0.7)])
  names(frame) = c(names(df[i]))
  if(dim(frame)[1] > 2){
    valores[[i]] <- frame
  }
}

valores_filtrados <- Filter(Negate(is.null), valores)
valores_filtrados
# esto demuesta que no hay una correlacion tan alta entre las variables como para sospechar en una multicolinealidad

library(readr)
library(caTools)
library(glmnet)

sample <- sample.split(df$y, SplitRatio = 1000/1200)

train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)