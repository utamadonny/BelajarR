lapply

x <- list(a=1:5, b=rnorm(10))
lapply(x, mean)

x <- 1:4
lapply(x, runif)

help(lapply)

x <- list(a=matrix(1:4,2,2),b=matrix(1:4,3,2))
x
lapply(x,function(xixixi) xixixi[,1]) # anon func

######
sapply

x <- list(a=1:4, b=rnorm(10), c=rnorm(20,1), d=rnorm(100,5))
lapply(x, mean)
sapply(x, mean)
mean(x) #error

#####
apply
str(apply)

x <- matrix(rnorm(200),20,10)
apply(x, 2, mean) #has 10 columns
apply(x, 1, mean) #has 20 rows 

# rowsums = apply(x,1,sum)
rowSums(x)
apply(x, 1, sum)
#rowmean = apply(x,1,mean)
rowMeans(x)
apply(x, 1, mean)
# colsums = apply(x,2,sum)
colSums(x)
apply(x, 2, sum)
#colmean = apply(x,2,mean)
colMeans(x)
apply(x, 2, mean)

x <- matrix(rnorm(200),20,10)
apply(x, 1, quantile, probs=c(0.25,0.75))

a <- array(rnorm(2*2*10),c(2,2,10))
apply(a, c(1,2), mean)
rowMeans(a,dims = 2)

#######
mapply
str(mapply)

list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))
mapply(rep, 1:4, 4:1)

noise <- function(n,mean,sd){
  rnorm(n,mean,sd)
}
noise(5,1,2)
noise(1:5,1:5,2)

mapply(noise,1:5,1:5,2)
list(noise(1,1,2),noise(2,2,2),
     noise(3,3,2),noise(4,4,2),
     noise(5,5,2))

######
tapply
str(tapply)

x <- c(rnorm(10),runif(10),rnorm(10,1))
f <- gl(3,10)
f
x
tapply(x, f, mean)
tapply(x, f, mean, simplify=FALSE)
tapply(x, f, range)

#### is not loop function
split
str(split)

x <- c(rnorm(10),runif(10),rnorm(10,1))
f <- gl(3,10)
split(x,f)

lapply(split(x,f), mean)

head(airquality)
s<- split(airquality,airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone","Solar.R","Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone","Solar.R","Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone","Solar.R","Wind")],
                               na.rm = TRUE))

x<- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
interaction(f1,f2)
str(split(x, list(f1,f2)))
str(split(x, list(f1,f2), drop=TRUE))

library(data.table)
iris_dt <- as.data.table(iris)
iris_dt[Species == "virginica",round(mean(Sepal.Length)) ]

library(tidyverse)
round(mean(filter(iris, Species=="virginica")$Sepal.Length))

a <- apply(iris[1:4], 2, mean)
b <- apply(iris[,1:4], 2, mean)
a
b

data("mtcars")
with(mtcars, tapply(mpg,cyl,mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)

mtcars_dt <- as.data.table(mtcars)
mtcars_dt <- mtcars_dt[,  .(mean_cols = mean(hp)), by = cyl]
round(abs(mtcars_dt[cyl == 4, mean_cols] - mtcars_dt[cyl == 8, mean_cols]))

abs(mean(filter(mtcars, cyl==4)$hp) - mean(filter(mtcars, cyl==8)$hp))
