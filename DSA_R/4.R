make.power <- function(n){
  pow <- function(x){
    x^n
  }
  pow
}

xa <- make.power(3)
xi<- make.power(2)
xa(3)
xi(3)

### 

ls(environment(xa))
get("n",environment(xa))

##
y <- 10

f<- function(x){
  y<- 2
  y^2 + g(x)
}

g<- function(x){
  x*y
}

f(3)

###
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z<-10
f(3)
