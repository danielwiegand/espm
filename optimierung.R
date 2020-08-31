library(dplyr)
library(ggplot2)
library(nleqslv)

# Geteilte Funktion

f1 <- function(x) {3 * .97 ^ x}
f2 <- function(x) {-x+30}
f3 <- function(x) {e_min}

g <- function(x) {
  dat <- data.frame(emission = 3, t = 0:82)
  dat$emission[i] <- dat$emission[i-1] * x[1]
  y[y < .2] <- f2(x[y < 2])
  y[y <= e_min] <- f3(x[y <= e_min])
  
}

# Optimierung

emis_by <- 4
budget <- 60
emis_neg <- -.2

make_linear <- function(x) {
  for(i in 3:length(x)) {
    if(x[i] <= 1) {
      x[i] <- x[i-1] + x[i-1] - x[i-2]
    }
  }
  return(x)
}

make_horizontal <- function(x) {
  for(i in 3:length(x)) {
    if(x[i] <= -0.1) {
      x[i] <- -0.1
    }
  }
  return(x)
}

fun <- function(x){
  dat <- data.frame(emission = emis_by, t = 0:82)
  for(i in 2:83){
    dat$emission[i] <- emis_by * x ^ i
  } 
  dat$emission <- make_linear(dat$emission)
  dat$emission <- make_horizontal(dat$emission)

  ret <- numeric(1)
  ret[1] <- sum(dat$emission) - budget
  return(ret)
}

xstart <- matrix(runif(800, min=0, max=1), ncol = 1) # Davor: -0.3 / 0.3
opt_x <- searchZeros(xstart, fun,  method="Broyden", global="pwldog", control=list(btol=1e-6))

fun(x = .6)
fun(x = opt_x[[1]][1])

# Check

dat <- data.frame(emission = emis_by, t = 0:82)
for(i in 2:83){
  dat$emission[i] <- emis_by * opt_x[[1]][1] ^ i
} 
dat$emission <- make_linear(dat$emission)
dat$emission <- make_horizontal(dat$emission)

all.equal(sum(dat$emission), budget)

# Plot

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()

