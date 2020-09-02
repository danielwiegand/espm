library(dplyr)
library(ggplot2)
library(nleqslv)

# Geteilte Funktion ####

f1 <- function(x) {3 * .97 ^ x}
f2 <- function(x) {-x+30}
f3 <- function(x) {e_min}

g <- function(x) {
  dat <- data.frame(emission = 3, t = 0:82)
  dat$emission[i] <- dat$emission[i-1] * x[1]
  y[y < .2] <- f2(x[y < 2])
  y[y <= e_min] <- f3(x[y <= e_min])
  
}

# Optimierung ####

emis_by <- 3.03870921601875
base_year <- 2020
budget <- 22 #21.8043361981087
emis_neg <- -.2
rr_20 <- -0.0217324116377584
threshold_linear_rm1 <- 0.136741914720844000
threshold_linear_other <- 0.106354822560656
threshold_horizontal <- -0.2

make_linear <- function(x, rm) {
  for(i in 3:length(x)) {
    if(rm == "rm1") {
      if(x[i-1] <= threshold_linear_rm1) {
        x[i] <- x[i-1] + x[i-1] - x[i-2]
      }
    } else {
      if(x[i-1] <= threshold_linear_other) {
        x[i] <- x[i-1] + x[i-1] - x[i-2]
      }
    }
  }
  return(x)
}

make_horizontal <- function(x) {
  for(i in 3:length(x)) {
    if(x[i] <= threshold_horizontal) {
      x[i] <- threshold_horizontal
    }
  }
  return(x)
}

optimize_function <- function(fun, neg) {
  if(neg == T) {
    xstart <- matrix(runif(800, min = -1, max = 0), ncol = 1)
  } else {
    xstart <- matrix(runif(800, min = 0, max = 1), ncol = 1)
  }
  opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog",  control = list(btol = 1e-6))
  return(opt_x)
}

# RM-1 const ####

fun <- function(x){
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100
  for(i in 2:82){
    emis[i] <- emis[i-1] * (1 + x)
  } 
  emis <- make_linear(emis, rm = "rm1")
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = T)

fun(x = opt_x[[1]][1])
opt_x[[1]][1]

# Test

x <- -0.09278078498705640000

dat <- data.frame(emission = emis_by, t = 0:81, year = 2019:2100)
for(i in 2:82){
  dat$emission[i] <- dat$emission[i-1] * (1 + x)
} 
dat$emission <- make_linear(dat$emission, rm = "rm1")
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1]) # 22.495188954231

# Check

dat <- data.frame(emission = emis_by, t = 0:81, year = 2019:2100)
for(i in 2:82){
  dat$emission[i] <- dat$emission[i-1] * (1 + opt_x[[1]][which(opt_x[[1]] < 0)]) # only allow negative x
} 
dat$emission <- make_linear(dat$emission, rm = "rm1")
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1])
all.equal(sum(dat$emission[-1]), budget)

# Plot

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()



# RM-2 exp ####

# RR = RR_t-1 * (1 + a)

fun <- function(x){

  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100  
  rr <- rep(rr_20, 82)
  
  for(i in 2:82){
    rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] * (1 + x))
    emis[i] <- emis[i-1] * (1 + rr[i])
  } 
  emis <- make_linear(emis, rm = "rm2")
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = F)

fun(x = .6)
fun(x = opt_x[[1]][1])
opt_x[[1]][1]


# Test

x <- 0.163923990121359

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] * (1 + x))
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm2")
dat$emission <- make_horizontal(dat$emission)

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()

sum(dat$emission[-1]) # 21.93514822285140

# Check

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] * (1 + opt_x[[1]][which(opt_x[[1]] > 0)])) # only allow positive x
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm2")
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1])
all.equal(sum(dat$emission[-1]), budget)

# Plot

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()


# RM-3 lin ####

# RR = RR_t-1 * (1 + a)

fun <- function(x){
  
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100  
  rr <- rep(rr_20, 82)
  
  for(i in 2:82){
    rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] + x)
    emis[i] <- emis[i-1] * (1 + rr[i])
  } 
  emis <- make_linear(emis, rm = "rm3")
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = T)

opt_x[[1]]
fun(x = .6)
fun(x = opt_x[[1]][1])


# TEST

x <- -0.00899369958670053

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] + x)
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm3")
dat$emission <- make_horizontal(dat$emission)

all.equal(sum(dat$emission[-1]), budget)
sum(dat$emission[-1]) # 22.3150808047117

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()


# Check

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] + opt_x[[1]][which(opt_x[[1]] < 0)]) # only allow negative x
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm3")
dat$emission <- make_horizontal(dat$emission)

all.equal(sum(dat$emission[-1]), budget)

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()


# RM-4 quadr ####

# RR = a * (t - (BY + 1))^2 + RR_BY+1

fun <- function(x){
  
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100  
  rr <- rep(rr_20, 82)
  
  for(i in 2:82){
    rr[i] <- ifelse(year[i] == 2020, rr_20, x * (year[i] - base_year)^2 + rr_20)
    emis[i] <- emis[i-1] * (1 + rr[i])
  } 
  emis <- make_linear(emis, rm = "rm4")
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = T)

opt_x[[1]]
fun(x = opt_x[[1]][1])


# Check

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, opt_x[[1]][which(opt_x[[1]] < 0)] * (dat$year[i] - base_year)^2 + rr_20) # only allow negative x
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm4")
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1])
all.equal(sum(dat$emission[-1]), budget)

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()


# RM-5 rad ####

# RR = a * sqrt(t - (BY + 1) - 0.5) + RR_BY+1

fun <- function(x){
  
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100  
  rr <- rep(rr_20, 82)
  
  for(i in 2:82){
    rr[i] <- ifelse(year[i] == 2020, rr_20, x * sqrt(year[i] - 0.5 - base_year) + rr_20)
    emis[i] <- emis[i-1] * (1 + rr[i])
  } 
  emis <- make_linear(emis, rm = "rm5")
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = T)

opt_x[[1]]
fun(x = opt_x[[1]][1])


# Check

dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
for(i in 2:82){
  dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, opt_x[[1]][which(opt_x[[1]] < 0)] *  sqrt(dat$year[i] - 0.5 - base_year) + rr_20) # only allow negative x
  dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
} 
dat$emission <- make_linear(dat$emission, rm = "rm5")
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1])
all.equal(sum(dat$emission[-1]), budget)

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()


## WIESO HIER CORRECTING FACTOR???


# RM-6 abs ####

fun <- function(x){
  
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100  

  for(i in 2:82){
    emis[i] <- emis[i-1] + x
  } 
  emis <- make_horizontal(emis)
  
  ret <- numeric(1)
  ret[1] <- sum(emis[-1]) - budget
  return(ret)
}

opt_x <- optimize_function(fun, neg = T)

# Check

dat <- data.frame(emission = emis_byabs, year = 2019:2100)
for(i in 2:82){
  dat$emission[i] <- dat$emission[i-1] + opt_x[[1]][which(opt_x[[1]] < 0)] # only allow negative x
} 
dat$emission <- make_horizontal(dat$emission)

sum(dat$emission[-1])
all.equal(sum(dat$emission[-1]), budget)

dat %>%
  ggplot(aes(x = year, y = emission)) +
  geom_line()
