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
budget <- 21.8043361981087
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

# RM-1 const ####

fun <- function(x){
  dat <- data.frame(emission = emis_by, t = 0:81, year = 2019:2100)
  for(i in 2:82){
    dat$emission[i] <- dat$emission[i-1] * (1 + x)
  } 
  dat$emission <- make_linear(dat$emission, rm = "rm1")
  dat$emission <- make_horizontal(dat$emission)

  ret <- numeric(1)
  ret[1] <- sum(dat$emission[-1]) - budget
  return(ret)
}

xstart <- matrix(runif(800, min=-1, max=0), ncol = 1)
opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "pwldog", control = list(btol = 1e-6))

fun(x = .6)
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
  dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
  for(i in 2:82){
    dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] * (1 + x))
    dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
  } 
  dat$emission <- make_linear(dat$emission, rm = "rm2")
  dat$emission <- make_horizontal(dat$emission)
  
  ret <- numeric(1)
  ret[1] <- sum(dat$emission[-1]) - budget
  return(ret)
}

xstart <- matrix(runif(800, min = 0, max = 1), ncol = 1)
opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "pwldog", control = list(btol = 1e-6))

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
  dat <- data.frame(emission = emis_by, t = 0:81, rr = rr_20, year = 2019:2100)
  for(i in 2:82){
    dat$rr[i] <- ifelse(dat$year[i] == 2020, rr_20, dat$rr[i-1] + x)
    dat$emission[i] <- dat$emission[i-1] * (1 + dat$rr[i])
  } 
  dat$emission <- make_linear(dat$emission, rm = "rm3")
  dat$emission <- make_horizontal(dat$emission)
  
  ret <- numeric(1)
  ret[1] <- sum(dat$emission[-1]) - budget
  return(ret)
}

xstart <- matrix(runif(800, min = -1, max = 0), ncol = 1)
opt_x <- searchZeros(xstart, fun,  method = "Broyden", global = "dbldog",  control = list(btol = 1e-6))

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

# Plot

dat %>%
  ggplot(aes(x = t, y = emission)) +
  geom_line()
