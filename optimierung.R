library(dplyr)
library(ggplot2)
library(nleqslv)
library(bench)


# Optimierung ####

emis_by <- 3.03870921601875 
base_year <- 2020
budget <- 21.827 #22 #21.8043361981087
rr_20 <- -0.0217324116377584
threshold_linear_rm1 <- 0.136741914720844000
threshold_linear_other <- 0.106354822560656
threshold_horizontal <- -0.2430967

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

create_fun <- function(rm) {
  fun <<- function(x){
    emis <- rep(emis_by, 82)
    t <- 0:81
    year <- 2019:2100
    rr <- rep(rr_20, 82)
    for(i in 2:82){
      if(rm == "rm1") {
        emis[i] <- emis[i-1] * (1 + x)
      } else if (rm == "rm2") {
        rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] * (1 + x))
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm3") {
        rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] + x)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm4") {
        rr[i] <- ifelse(year[i] == 2020, rr_20, x * (year[i] - base_year)^2 + rr_20)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm5") {
        rr[i] <- ifelse(year[i] == 2020, rr_20, x * sqrt(year[i] - 0.5 - base_year) + rr_20)
        emis[i] <- emis[i-1] * (1 + rr[i])
      } else if (rm == "rm6") {
        emis[i] <- emis[i-1] + x
      }
    } 
    
    emis <- make_linear(emis, rm = rm)
    emis <- make_horizontal(emis)
    
    ret <- numeric(1)
    ret[1] <- sum(emis[-1]) - budget
    return(ret)
  }
}

calculate_result <- function(x, rm) {
  emis <- rep(emis_by, 82)
  t <- 0:81
  year <- 2019:2100
  rr <- rep(rr_20, 82)
  for(i in 2:82){
    if(rm == "rm1") {
      emis[i] <- emis[i-1] * (1 + x)
    } else if (rm == "rm2") {
      rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] * (1 + x))
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "rm3") {
      rr[i] <- ifelse(year[i] == 2020, rr_20, rr[i-1] + x)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "rm4") {
      rr[i] <- ifelse(year[i] == 2020, rr_20, x * (year[i] - base_year)^2 + rr_20)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "rm5") {
      rr[i] <- ifelse(year[i] == 2020, rr_20, x * sqrt(year[i] - 0.5 - base_year) + rr_20)
      emis[i] <- emis[i-1] * (1 + rr[i])
    } else if (rm == "rm6") {
      emis[i] <- emis[i-1] + x
    }
  } 
  
  emis <- make_linear(emis, rm = rm)
  emis <- make_horizontal(emis)
  
  dat <- data.frame(t = t,
                    year = year,
                    emissions = emis,
                    rr = rr)
  return(dat)
}

plot_result <- function(x) {
  ggplot(x, aes(x = year, y = emissions)) +
    geom_line()
}


# RM-1 const ####

create_fun(rm = "rm1")

opt_x <- optimize_function(fun, neg = T)

result_rm1 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)],
                 rm = "rm1")

plot_result(result_rm1)


# RM-2 exp ####

# RR = RR_t-1 * (1 + a)

create_fun(rm = "rm2")

opt_x <- optimize_function(fun, neg = F)

result_rm2 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] > 0)], # only allow positive x
                               rm = "rm2")

plot_result(result_rm2)


# RM-3 lin ####

# RR = RR_t-1 * (1 + a)

create_fun(rm = "rm3")

opt_x <- optimize_function(fun, neg = T)

result_rm3 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)], # only allow negative x
                               rm = "rm3")

plot_result(result_rm3)


# RM-4 quadr ####

# RR = a * (t - (BY + 1))^2 + RR_BY+1

create_fun(rm = "rm4")

opt_x <- optimize_function(fun, neg = T)

result_rm4 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)], # only allow negative x
                               rm = "rm4")

plot_result(result_rm4)


# RM-5 rad ####

# RR = a * sqrt(t - (BY + 1) - 0.5) + RR_BY+1

create_fun(rm = "rm5")

opt_x <- optimize_function(fun, neg = T)

result_rm5 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)], # only allow negative x
                               rm = "rm5")

plot_result(result_rm5)



## WIESO HIER CORRECTING FACTOR???


# RM-6 abs ####

create_fun(rm = "rm6")

opt_x <- optimize_function(fun, neg = T)

result_rm6 <- calculate_result(x = opt_x[[1]][which(opt_x[[1]] < 0)], # only allow negative x
                               rm = "rm6")

plot_result(result_rm6)
