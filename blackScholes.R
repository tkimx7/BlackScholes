#.rs.restartR()
rm(list = ls())

library(DBI)
library(doltr)
library(tictoc)
library(doSNOW)
library(foreach)
library(tidyverse)
library(doParallel)
library(data.table)

options(digits = 15)

################################################################################
### https://quant.stackexchange.com/questions/7761/a-simple-formula-for-calculating-implied-volatility
### https://quant.stackexchange.com/questions/58634/newtons-algorithm-for-implied-volatility
### https://quant.stackexchange.com/questions/45514/why-isnt-this-iv-calc-correct/45515#45515
### https://math.stackexchange.com/questions/2223296/cdf-of-standard-normal
### https://quant.stackexchange.com/questions/45514/why-isnt-this-iv-calc-correct/45515#45515
### A&S Standard Normal Definition 26.2.1
### 
################################################################################

N <- function(d_) {
  
  c1 <- 0.5 # 1/2
  c2 <- 0.398942280401433 # 1/(sqrt(2*pi))
  c3 <- -0.0664903800669055 # -1/(6 * sqrt(2*pi))
  c4 <- 0.00997355701003582 # 1/(40 * sqrt(2*pi))
  c5 <- -0.00118732821548045 # -1/(336 * sqrt(2*pi))
  c6 <- 0.000115434687616155 #1/(3456 * sqrt(2*pi))
  
  Z  <- c6*d_^9 + c5*d_^7 + c4*d_^5 + c3*d_^3 + c2*d_^1 + c1
  
  return(Z)
}

V <- function(type_ = "c", s_ = 300, k_ = 250, r_ = 0, t1_ = 1, t0_ = 0, v_ = 0.15) {
  
  # https://goodcalculators.com/black-scholes-calculator/
  
  t_     <- as.numeric(t1_-t0_)
  d1_    <- (log(s_/k_) + (r_ + 1/2 * v_^2) * t_)  / (v_ * sqrt(t_))
  d2_    <- d1_ - v_ * sqrt(t_)
  
  c_     <<- s_ * N(d1_) - k_ * exp(-r_ * t_) * N(d2_)
  p_     <<- k_ * exp(-r_ * t_) * N(-d2_) - s_ * N(-d1_)
  
  if (type_ == "c") {
   
    f_ = c_ 
  }
  else {
    
    f_ = p_
  }
  parity <<- s_ - k_* exp(-r_ * t_) 
  
  return(f_)
}
