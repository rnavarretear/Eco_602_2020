getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_05")

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")


exp_fun = function(x, a, b) 
{
  return(a * exp(b * x))
}


curve(
  exp_fun(x, 1, 1), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 1, -1), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function: a = 1, b = -1",
  ylab = "f(x)", xlab = "x")

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)

dev.off()

set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed)

set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
plot(x_sim, y_observed_2)

?rexp

set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

rexp(n = x_sim, rate = 1.2)

y_observed_3 = 
  y_pred +
  rexp(n = x_sim, 
       rate = 1.2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")

read.csv("salamander_dispersal.csv")

#Question 1
#Q1

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

#Q2

{
curve(
  exp_fun(x, 
          1.9,
          0.1
  ),
          from = 0, 
          to = 35, 
          add = FALSE,
          ylab = "f(x)", 
          xlab = "x"
)
curve(
  exp_fun(x, 
          1.9,
          0.3
  ),
          from = 0, 
          to = 35, 
          add = TRUE,
          ylab = "f(x)", 
          xlab = "x",
          col = 1,
          lty = 2 
)
curve(
  exp_fun(x, 
          1.2,
          0.2
  ),
          from = 0, 
          to = 35, 
          add = TRUE,
          ylab = "f(x)", 
          xlab = "x",
          col = 2,
          lty = 1 
)
curve(
  exp_fun(x, 
          1.2,
          0.4
  ),
          from = 0, 
          to = 35, 
          add = TRUE,
          ylab = "f(x)", 
          xlab = "x",
          col = 2,
          lty = 2
)
}

#Question 2
#•	Q1 (2 pts.) Qualitatively describe what happens to the curve as you vary parameter a
#•	Q2 (2 pts.) Qualitatively describe what happens to the curve as you vary parameter b

#Question 3

#Q1

curve(
  ricker_fun(x, 
             25, 
             0.1
             ),
  add = FALSE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 1,
  lty = 1
)
curve(
  ricker_fun(x, 
             20, 
             0.2
  ),
  add = TRUE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 1,
  lty = 2
)
curve(
  ricker_fun(x, 
             10, 
             0.2
  ),
  add = TRUE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 1,
  lty = 2
)
curve(
  ricker_fun(x, 
             75, 
             0.3
  ),
  add = TRUE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 2,
  lty = 1
)
curve(
  ricker_fun(x, 
             50, 
             0.3
  ),
  add = TRUE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 2,
  lty = 2
)
curve(
  ricker_fun(x, 
             40, 
             0.3
  ),
  add = TRUE,
  from = 0,
  to = 55,
  ylab = "f(x)", 
  xlab = "x",
  col = 2,
  lty = 2
)

#Question 4
#Q1 (2 pts.) Qualitatively describe how the a parameter influences the curve shape.
#Q2 (2 pts.) Qualitatively describe how the b parameter influences the curve shape.

#Question 5

#Q1

dat_dispersal <- data.frame(read.csv("salamander_dispersal.csv"))
dat_dispersal

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlab = "Distance Class", 
  ylab = "Dispersal Rate of First Time Breeders",
  main = "Marbled Salamander
  Distance Class vs. Dispersal Rate of First Time Breeders")

#Question 6

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlab = "Distance Class", 
  ylab = "First Time Breeders Dispersal Rate",
  main = "Marbled Salamander - Linear Model
  Distance Class vs. First Time Breeders Dispersal Rate")
curve(
  line_point_slope(x,
                   800,
                   0.175,
                   -0.0003),
  add = TRUE
)

#Question 7

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlab = "Distance Class", 
  ylab = "First Time Breeders Dispersal Rate",
  main = "Marbled Salamander - Exponential Curve Model
  Distance Class vs. First Time Breeders Dispersal Rate")
curve(
  exp_fun(x,
          0.95,
          0.0033
  ),
  add = TRUE
)

#Question 8

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlab = "Distance Class", 
  ylab = "First Time Breeders Dispersal Rate",
  main = "Marbled Salamander - Ricker Curve Model
  Distance Class vs. First Time Breeders Dispersal Rate")
curve(
  ricker_fun(x,
          0.0065,
          0.0049
  ),
  add = TRUE
)

#Question 9

line_point_slope(x,
                 800,
                 0.175,
                 -0.0003
)

resids_linear = 
  line_point_slope(
    dat_dispersal$dist.class, 
    800, 0.175,
    -0.0003) - 
  dat_dispersal$disp.rate.ftb

hist(resids_linear, main = "Histogram of residuals obtained with the linear model")

#Question 10

exp_fun(x,
        0.95,
        0.0033
)

resids_exp = 
  exp_fun(
    dat_dispersal$dist.class, 
    0.95, 
    0.0033) - 
  dat_dispersal$disp.rate.ftb

hist(resids_exp, main = "Histogram of residuals obtained with the exponential model")

#Question 11

ricker_fun(x,
           0.0065,
           0.0049
)

resids_ricker = 
  ricker_fun(
    dat_dispersal$dist.class, 
    0.0065, 
    0.0049) - 
  dat_dispersal$disp.rate.ftb 
hist(resids_ricker, , main = "Histogram of residuals obtained with the Ricker model")

