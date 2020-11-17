install.packages("here")
here()
require(here)
here()
getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_03")
getwd()
here()
read.csv(here("data","bird.sta.csv"))
read.csv(here("data","hab.sta.csv"))
file.exists(here("data", "my_data.csv"))
file.exists(here("data","hab.sta.csv"))

dat_habitat <- data.frame(read.csv(here("data","hab.sta.csv")))
dat_habitat
hist(dat_habitat$elev)
hist(dat_habitat$slope)
hist(dat_habitat$aspect)

plot(dat_habitat$elev, dat_habitat$ba.tot, main = "Elevation vs. basal area")
plot(dat_habitat$slope, dat_habitat$ba.tot, main = "Slope vs. basal area")
plot(dat_habitat$aspect, dat_habitat$ba.tot, main = "Aspect vs. basal area")
qqplot(dat_habitat$aspect,dat_habitat$elev)

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

curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.4), add = TRUE)
curve(sin(x)+100, add=TRUE)
