install.packages("here")
require("here")

getwd()
here()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lec_03")

getwd()
here()

hab <- c(read.csv(here("data", "hab.sta.csv")))
hab

file.exists(here("data", "data_sets", "hab.sta.csv"))

file.exists(here("data", "hab.sta.csv"))

png("Elevation.png", width = 1300, height = 800)

hist(hab$elev,
     main = "Frequency of Elevation (m)",
     xlab = "Elevation (m)",
     xlim = c(0,1000), 
     ylim = c(0,200),
     col = "darkolivegreen4")

dev.off()

png("Aspect.png", width = 1300, height = 800)

hist(hab$aspect,
     main = "Frequency of Aspect (degrees)",
     xlab = "Aspect (degrees)",
     xlim = c(0,400), 
     ylim = c(0,200),
     col = "darkorange3")

dev.off()

?hist
png("Slope.png", width = 1300, height = 800)

hist(hab$slope,
     main = "Frequency of Slope (%)",
     xlab = "Slope (%)",
     xlim = c(0,120), 
     ylim = c(0,200),
     col = "dodgerblue4")

dev.off()

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

png("ElevationBasalArea.png", width = 1300, height = 800)

par(mfrow = c(1, 3))
plot(hab$elev,
     hab$ba.tot,
     main = "Elevation vs. Total Basal Area",
     xlab = "Elevation (m)",
     ylab = "Total Basal Area (m^2 per ha)",
     col = 2)
curve(
  line_point_slope(x, 
                   x1 = 370, 
                   y1 = 20, 
                   slope = 0.07), 
  add = TRUE)



plot(hab$aspect,
     hab$ba.tot,
     main = "Aspect vs. Total Basal Area",
     xlab = "Aspect (degrees)",
     ylab = "Total Basal Area (m^2 per ha)",
     col = 3)
curve(
  line_point_slope(x, 
                   x1 = 150, 
                   y1 = 17, 
                   slope = 0.01), 
  add = TRUE)



plot(hab$slope,
     hab$ba.tot,
     main = "Slope vs. Total Basal Area",
     xlab = "Slope (%)",
     ylab = "Total Basal Area (m^2 per ha)",
     col = 4)
curve(
  line_point_slope(x, 
                   x1 = 40, 
                   y1 = 22, 
                   slope = 0.47), 
  add = TRUE)

dev.off()
