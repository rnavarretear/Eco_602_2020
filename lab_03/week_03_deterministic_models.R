getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lec_03")
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_03")

install.packages("psych")
require(psych)
pairs.panels(iris)

dat_birds <- data.frame(read.csv(here("data","bird.sta.csv")))
dat_habitat <- data.frame(read.csv(here("data","hab.sta.csv")))

?merge

dat_all <- merge(dat_birds, dat_habitat)

nrow(dat_all)
nrow(dat_birds)
nrow(dat_habitat)

ncol(dat_all)
ncol(dat_birds)
ncol(dat_habitat)

dat_birds

plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

census.vec <- c(dat_all$CEWA)
census.vec

presence.vec <- c(census.vec>=1)
presence.vec

cewa_present_absent <- as.numeric(presence.vec)
cewa_present_absent

?pairs
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slopoe and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

#Instructions and Deliverables

#Use the pair plot function from psych to create a pair plot of the three terrain variable and basal area from the lecture questions.
?pairs
pairs(dat_habitat[, c(6:8,17)])

#Choose two bird species and create plots of presence/absence (on the y-axis) and basal area (on the x axes).
#Common Raven=CORA

census.CORA.vec <- c(dat_all$CORA)
census.CORA.vec

presence.CORA.vec <- c(census.CORA.vec>=1)
presence.CORA.vec

CORA_present_absent <- as.numeric(presence.CORA.vec)
CORA_present_absent

png("cora_logistic_function.png", width = 1300, height = 800)

plot(x = dat_all$ba.tot, 
     y = CORA_present_absent,
     main = "Presence or Absence of Common Raven vs. Total Basal Area",
     xlab = "Total Basal Area (m^2 per ha)",
     ylab = "Presence or Absence of Common Raven",
     col = 4)
curve(logistic_midpoint_slope(x, midpoint = 99, slope = -0.67), add = TRUE)

dev.off()

#Rufous Humingbird=RUHU

census.RUHU.vec <- c(dat_all$RUHU)
census.RUHU.vec

presence.RUHU.vec <- c(census.RUHU.vec>=1)
presence.RUHU.vec

RUHU_present_absent <- as.numeric(presence.RUHU.vec)
RUHU_present_absent

png("ruhu_logistic_function.png", width = 1300, height = 800)

plot(x = dat_all$ba.tot, 
     y = RUHU_present_absent,
     main = "Presence or Absence of Rufous Humingbird vs. Total Basal Area",
     xlab = "Total Basal Area (m^2 per ha)",
     ylab = "Presence or Absence of Rufous Humingbird",
     col = 3)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.8), add = TRUE)

dev.off()

#Question 3: Counting total jays
#Using the dat_all data frame, calculate the total number of Gray Jays observed in all of the sampling sites.

#Q1 (1pt): Past the R code you used for the calculation into the text entry window.

#1st option

sum(dat_all$GRJA)

#2nd option

GRJA <- c(dat_all$GRJA)
GRJA
 sum(GRJA)

# Question 4: Counting total sites with jays
# Using the dat_all data frame, calculate the total number of sampling sites in which Gray Jays were observed.
 
# Hint: What happens when you use the sum() function on a vector of Boolean values?
   
#   Q1 (1pt): Past the R code you used for the calculation into the text entry window.
 
 census.GRJA.vec <- c(dat_all$GRJA)
 census.GRJA.vec
 
 presence.GRJA.vec <- c(census.GRJA.vec>=1)
 presence.GRJA.vec
 
 sum(presence.GRJA.vec) 
 
 #or
 
 GRJA_present_absent <- as.numeric(presence.GRJA.vec)
 GRJA_present_absent
 
 sum(GRJA_present_absent)
 
mean(dat_all$elev)
sd(dat_all$elev)

mean(dat_all$elev)+(1.96*sd(dat_all$elev))
 
 
25*25*25
 