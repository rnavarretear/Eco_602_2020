getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_04")
getwd()

dnorm(x = -1.96, mean = 0, sd = 1)
pnorm(q = -1.96, mean = 0, sd = 1)

dnorm(x = -1,mean = 0, sd = 1)
pnorm(q = -1,mean = 0, sd = 1)

dnorm(x = 0,mean = 0, sd = 1)
pnorm(q = 0,mean = 0, sd = 1)

dnorm(x = 1.96,mean = 0, sd = 1)
pnorm(q = 1.96,mean = 0, sd = 1)

# Generate a vector of x-values
?seq
?dnorm
?abline
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

?set.seed
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)
?runif
?rnorm

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


plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)
?plot

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat$x, guess_x, guess_y, guess_slope)

y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)
y_predicted

dat$y_predicted <- y_predicted
dat

?abs

resids = dat$y_observed-dat$y_predicted
resids

dat$resids <- resids

dat

#Question 1: Random Normal 1
#Directions:
  
# Create three vectors of normally-distributed random numbers, norm_17, norm_30, and norm_300.
#You should tell R that you want your random deviates have a mean of 10.4 and a standard deviation of 2.4.
#norm_17 should have 17 elements, norm_30 should have 30 elements, and norm_300 should have 300 elements.

?rnorm

mean1 = 10.4
sd1 = 2.4

norm_17 <- rnorm(n = 17,mean = mean1, sd = sd1)
norm_17

norm_30 <- rnorm(n = 30,mean = mean1, sd = sd1)
norm_30

norm_300 <- rnorm(n = 300,mean = mean1, sd = sd1)
norm_300

#Q1 Paste the R code you used to create your vectors into the text entry box.

#Question 2: Random Normal 1B
#Directions:
  
#Create a figure including histograms of your vectors norm_17, norm_30, and norm_300.
#The three histograms should all be on the same figure, arranged vertically (3 rows, 1 column).
#Each histogram must have an informative title that indicates how many randomly generated data points were used to build the histogram.
#Save your figure to a file called lab_04_hist_01.png.
#Your figure should be 1400 pixels high, and 700 pixels wide.
#It should have a resolution of 180 dpi.

?png

png(
  "lab_04_hist_01.png", 
  width = 700, 
  height = 1400,
  units = "px",
  res = 180,
  bg = "transparent")

par(mfrow = c(3, 1))

hist(
  norm_17,
  main = "Histogram of 17 normally distributed random numbers",
  xlab = "Value",
  ylab = "Frequency",
  col = "greenyellow"
)

hist(
  norm_30,
  main = "Histogram of 30 normally distributed random numbers",
  xlab = "Value",
  ylab = "Frequency",
  col = "dodgerblue"
)

hist(
  norm_300,
  main = "Histogram of 300 normally distributed random numbers",
  xlab = "Value",
  ylab = "Frequency",
  col = "darkorange"
)


dev.off()

getwd()

#Q1 (2 pts) Paste the R code you used to create your figure into the text entry box.
#Q2 (1 pt) Upload your figure image file into the file upload box.
#You’ll need to check out the help entry for the appropriate arguments to png() to set the width, height, and resolution.
#If you get a strange gray border around your image, try setting the background color argument to transparent: bg = "transparent".

#Question 3: Normal Density 2
#Consider the figure with three histograms you created.

#Q1 Qualitatively describe the differences in the patterns you observe.
#Q2 Explain why they histograms are different.

#Question 4: Normal Density 3
#Remember the code I used to create a plot of the density function of a standard normal distribution:
  
#  Instructions

#Use the code above as a template to plot a density curve for a normal distribution with:
#  mean = 10.4
#standard deviation = 2.4
#Include an informative title that states the mean and standard deviation values you used.
# Generate a vector of x-values

mean1 = 10.4
sd1 = 2.4

png(
  "norm_1.png", 
  width = 1400, 
  height = 1100,
  units = "px",
  res = 180,
  bg = "transparent")

par(mfrow = c(1, 1))

x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean = mean1, sd = sd1)

plot(x, y, main = "Normal Standard Distribution
     Mean = 10.4
Standard deviation = 2.4", type = "l")
abline(h = 0)

dev.off()

?dnorm

#Save your figure to a file called norm_1.png.
#Hint: check out the help entry for dnorm() for a description of the arguments.
#Q1 (1 pt). What are the parameters and their values for the standard Normal distribution?
#  Q2 (2 pts). Paste the R code you used to create and save your figure into the text-entry window.
#Q1 (1 pts). Paste the image file of your figure into the file entry box.

#Question 5: Simulated Data 1
#Instructions:
  
# Review the section of the lab instructions in which I created a data.frame of random data.
#Experiment with creating and plotting random data:
#  Try different numbers of points.
#Try different plotting characters.
#Try using set.seed() with different seed values.

set.seed(17)
n_pts1 = 10
x_min1 = 1
x_max1 = 10
x1 = runif(n = n_pts1, min = x_min1, max = x_max1)

dat1 = data.frame(x = x1, y_observed = rnorm(n_pts1))
dat1

set.seed(35)
n_pts2 = 28
x_min2 = 10
x_max2 = 100
x2 = runif(n = n_pts2, min = x_min2, max = x_max2)

dat2 = data.frame(x = x2, y_observed = rnorm(n_pts2))
dat2

set.seed(47)
n_pts3 = 55
x_min3 = 1
x_max3 = 100
x3 = runif(n = n_pts3, min = x_min3, max = x_max3)

dat3 = data.frame(x = x3, y_observed1 = rnorm(n_pts3))
dat3

set.seed(74)
n_pts4 = 111
x_min4 = 100
x_max4 = 1000
x4 = runif(n = n_pts4, min = x_min4, max = x_max4)

dat4 = data.frame(x = x4, y_observed1 = rnorm(n_pts4))
dat4

png("sim_data_scatterplots.png", width = 1300, height = 800)

par(mfrow = c(2, 2))
plot(dat1$x,
     dat1$y_observed,
     main = "Dataset 1
     10 x points from 1 to 10",
     xlab = "x",
     ylab = "y",
     col = 2,
     pch = 4)

plot(dat2$x,
     dat2$y_observed,
     main = "Dataset 2
     28 x points from 10 to 100",
     xlab = "x",
     ylab = "y",
     col = 3,
     pch = 7)

plot(dat3$x,
     dat3$y_observed,
     main = "Dataset 3
     55 x points from 1 to 100",
     xlab = "x",
     ylab = "y",
     col = 4,
     pch = 11)

plot(dat4$x,
     dat4$y_observed,
     main = "Dataset 4
     111 x points from 100 to 1000",
     xlab = "x",
     ylab = "y",
     col = 5,
     pch = 14)

dev.off()

#Create scatterplots of four of your favorite random data sets.
#Arrange your scatterplots in a single figure: it should be a 2 by 2 grid of scatterplot panels.
#Save your figure to a file called sim_data_scatterplots.png
#Q1 (3 pts.) Paste the R code you used to create one of the random datasets in your figure into the text entry box.
#Q2 (3 pts.) Paste the R code you used to create your figure into the text entry box.
#Q3 (1 pt.) Paste your figure file into the file entry box.

#Question 6: Model Fit 1
#Instructions:
  
#Choose one of your datasets from the previous question.
#Using the code in the lab walkthrough, visually fit a linear deterministic function through the data. Make sure you save your parameters to variables so you can use them in the next question.
#Create a plot of your simulated data and the line of your fitted model.

png("dataset_linear_function.png", width = 1300, height = 800)

par(mfrow = c(1, 1))

plot(dat2$x,
     dat2$y_observed,
     main = "Dataset 2
     28 x points from 10 to 100",
     xlab = "x",
     ylab = "y",
     col = 3,
     pch = 7
)

curve(
  line_point_slope(x, 
                   x1 = 67, 
                   y1 = 0.5, 
                   slope = 0.044), 
  add = TRUE
)

dev.off()

#Q1 (3 pts.) Paste the R code you used to fit your model.
#Q3 (1 pt.) Paste your figure file into the file entry box.

#Question 7: Residuals 1
#Instructions:
  
#Use the dataset you chose for the previous question.
#Using the code in the lab walkthrough, create a column of predicted y-values.
#Using the code in the lab walkthrough, create a column of residuals.

set.seed(35)
n_pts2 = 28
x_min2 = 10
x_max2 = 100
x2 = runif(n = n_pts2, min = x_min2, max = x_max2)

dat2 = data.frame(x = x2, y_observed = rnorm(n_pts2))
dat2

plot(dat2$x,
     dat2$y_observed,
     main = "Dataset 2",
     xlab = "x",
     ylab = "y",
     col = 3,
     pch = 7
)

curve(
  line_point_slope(x, 
                   x1 = 67, 
                   y1 = 0.5, 
                   slope = 0.044), 
  add = TRUE
)


dat2$y_predicted = line_point_slope(dat2$x, 
                               67, 
                               0.5, 
                               0.044)
y_predicted

dat2$y_predicted <- y_predicted
dat2

resids2 = dat2$y_observed-dat2$y_predicted
resids2

dat2$resids <- resids2

dat2

hist(dat2$resids)

plot(dat2$x,
     dat2$resids,
     main = "Dataset 2",
     xlab = "x",
     ylab = "y",
     col = 3,
     pch = 7
)

#Q1 (2 pts.) Paste the R code you used to create create the columns of predicted values and residuals.
