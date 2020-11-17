getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_06")

getwd()


sse_mean = function(x, a, b)
{
  return(sd(x)/(b^0.5))
}

sse_mean(25, 45)

install.packages("palmerpenguins")
require(palmerpenguins)
palmerpenguins
sse_mean(penguins$bill_depth_mm)

?is.na()

sse_mean = function (x) 
{
  n = length(x)-length(x[is.na(x)])
  standard_dev = sd(x, na.rm = TRUE)
  return(
    standard_dev / (n^0.5)
  )
}

boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

dev.off()

set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

?round

diff(t_test$estimate)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

?aggregate

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152


two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  
  return(mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
)
}

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

str(t_test)

t_test = t.test(flipper_shuffled ~ dat_pen$species)

t_test$estimate

#Lab Questions
#Question 01: Standard error of the mean function
#Recall your sse_mean() function. Your function should accept a single numeric vector and return a numeric value:

sse_mean = function (x) 
{
  n = length(x)-length(x[is.na(x)])
  standard_dev = sd(x, na.rm = TRUE)
  return(
    standard_dev / sqrt(n)
  )
}

?sd

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Question 02: Two-sample resampling function
#Your two_group_resample() funciton should accpet three arguments;

#x: A numeric vector
#n_1: The number of values (with replacement) to take from x for the first group.
#n_2: The number of values (with replacement) to take from x for the second group.
#Your function should return:
  
 #A single numeric value: the difference in means between the two groups

two_group_resample = function(x, n_1, n_2) 
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  
  return(diff_simulated
  )
}

two_group_resample(dat_pen$flipper_length_mm,68,152)

#Question 03: Resampling penguin data: histogram
#Use your two_group_resample() function along with a loop to generate 2000 resampled differences of means for flipper length between the two penguin species.

#-Hint: see the Resampling simulations section in the lab walkthrough for an example of how to do this.

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences,
     main = "Histogram of the resampled differences of means"
     )

#Question 04: Resampling penguin data: significance
#Use your two_group_resample() function along with a loop to generate 2000 resampled differences of means for flipper length between the two penguin species.

#-Hint: see the Resampling simulations section in the lab walkthrough for an example of how to do this.

m = 2000
mean_differences = c()
for (i in 1:m)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}

sum_dif = sum(abs(mean_differences) >= 5.8)
sum_dif

#sum_dif_neg = sum(abs(mean_differences) < -5.8)

#sum_dif_tot = sum_dif_pos + sum_dif_neg

#sum_dif_tot

#Question 05: Resampling: p-value
#Recall this interpretation of a Frequentist p-value:
  
#How often would I expect to see a result as extreme, or more extreme, if the null hypothesis were true.

#The p-value from the t-test for the difference in mean flipper length was very very small:
  
#6.049e-08
#That’s less than once in 10 million!
  # Question
#Given a p value of less than 1 per 10 million, how many simulations do you think you would have to do to see a difference in mean flipper length equal to or greater than 5.8 mm?

#Answer: 10 million

#Question 06: Resampling
#Try out your two_group_resample() on one of the other variables in the penguin data set.

#Instructions:
#  Choose another variable of penguin data from dat_pen.
#Create a boxplot of the data from your chosen column in dat_pen, grouped by species.
#See the walkthrough
#Use aggregate() to calculate the group means and the difference in means.
#Write the difference in means into a variable called diff_crit
#Conduct a t-test and observe p-value.
#Conduct a resampling test with 1000 repetitions using your two_group_resample() function.

dat_pen
boxplot(body_mass_g ~ species, data = dat_pen)

agg_means1 = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_crit = diff(agg_means1[, 2])

agg_means1
diff_crit

t_test1 = t.test(dat_pen$body_mass_g ~ dat_pen$species)

str(t_test1)

k = 1000
mean_differences1 = c()
for (i in 1:k)
{
  mean_differences1 = c(
    mean_differences1,
    two_group_resample(dat_pen$body_mass_g, 68, 152)
  )
}

sum_dif1 = sum(abs(mean_differences1) >= diff_crit)
sum_dif1
hist(mean_differences1)

