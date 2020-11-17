getwd()
setwd("/Users/Roberto/Documents/R/eco_634_2020/lab_08")
getwd()

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages("simpleboot")

require(simpleboot)

??two.boot

adelie = subset(penguin_dat, species == "Adelie")
adelie

chinstrap = subset(penguin_dat, species == "Chinstrap")
chinstrap

hist(two.boot(adelie$flipper_length_mm, chinstrap$flipper_length_mm, FUN = mean, R = 10000, na.rm = TRUE))

require(here)

veg = read.csv(here("data", "vegdata.csv"))

boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

boxplot(pine ~ treatment, dat = dat_tree)

?table
table(dat_tree$treatment)

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )
tree_boot

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

require(boot)

boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

dat_all

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")


fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
?lm
coef(fit_1)

slope_observed = coef(fit_1)[2]


plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

dat_1

index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)
?nrow

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

m = 10000 
result = numeric(m)
?numeric

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
 
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2] 
} 

result

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))

#Question 01: flipper bootstrap histogram
#Instructions
#Use the two.boot() function to calculate 10000 bootstrap replicates of the difference in mean flipper length of Chinstrap and Adelie penguins.
#Is there missing data? If so, what argument have we used before in functions like mean() and sd() to exclude NA values?
#  Save the output of two.boot() to a variable called pen_boot. This is so that I can replicate your code easily on my machine for grading and assistance.
#Plot a histogram of your bootstrapped differences and save it to an image file.

pen_boot = 
  two.boot(
    subset(penguins, species == "Adelie")$flipper_length_mm,
    subset(penguins, species == "Chinstrap")$flipper_length_mm,
    FUN = mean,
    R = 10000,
    na.rm = TRUE,
    )
hist(pen_boot$t,
     main = "Histogram of 10000 bootstrap differences in mean flipper length 
     of Adelie and Chinstrap penguins",
     xlab = "Differences in mean flipper length (mm) between
     Adelie and Chinstrap penguins"
     )
pen_boot


#Question 02: flipper bootstrap CI
#Instructions
#Use the two.boot() function to calculate 10000 bootstrap replicates of the difference in mean flipper length of Chinstrap and Adelie penguins.
#Save the output of two.boot() to a variable called pen_boot. This is so that I can replicate your code easily on my machine for grading and assistance.
#Use quantile() with pen_boot to calculate a 95% Confidence interval on the difference in mean flipper lengths.

pen_boot = 
  two.boot(
    subset(penguins, species == "Adelie")$flipper_length_mm,
    subset(penguins, species == "Chinstrap")$flipper_length_mm,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

quantile(pen_boot$t, c(.025, 0.975))

pen_boot

#Question 03: flipper bootstrap empirical distribution
#Use the two.boot() function to calculate 10000 bootstrap replicates of the difference in mean flipper length of Chinstrap and Adelie penguins.
#Save the output of two.boot() to a variable called pen_boot. This is so that I can replicate your code easily on my machine for grading and assistance.
#Create a distribution function from pen_boot using ecdf().
#Name the function created by ecdf() pen_ecdf.
#The ecdf() function is a little different than other functions that we’ve worked with because it returns a new function. You can use the new function to calculate the cumulative density.

pen_ecdf = ecdf(pen_boot$t)
?ecdf
pen_ecdf

#Question 04: flipper length hypotheses
#Instructions
#Consider the alternative and null hypothesis for a two-sample test of the difference in mean flipper length between the two two penguin species.



#Question 05: flipper bootstrap empirical distribution
#Recall that we used bootstrapping to characterize the sampling distribution of the alternative hypothesis.

#Remember the law of total probability and complementary events.

#pen_ecdf() is a lot like pnorm(). It calculates the area under the density curve to the left of x.

#Remember plots like this one that show the shaded area to the left of x for a standard normal distribution?

1 - pen_ecdf(-4.5)


1 - pen_ecdf(0)


#Question 06: pine seedling Wilcoxon test

#Conduct a Wilcoxon ranked sum test on the difference in the mean number of pine seedlings in between the treatments control and clipped treatments.
#You should use the formula notation in R to conduct the test.

wilcox.test(subset(dat_tree, treatment == "clipped")$pine,
            subset(dat_tree, treatment == "control")$pine, 
            alternative = "two.sided")

wilcox.test(dat_tree$pine ~ dat_tree$treatment, 
            alternative = "two.sided")
#This is the formula notation

boxplot(dat_tree$pine)

boxplot(dat_tree$pine ~ dat_tree$treatment)

#Question 07: pine seedling bootstrap CI

#Use two.boot() to create a bootstrapped data set of the differences in mean tree count between the clipped and control treatments.
#Save your results as tree_boot.
#Use quantile() to find a 95% CI.

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

quantile(tree_boot$t, c(.025, 0.975))

mean(subset(dat_tree, treatment == "clipped")$pine) - mean(subset(dat_tree, treatment == "control")$pine)

#Question 08: Simpson’s diversity resampling loop
#Complete the code for a loop to resample the slope parameter of a simple linear regression of the Simpson’s diversity indices for vegetation and birds.

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2] 
}

#Question 09: Simpson’s diversity null distribution
#Use your loop to create a MC resampled vector of 10000 model slope parameters.

#Plot a histogram of the MC simulated slope parameters.

#Use quantile to find the 5% quantile of slopes in the null distribution. This is the critical value.

#Use vline() to add a vertical line showing where the observed slope occurred.

#This line should be be blue, solid, and have a width of 2.0.
#Use vline() to add a vertical line showing the critical value.

#This line should be be red, dotted, and have a width of 2.0.

m = 10000 
result = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2] 
} 

crit_value = quantile(result, c(.05))

hist(result, 
     main = "Null Distribution of Regression Slope", 
     xlab = "Slope Parameter")
abline(v = slope_observed, col = 4 , lwd = 2.0, lty = 1)
abline(v = crit_value, col = 2 , lwd = 2.0, lty = 2)
?abline

slope_observed

??vline
#Question 10: Simpson’s diversity critical value
#Use quantile to find the 5% quantile of slopes in the null distribution. This is the critical value.

crit_value = quantile(result, c(.05))
crit_value

