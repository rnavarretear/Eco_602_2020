?pbinom
dbinom(x = 4, size = 10, prob = 0.3)
pbinom(q = 4, size = 10, prob = 0.3)
qbinom(p = 0.9, size = 10, prob = 0.3)

?dnorm
dnorm(x = 8, mean = 10, sd = 2)
pnorm(q = 8, mean = 10, sd = 2)
qnorm(p = 0.9, mean = 10, sd = 2)

#Q1: 2 pts
#What is the probability of observing a count of exactly 3 successes in a binomial distribution with parameters n = 4 and p = 0.75?

# 0.421875

#Paste the R code that you used to find your answer.

dbinom(x = 3, size = 4, prob = 0.75)

#Q1: 2 pts
#What is the probability of observing a count of 3 successes or fewer in a binomial distribution with parameters n = 4 and p = 0.75?

#0.6835937

#Paste the R code that you used to find your answer.

pbinom(q = 3, size = 4, prob = 0.75)

#Q1: 3 pts
#What is the probability of observing more than 3 successes in a binomial distribution with parameters n = 5 and p = 0.75?

#0.6328125

#Paste the R code that you used to find your answer.

1 - pbinom(q = 3, size = 5, prob = 0.75)

#Q1: 2 pts.
#What is the probability of observing a value of less than 1.2 from a normally-distributed population with mean = 2 and standard deviation = 2?

#0.3445783

#Paste the R code that you used to find your answer.

pnorm(q = 1.2, mean = 2, sd = 2)

#Q1: 3 pts.
#What is the probability of observing a value of greater than 1.2 from a normally-distributed population with mean = 2 and standard deviation = 2?

#0.6554217

#Paste the R code that you used to find your answer.

1-pnorm(q = 1.2, mean = 2, sd = 2)

#Q1: 4 pts.
#What is the probability of observing a value between 1.2 and 3.2 from a normally-distributed population with mean = 2 and standard deviation = 2?

#0.3811686

#Paste the R code that you used to find your answer.

pnorm(q = 3.2, mean = 2, sd = 2) - pnorm(q = 1.2, mean = 2, sd = 2)
