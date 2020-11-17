dbinom(x = 4, size = 10, prob = 0.3)

pbinom(q = 4, size = 10, prob = 0.3)

qbinom(p = 0.9, size = 10, prob = 0.3)

dnorm(x = 8, mean = 10, sd = 2)

pnorm(q = 8, mean = 10, sd = 2)

qnorm(p = 0.9, mean = 10, sd = 2)


rm(list = ls())


pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
pol_predation_rate = pol_n_predation/pol_n_total
pol_predation_rate
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = 731
psd_predation_rate = psd_n_predation/psd_n_total
psd_predation_rate

print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3))) 

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))

ratio = pol_predation_rate/psd_predation_rate
ratio