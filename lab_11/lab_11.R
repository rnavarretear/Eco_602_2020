require(here)

bird <- read.csv(here("data", "bird.sub.csv"))
hab <- read.csv(here("data", "hab.sub.csv"))

bird

birdhab <- merge(bird, hab, by = c("basin", "sub"))
birdhab
?merge

dim(birdhab)

fit_1 = lm(BRCR ~ ls, data = birdhab)

plot(BRCR ~ ls, data = birdhab)
abline(lm(BRCR ~ ls, data = birdhab))

summary(lm(BRCR ~ ls, data = birdhab))

linear = function(x, y_int, slope) 
{return(y_int + x * slope)}

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)


linear_simulator = function(x, y_int, slope, st_dev)
{
  return((y_int + slope*x) + rnorm(length(x), mean = 0, sd= st_dev))
}


n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, 
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}


fit_1_coefs = coefficients(fit_1)
fit_1_coefs
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
fit_1_summary$sigma

int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2] 
sd_obs = fit_1_summary$sigma

dev.off()

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

summary(lm(y_sim ~ ls, data = birdhab))

n_sims = 1000
p_vals = numeric(n_sims)
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < 0.05) / n_sims
p_vals

linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  return(lm(y_sim ~ x))
}

alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    power       = effect_size_powers,
    effect_size = effect_sizes_1)

sim_effect_size
p_vals

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')





alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
sample_size_powers = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_sample_size = 
  data.frame(
    power       = sample_size_powers,
    sample_size = sample_sizes)

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)
sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )


image(sim_n_effect_size$power)

contour(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power)

persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages("rgl")

require(rgl)

persp3d(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')


rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot.html"),
  width = 1200, height = 1200
)

save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

load(file = here::here("data", "lab_11_n_effect_sizes.Rdata")))




#Lab Assignment

#First question

alpha = 0.05
n_sims = 500
p_vals = numeric(n_sims)

n_sds = 500
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}



sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_power
)


plot(
  power ~ sd, data = sim_output_dispersion,
  type = 'l', xlab = 'Standard Deviation', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')


#Second and third question

alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

n_sds = 100
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

sample_sizes_1 = seq(5, 100)


sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes_1))


for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes_1))
  {
    x_vals_1 = seq(0, 100, length.out = sample_sizes_1[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals_1,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k," of ", length(pop_sds)))
}

image(sim_output_3)
image(sim_3_dat$power)

sim_output_3

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)



contour(x = sim_3_dat$pop_sd,
        y = sim_3_dat$sample_size,
        z = sim_3_dat$power)

persp3d(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "sd", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75, alpha = 0.5,
  ticktype = 'detailed')



rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_sds_power_sim_plot.html"),
  width = 1200, height = 1200
)

save(
  sim_3_dat,
  file = here::here("data", "lab_11_n_sds.Rdata"))

load(file = here::here("data", "lab_11_n_sds.Rdata"))

#sim_n_effect_size = 


  
  
image(sim_output_3$power)

contour(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power)






persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages("rgl")

require(rgl)




rgl::writeWebGL(
  dir = here::here("docs", "webGL"), 
  filename = here::here(
    "docs", "webGL",
    "n_effect_size_power_sim_plot.html"),
  width = 1200, height = 1200
)

save(
  sim_n_effect_size,
  file = here::here("data", "lab_11_n_effect_sizes.Rdata"))

load(file = here::here("data", "lab_11_n_effect_sizes.Rdata")))


# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(...)

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = ...)