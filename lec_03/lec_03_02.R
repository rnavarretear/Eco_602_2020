install.packages("palmerpenguins")
install.packages("here")

require(palmerpenguins)
require(here)

library("palmerpenguins")

class("palmerpenguins")

penguins = data.frame(penguins)

penguins

mean(penguins$body_mass_g)
?mean
mean(penguins$body_mass_g,na.rm = TRUE)

summary(penguins)

boxplot(penguins$bill_depth_mm)

boxplot(bill_depth_mm ~ sex, data = penguins)
boxplot(bill_depth_mm ~ flipper_length_mm, data = penguins)
boxplot(bill_depth_mm ~ island, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(bill_depth_mm ~ island, data = penguins)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | flipper_length_mm, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | island, data = penguins)

require(here)
png(filename = here("basic_histogram.png"), width = 800, height = 600)
hist(penguins$body_mass_g)
dev.off()

require(here)
png(filename = here("boxplot_sex_island.png"), width = 800, height = 600)
par(mfrow = c(1, 2))
boxplot(bill_depth_mm ~ island, data = penguins)
boxplot(bill_depth_mm ~ sex, data = penguins)
dev.off()

require(here)
png(filename = here("coplot_flipperlength.png"), width = 800, height = 600)
par(mfrow = c(1, 2))
coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | flipper_length_mm, data = penguins)
dev.off()