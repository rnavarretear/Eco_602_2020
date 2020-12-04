install.packages("palmerpenguins")
require(palmerpenguins)


t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

dev.off()

boxplot(body_mass_g ~ species, data = penguins)

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

dat_adelie = subset(penguins, species == "Adelie")
mean(dat_adelie$body_mass_g, na.rm = TRUE)

dat_gentoo = subset(penguins, species == "Gentoo")
mean(dat_gentoo$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)
shapiro.test(dat_adelie$body_mass_g)
shapiro.test(dat_gentoo$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

anova(fit_species)

boxplot(body_mass_g ~ sex * species, data = penguins)

fit_both = lm(body_mass_g ~ sex * species, data = penguins)
fit_both

summary(fit_both)
