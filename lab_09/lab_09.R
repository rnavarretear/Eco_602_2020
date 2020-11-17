rm(list = ls())

require(here)

read.csv(here("data", "rope.csv"))
rope <- read.csv(here("data", "rope.csv"))
  
?factor
rope$rope.type = factor(rope$rope.type)
rope

levels(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

grand_mean = mean(rope$p.cut)
grand_mean

resids_rope = rope$p.cut - grand_mean
resids_rope

ss_tot = sum(resids_rope^2)
ss_tot

boxplot(rope$p.cut)
boxplot(p.cut ~ rope.type, data = rope)

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

#does the same:

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

#not means but residuals:

agg_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) c(x - mean(x))
)
agg_resids

str(agg_resids)

agg_sq_resids = aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum(c(x - mean(x))^2)
)
agg_sq_resids

?str
str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
ss_within

ss_among = ss_tot - ss_within

df_tot = n_obs - 1
df_tot

df_within = n_obs - n_groups
df_within

df_among = n_groups - 1

ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

ms_among
ms_within

f_ratio = ms_among/ms_within
f_ratio

?pf

pf(q = f_ratio, df1 = 5, df2 = 115 )
f_pval = 1 - pf(q = f_ratio, df1 = 5, df2 = 115 )

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"

# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)
