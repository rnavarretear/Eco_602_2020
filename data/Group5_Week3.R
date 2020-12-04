dat_birds <- data.frame(read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv"))
#We create the data frame dat_birds from a data set that is located on https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv
dat_birds
dat_habitat <- data.frame(read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv"))
#We create the data frame dat_habitat from a data set that is located on https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv
dat_habitat
#we create simple pair plots: subsetting by names, the column names were visualized by the function "head"
pairs(dat_habitat[,c("aspect","ba.snag","ba.ratio")])
#We create a pair plot among "aspect", "ba.snag" and "ba.ratio"
hist(dat_birds$WIWA,xlab = "Number of birds counted",breaks = 0:7 - 0.5)
#We create a histogram with WIWA. The argument breaks = 0:7 - 0.5 helps us to evaluate how the frecuency results for each integer from 0 to 7. Since there are only integers, the range from one halfpoint to another halfpoint will include all the values of the integer.