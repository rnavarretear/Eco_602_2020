a <- "Roberto"
b1 <- 45.6
b2 <- "45.6"
c <- c(0:3)
c1 <- c(0,1,2,3)

class(a)
class(b1)
class(b2)
class(c)
class(c1)

b1+b2
b1+c

v1 <- c(-2:2)
v1

v2 <- 3*v1
v2

sum(v2)

#Create a list, named my_list_1 with following three elements:
# 1.	first element is numeric: 5.2
#2.	second element is a string "five point two"
#3.	third element is a vector of all integers from 0 to 5. Do recall how to do this from the DataCamp course?
a <- 5.2
b <- "five point two"
c <- c(0:5)

my_list_1 <- list(a,b,c)

# Name the elements in my_list_1:
#1.	"two"
#2.	"one"
#3.	"three"

names(my_list_1) <- c("two","one","three")
my_list_1

my_list_1[[3]]
my_list_1$three

my_list_1[["one"]]
my_list_1$one

my_vec = rep(1:3, 5)
my_vec
##  [1] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3
my_bool_vec <- my_vec==3
my_bool_vec
data.frame(my_vec, my_bool_vec)
my_vec[my_bool_vec]
