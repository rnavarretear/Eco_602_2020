#Question 1
#You've used logical subsetting to select elements of a matrix on a vector. With small data sets it's possible to look at all of the elements at once and visually detect the indices of the elements you want. This is not possible with larger data sets.

#Run the following code to create a large vector containing randomly generated integers between 1 and 12:

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

#Use a logical test operator to create a Boolean vector (called vec_2) whose entries are TRUE if the corresponding entry in vec_1 is 3 and FALSE otherwise.

#Self test: you can use vec_2 to retrieve all of the 3 elements of vec_1 using the following:

vec_2 = vec_1==3
vec_2
vec_1[vec_2]


n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
vec_2 <- c(vec_1 == 3)
vec_2

vec_1[vec_2]

#Question 2
#Run the following code to create a large vector containing randomly generated integers between 1 and 12:

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)

sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Question 3
#Recall the simple for loop example in the lab assignment background reading.

for (i in 1:10)
{
  print(i)
}

for (i in 1:10)
{
  print(paste0("This is loop iteration: ", i))
}

#Question 4
#Recall the simple for loop example in the lab assignment background reading.

n = 47
for (i in 1:n)
{
  print(i)
}


#Question 5
#Create an integer variable, n, that holds the value 17.
#Write code to create a vector called vec_1 of length n. vec_1 should contain [pseudo]randomly generated integers between 1 and 10.

n = as.integer(17) 

vec_1 = sample(10, n, replace = TRUE)
vec_1

for (i in 1:n)
{
  print(paste0("The element of vec_1 at index ", i, " is ", vec_1[i]))
}

#Question 6

create_and_print_vec = function(n,min=1,max=10)
{
  vec_0 <- sample(max,n,replace = TRUE)
  for (i in 1:n)
  {
    print(paste0("The element at index ", i, " is ", vec_0[i]))
  }
}
 
create_and_print_vec(10)
create_and_print_vec(10, min = 100, max = 2000)
