# Chapter 2 R Fundamentals


# ====================
# Exercise 2.1
# ====================
scores <- rnorm(1000, mean = 75, sd = 5.8)
plot(density(scores))
hist(scores)
boxplot(scores)
# ====================
# END
# ====================



# ====================
# Exercise 2.2
# ====================
2^(2+1) - 4 + 64^((-2)^(2.25-1/4))
# ====================
# END
# ====================



# ====================
# Exercise 2.3
# ====================
make_students_happy <- function(old_scores = ""){
  print(sqrt(old_scores) * 10)
}
student_current_scores <- c(20, 34, 60, 87, 100)
make_students_happy(old_scores = student_current_scores)
# ====================
# END
# ====================



# ====================
# Exercise 2.4
# ====================
data(iris)
summary(iris)

# The Iris Dataset has:
# 3 species of Iris: Iris setosa, Iris virginica and Iris versicolor
# 4 features: length, width of sepals and petals
# 50 samples of each species
# Total of 150 objects(samples)

# ====================
# END
# ====================



# ====================
# Exercise 2.5
# ====================

?str     # display the structure of an R object (alternative to summary)

?summary # a function to produce summaries of various model results

?dim     # retrieve or set dimension of an object

?colnames  # retrieve or set row/ column names of matrix-like object

?names   # set or get the names of an object

?nrow    # the number of rows

?ncol    # the number of columns

?head    # return the first parts of vectors, matrices, tables, df, funcs

?tail    # return the last parts of vectors, matrices, tables, df, funcs

#We can get information of "Description", "Usage", "Arguments", "Details", and "Value"


# ====================
# END
# ====================



# ====================
# Exercise 2.6
# ====================

myfac <- function(x){
  
  if (x == 0){
    r <- 1
  } else {
    
    r <- x
    while(x > 1){
      r <- (x - 1) * r
      x <- x - 1
    }
  }
  return(r)
}

#(i)
myfac(5)

#(ii)
myfac(12)

#(iii)
myfac(0)
# ====================
# END
# ====================
