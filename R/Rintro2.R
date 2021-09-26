##############################################################
##############  R intro part 2          ######################
##############################################################


#############################################
#############  Internal functions
#############################################

### create a dataframe 
#df <- data.frame(model=as.character(),price=as.numeric(),mpg=as.integer())

df <- data.frame(model=c("2018 Toyota Camry Hybrid","2018 Ford Fusion Hybrid","2018 Toyota Prius",
                         "2018 Hyundai Ioniq","2018 Kia Optima Hybrid","2018 Ford C-Max Hybrid"))
df$price <- c(32400, 37370, 30565, 28300, 35210, 27275)
df['mpg'] <- c(52, 42, 52, 58, 43, 40)

df
summary(df)

#####################
###  Aggregation
#####################
min(df$price)
max(df$price)
mean(df$price)
sd(df$price)
median(df$price)
quantile(df$price,c(0.01,0.1,0.25,0.75,0.9,0.99))
sum(df$price)

########################
### Concatenation
########################

a <- c(10, 25, 8, 33, 12, 4, 28, 9, 11, 22)
b <- c(20, 30, 33, 28, 34, 21, 26, 29, 20, 25)

### bind rows
c <- rbind(a,b)
c
class(c)
str(c)
dim(c)
rownames(c)

d <- cbind(a,b)

### bind columns
d <- c("Yes","No","No","Yes","Yes","Yes","No","No","Yes","No")

e <- cbind(a,b,d)
e <- cbind(e,a)
e
class(e)
str(e)
dim(e)
colnames(e)

#### add a new column
df
df2 <- cbind(df, available=c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE))
df2
class(df2)
str(df2)
summary(df2)
names(df2)

## alternative way
df2$available <- c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE)

### concatenate strings
s1 <- paste("I","love","data","science")
s1
class(s1)
str(s1)
summary(s1)

s2 <- paste("I","love","data","science",sep="_")
s2
class(s2)
str(s2)
summary(s2)


s3 <- c("I","love","data","science")

s4 <- paste(s1,collapse=" ")
s4
class(s4)
str(s4)
summary(s4)

s5 <- paste(s1,collapse="+")
s5
class(s5)
str(s5)

s6 <- paste0("I","love","data","science")
s6
class(s6)
str(s6)

###################################
### Tranformation
###################################

### functions to transform existing variables
### In data science we use very widely the following transformations:

d1 <- c(1,2,3,4,5,6)
d2 <- d1^2 ## quadratic
d2
d1^3  ## cubic
sqrt(d2)  ## square root

d3 <- log(d1) ## natural (neperian) logarithm
d3
exp(d3)
log10(d1) ## decimal logarithm

d4 <- c(-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5)
sign(d4)
abs(d4)

### geometric transformations
sin(c(10,30,90,180))
cos(c(10,30,90,180))
tan(c(10,30,90,180))

### other
pi <- 3.1415927
round(pi)
round(pi,1)
round(pi,2)
round(pi,4)

ceiling(pi)
floor(pi)

##############################################################
###   SYSTEM FUNCTIONS
##############################################################

ls()  ## show all the available objects existing in the environment
rm("c")  ## remove the C object

getwd()  ## get the working directory
setwd("C:/Users/Thomas/Documents/Python/")  ## set the working directory to the specified path

dt <- Sys.time()  ## get system date-time

dt
format(dt,format="%d-%m-%Y")  ## date format: '22-10-2014'
format(dt,format="%m/%d/%Y")  ## date format: '10/22/2014'
format(dt,format="%H:%M")     ## time format: '17:05'
format(dt,format="%I:%M %p")  ## time format: '05:05 PM'

timestamp()
d2 <- date()
class(d2)


format(as.Date('2020-01-03'),format="%m-%d-%Y") 

d3 <- as.Date('2020-01-03')
class(d3)

d3 + 30

### Sequences
1:10
seq(1,10,2)
rep(0.5, 10)
length(c(1,2,3,4,5,6,7,8,9,10))

### randomization
sample(c(1,2,3,4,5,6,7,8,9,10),3)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

set.seed(123)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

set.seed(124)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

runif(n=10, min=0, max=1)  ## 10 random numbers between 0 and 1
runif(n=5, min=-3, max=3)  ## 3 random numbers between -3 and 3

## normal distribution
rnorm(n=10, mean=5, sd=2)

## binomial distribution
rbinom(n=10, size=1, prob=0.4)

################################################################
######  Conditional expressions - return a boolean response
################################################################

a <- TRUE
b <- TRUE
c <- FALSE
d <- FALSE

a & b  # and
a & c
c & d

a | b  # or
a | c
c | d

a + b
a + c
c + d

a * b
a * c
c * d

a == b
a == c
c == d

cities = c('Jerusalem', 'Tel Aviv', 'Haifa')

'Jerusalem' %in% cities
'Holon' %in% cities

!('Jerusalem' %in% cities)  ## not in
!('Holon' %in% cities)      ## not in

('Jerusalem' %in% cities) | ('Holon' %in% cities)     


############################################
### Apply and friends
############################################

### apply: apply a function recursively on a matrix or array
### MARGIN: how to apply the functions. 1=by rows, 2=by columns
### FUN: the function to apply. 
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=T)
m1
apply(X = m1, MARGIN = 1, FUN = sum)   # like rowSums(m1)
apply(X = m1, MARGIN = 2, FUN = sum)   # like colSums(m1)


############
#### lapply: apply a function recursively on a list.. the result will be a list
############
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=T)
m2 <- matrix(c(2,1,2,2,5,3,5,4,5,5,5,1),ncol=3,byrow=T)
m3 <- matrix(c(8,8,8,8,6,6,6,6,4,4,4,4),ncol=4)
l1 <- list(m1=m1,m2=m2,m3=m3)
l1


lapply(l1, sum)
### We can use the selection operator `[` for extracting values at the same position
### Get the element of the third column of each element of a list 
lapply(l1, "[",,1:2)  

### Get the element of the first row of each element of a list 
lapply(l1, "[",1,)  

###########
### sapply: like lapply, but tries to simplify the output
### 
###########
l1

### We can use the selection operator `[` for extracting values at the same position
### the following extract the first value of each element in the list 
sapply(l1, "[",1,,simplify = F)

### This will extract matrices containing the three first rows and columns of each element 
sapply(l1, "[",1:3,1:3,simplify = F)

### Simplify true will make the same, but joining the output into one unique matrix
sapply(l1, "[",1:3,1:3,simplify = T)

###########
### mapply: "multivariate" apply - apply a function to multiple objects at a time
###########
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=F)
m1

### We can generate the same matrix using mapply
m2 <- mapply(rep,1:5,5)
m2

m1 == m2

### generate a list of vectors containing letters repeated 4 times each 
mapply(rep, LETTERS[1:6], 4, SIMPLIFY = TRUE)

### what will this generate?
mapply(rep, LETTERS[1:6], 6:1, SIMPLIFY = FALSE)

#####################################################################
############  Programming with R
#####################################################################

##########################
### Conditional 
##########################

###  if (condition) { ... } 
x <- 0
y <- 0
if( x > 1 ) { y = 3 }
c(x,y)

###  if (condition) { ... } else { ... }
x <- 5
y <- 0
if( x > 5 ) { y = 10 } else { y = 5 }
c(x,y)

###  ifelse(condition, value1, value2) 
x <- 5
y <- ifelse(x > 5, 10, 5)
c(x,y)

###################################
### Recursive
###################################

### for(var in sequence) { ... }
y <- 1
for(x in 1:10) {
  y <- y + x
  print(c(x,y))
}
y

### exit prematurely from the for loop
for (x in 1:1000) {
  print(x)
  if(x==20) {
    break
  }
}

### while(condition) { ... }
y <- 10
x <- 1
while(x < 5) {
  y <- y * x
  x <- x + 1
  print(c(x,y))
}



### permanent recursion
x <- 0
while(TRUE) {
  x <- x + 1
  print(x)
}
## how can we prevent it from running infinitely? 

## repeat works like the last while example.
x <- 0
repeat {   
  x <- x + 1
  if (x == 1) {
    print("We are just beginning")
  } else if (x < 10) {
    print(paste("Counting...",x))
  } else {
    print("We are done !!!")
    break
  }
}

###################################
### Functional
###################################

# myfun <- function(arg1, a1g2) {
#    body
#}


xplusone <- function(x) {
  x + 1
}

xplusone(7)
xplusone(20)
b <- c(1,2,3,4,5)
xplusone(b)

### beware from reserved words !!!
c <- function(x) {
  x + 1
}

c(5)
b <- c(1,2,3,4,5)
c(b)

rm(c)

BMI <- function(weight, height) {
  weight/(height^2)
}

BMI(70,1.75)
BMI(85,1.65)

weights <- c(rnorm(10,mean=75,sd=8.5))
heights <- c(rnorm(10,mean=1.75,sd=0.2))

BMI(weights, heights)

BMI()
BMI(65)

BMI <- function(weight, height=1.75) {
  weight/(height^2)
}

BMI(65)
BMI(65,1.65)

BMI <- function(weight, height=NULL) {
  weight/(height^2)
}

BMI()
BMI(65)
BMI(65,1.65)

BMI <- function(weight=NULL, height=NULL) {
  try(weight/(height^2))
}

BMI()
BMI(65)
BMI(65,1.65)


BMI <- function(weight=NULL, height=NULL) {
  if(is.null(weight)) {
    stop("Please enter a value for weight")
  }
  if(is.null(height)) {
    stop("Please enter a value for height")
  }
  weight/(height^2)
}


BMI()
BMI(65)
BMI(65,"a")

BMI <- function(weight=NULL, height=NULL) {
  if(!is.numeric(weight)) {
    stop("Please enter a numeric value for weight")
  }
  if(!is.numeric(height)) {
    stop("Please enter a numeric value for height")
  }
  weight/(height^2)
}

BMI()
BMI(65)
BMI(65,"a")
BMI(65,0)

BMI <- function(weight, height) {
  res <- tryCatch(
    { 
      weight/(height^2)
    },
    error= function(cond) {
      message("Please enter a numeric value for weight or height")
      return(NA)
    },
    finally= {
      message("Your BMI is: ")
    }
  )
  return(res)
}

BMI()
BMI(65)
BMI(65,0)
BMI(65,1.5)

#######################################################################
###########  Regular Expressions
#######################################################################

#### grep
grep("a", c("abc", "def", "cba a", "aa"), value=FALSE)  ### return indices
grep("a", c("abc", "def", "cba a", "aa"), value=TRUE)   ### return values

head(colours())
grep("orange",colours())
grep("orange",colours(),value=T)

grep("dark",colours())
grep("dark",colours(),value=T)

### "red", "^red", "$red"
grep("red",colours(),value=T)  ## contains red
grep("^red",colours(),value=T) ## ^ -> begins with red
grep("red$",colours(),value=T) ## $ -> ends with red
grep("red.$",colours(),value=T) ## '.' -> match any single character
grep("^red",colours(),value=T) ## '^' -> begins with red
grep("^red[2:4]",colours(),value=T) ## [] -> begins with red and has a number of 2 or 4 
grep("^red[2|4]",colours(),value=T) ## [] -> begins with red and has a number of 3 or 4
grep("^red[2-4]",colours(),value=T) ## [] -> begins with red and has a number between 2 and 4

grep("^red[2:4]|^blue[2:4]",colours(),value=T)  

grep("2|4",colours()[100:150],value=T)  
grep("[24]",colours()[100:150],value=T)  

grep("^[dl].*2$",colours(),value=T)  ## all strings beginin with d or l and ending with 2

#### logical grep
grepl("a", c("abc", "def", "cba a", "aa"))  ##  value=TRUE is not supported
grepl("white",colours()[1:20])

#### Find and Replace once
sub("a", "A", c("abc", "def", "cba a", "aa"))  ##  replace the first occurrence of 'a' 
##   on each element with an 'A'
#### Find and Replace any occurence
gsub("a", "A", c("abc", "def", "cba a", "aa"))

#### Return the index of the first match of 'a' as a vector
regexpr("a", c("abc", "def", "cba a", "aa"))

#### Return the indices of all the matches of 'a' as a list
gregexpr("a", c("abc", "def", "cba a", "aa"))

#### Return the index of the first match of 'a' as a list
regexec("a", c("abc", "def", "cba a", "aa"))

#### Split an object when a character or string appears.
strsplit(c("abc", "def", "cba a", "aa"),split = "b")

### separating by a comma
strsplit(c("The boy is playing with his car, his father is talking on the phone"),split=',')

### separating by a point
strsplit(c("The boy is playing with his car. His father is talking on the phone"),split='\\.')




 
