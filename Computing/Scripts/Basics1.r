#setting your working directory
#===================================================

?setwd()

setwd("C:/allan/STATS/Honours/Honours StatisticalComputing/R2014/R/Scripts")

#Some basic Computations
#===================================================

2 + 3 * 5      # Note the order of operations.
log (10)       # Natural logarithm with base e=2.718282
4^2            # 4 raised to the second power
15/4            # Division
sqrt (16)      # Square root
abs (3-7)      # Absolute value of 3-7
pi             # The mysterious number
exp(2)         # exponential function
15 %/% 4       # This is the integer divide operation
?"%/%"         # Help on the function
> # This is a comment line 
  

#Creating objects
#===================================================  
x<- log(2.843432)*pi
x

sqrt(x)

floor(x)        # largest integer less than or equal to x (Gauss number)

ceiling(x)      # smallest integer greater than or equal to x


#Vectors
#===================================================

x<-c(1,3,2,10,5)    #create a vector x with 5 components
x

y<-1:5              #create a vector of consecutive integers
y

y+2                 #scalar addition

2*y                 #scalar multiplication

y^2                 #raise each component to the second power

2^y                 #raise 2 to the first through fifth power

y                   #y itself has not been changed

y<-y*2
y                   #it is now changed

length(y)

sum(y)

max(y)

min(y)

prod(y)

x[2]     #the second element in x
x[6]     #NA since there are only 5 elements in the object 'x'

x[1]+x[5] #referencing elements of a vector


#Some more Vectors
#===================================================

x<-c(1,3,2,10,5); y<-1:5 #two or more statements are separated by semicolons

x+y

x*y

x/y   #what is being done here?

x^y   #what is being done here?


#Sorting can be done using sort() 
#===================================================

x

sort(x)                # increasing order

sort(x, decreasing=T)  # decreasing order

#Logical vector can be handy:
#===================================================

x>3      #The answer is: [1] FALSE FALSE FALSE  TRUE  TRUE

as.numeric(x>3)     # as.numeric() function coerces logical components to numeric
#[1] 0 0 0 1 1

sum(x>3)            # number of elements in x greater than 3

#(1:length(x))[x<=2] # indices of x whose components are less than or equal to 2
#[1] 1 3

z<-as.logical(c(1,0,0,1)) # numeric to logical vector conversion
z


#Component extraction is a very important part of 
#vector calculation.
#===================================================

x

length(x)           # number of elements in x

x[3]                # the third element of x

x[3:5]              # the third to fifth element of x, inclusive

x[-2]               # all except the second element

x[x>3]              # list of elements in x greater than 3


#Character vector:
#===================================================

colours<-c("green", "blue", "orange", "yellow", "red")
colours

colours[2]
colours[5]

colours[1] + colours[2] #we cannot perform numeric operations here!


#Individual components can be named and referenced by 
#their names.
#===================================================

names(x)            # check if any names are attached to x

names(x)<-colours    # assign the names using the character vector colors

names(x)

x
x["green"]          # component reference by its name

names(x)<-NULL      # names can be removed by assigning NULL
x

#seq() and rep() provide convenient ways to a 
#construct vectors with a certain pattern.
#===================================================

seq(10)

seq(0,1,length=10)

seq(0,1,by=0.1)

rep(1,3)

c(rep(1,3),rep(2,2),rep(-1,4))

rep("Small",3)

c(rep("Small",3),rep("Medium",4))

rep(c("Low","High"),3)


#Some Matrices
#===================================================

x<-c(1,3,2,10,5); y<-1:5 #two or more statements are separated by semicolons

x
y

m1<-cbind(x,y)     #column bind!
m1

t(m1)                # transpose of m1

m1<-t(cbind(x,y))    # Or you can combine them and assign in one step

dim(m1)              # 2 by 5 matrix

m1<-rbind(x,y)       # rbind() is for row bind and equivalent to t(cbind()).

#Of course you can directly list the elements and specify the matrix:
m2<-matrix(c(1,3,2,5,-1,2,2,3,9),nrow=3) 
m2


#Note that the elements are used to fill the first column, then the second column and so on. 
#To fill row-wise, we specify byrow=T option:

m2<-matrix(c(1,3,2,5,-1,2,2,3,9),ncol=3,byrow=T);m2

#Extracting the component of a matrix involves one 
#or two indices.
#===================================================

m2

m2[2,3]            #element of m2 at the second row, third column

m2[2,]             #second row

m2[,3]             #third column

m2[-1,]            #submatrix of m2 without the first row

m2[,-1]            #ditto, sans the first column

m2[-1,-1]          #submatrix of m2 with the first row and column removed


#Matrix computation is usually done component-wise.
#===================================================

m1<-matrix(1:4, ncol=2); 
m2<-matrix(c(10,20,30,40),ncol=2)

2*m1                # scalar multiplication

m1+m2               # matrix addition

m1*m2               # component-wise multiplication

#Note that m1*m2 is NOT the usual matrix multiplication. 
#To do the matrix multiplication, you should use %*% operator instead.

m1 %*% m2

solve(m1)            #inverse matrix of m1

solve(m1)%*%m1       #check if it is so

diag(3)              #diag() is used to construct a k by k identity matrix

diag(c(2,3,3))       #as well as other diagonal matrices


#Some very simple Graphics
#===================================================

#We will use a data set fuel.frame which is based on makes 
#of cars taken from the April 1990 issue of Consumer Reports.

#open 'fuel-frame.txt' using note pad (not in R)
#note what the data looks like
#also have a look at the *xlsx file
#R could be used to open both of these files
#common ways of reading txt or csv files are with 'read.table'
#read.csv could also be used

fuel.frame<-read.table("fuel-frame.txt", header=T, sep=",")
read.table("fuel-frame.txt", header=T, sep=",")
fuel.frame
#read.table("the path name", header=T, sep=",") 
#you created an 'object' with the name 'fuel.frame'
#technically this is a 'dataframe'

#have a look at the different types of variables
#'Weight' is continuous while 'type' is categorical

names(fuel.frame) #gives the names of the variables used

#Type the following
Disp.

#You should get an error. Why?

attach(fuel.frame) #adds the variable names to the search path 
#now you can use the variable names
#attach() allows to reference variables in fuel.frame 

Disp.

#variables could also be referenced as 
fuel.frame$Disp.

#hist() produces a histogram, boxplot() does a boxplot, etc.
hist(Mileage)
hist(Mileage, freq=F)    # if probability instead of frequency is desired 


#Built in data sets
#===================================================

data(faithful)   #they are accessed sing the 'data' function

faithful

#the dataframe contains two variables

attach(faithful)
names(faithful)

hist(eruptions, seq(1.6, 5.2, 0.2), prob=T)
lines(density(eruptions, bw=0.1), col="red")   #adds a density plot to the histogram. 'col' changes the colour
rug(eruptions, col="red") #adds a rug to the fig


#Boxplots
#===================================================

boxplot(Weight)                # usual vertical boxplot
rug(Weight, side=2, col="red") 
#read up on the function. ?rug

boxplot(Weight, horizontal=T)  # horizontal boxplot

boxplot(Weight, horizontal=T, main="Boxplot of Weight")  # 'main' allows one to add a main caption

#If you want to get the statistics involved in the boxplots, the following commands show them. 
#In this example, a$stats gives the value of the lower end of the whisker, the first quartile (25th percentile), second quartile (median=50th percentile), third quartile (75th percentile), and the upper end of the whisker.

a<-boxplot(Weight, plot=F)
a$stats

a    #gives additional information

fivenum(Weight)    #directly obtain the five number summary


#Boxplot is more useful when comparing grouped data. 
#For example, side-by-side boxplots of weights grouped by vehicle types are shown below:

boxplot(Weight ~Type)
title("Weight by Vehicle Types")


#Plot
#===================================================

#plot() is a general graphic command with numerous options.
plot(Weight)

#The following command produce a scatterplot with Weight 
#on the x-axis and Mileage on the y-axis.
plot(Weight, Mileage, main="Weight vs. Mileage")

#A fitted straight line is shown in the plot by executing 
#two more commands.
fit<-lm(Mileage~Weight)
abline(fit, col="blue") 


#Creating a dataframe
#===================================================

y60<-c(316.27, 316.81, 317.42, 318.87, 319.87, 319.43, 318.01, 315.74, 314.00, 313.68, 314.84, 316.03)
y70<-c(324.89, 325.82, 326.77, 327.97, 327.91, 327.50, 326.18, 324.53, 322.93, 322.90, 323.85, 324.96)
y80<-c(337.84, 338.19, 339.91, 340.60, 341.29, 341.00, 339.39, 337.43, 335.72, 335.84, 336.93, 338.04)
y90<-c(353.50, 354.55, 355.23, 356.04, 357.00, 356.07, 354.67, 352.76, 350.82, 351.04, 352.69, 354.07)
y97<-c(363.23, 364.06, 364.61, 366.40, 366.84, 365.68, 364.52, 362.57, 360.24, 360.83, 362.49, 364.34)

CO2<-data.frame(y60, y70, y80, y90, y97)

#add row names to the dataframe
row.names(CO2)<-c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

CO2 

#matplot() is used to plot two or more vectors of equal length.
matplot(CO2) 

matplot(CO2,axes=F,frame=T,type='b',ylab="")
#axes=F: initially do not draw axis
#frame=T: box around the plot is drawn;
#type=b: both line and character represent a seris;
#ylab="": No label for y-axis is shown;
#ylim=c(310,400): Specify the y-axis range
axis(2) # put numerical annotations at the tickmarks in y-axis;

axis(1, 1:12, row.names(CO2))
# use the Monthly names for the tickmarks in x-axis; length is 12;
title(xlab="Month")    #label for x-axis;
title(ylab="CO2 (ppm)")#label for y-axis;
title("Monthly CO2 Concentration \n for 1960, 1970, 1980, 1990 and 1997")
# two-line title for the matplot
  

#Types of plots and the 'par' command
#===================================================

par(mfrow=c(2,2))
plot(Fuel, type="l"); title("lines")
plot(Fuel, type="b"); title("both")
plot(Fuel, type="o"); title("overstruck")
plot(Fuel, type="h"); title("high density") 



#Simple linear regression
#========================

#Lets generate some data
x<-rnorm(50)
y<-2 + 5*x + rnorm(50)

plot(y~x)

#fit a linear regression
fit<-lm(y~x)

summary(fit)
fit$coef

#Most of this code was taken from http://math.illinoisstate.edu/dhkim/Rstuff/Rtutor.html
