# The General Social Survey is a high-quality survey which gathers data on American society and opinions, conducted since 1972. This data set is a sample of 500 entries from the GSS, spanning years 1973-2018, including demographic markers and some economic variables. Note that this data is included for demonstration only, and should not be assumed to provide accurate estimates relating to the GSS.
# Preliminary
# Read the GSSsubset into R
# To avoid “figure margins too large error” first change graphical parameters by running the following code par(mar=c(1,1,1,1))

# Exercise 1
setwd("C:/Users/user/Desktop/R-Class/Beginner Leve/session2.1/Lecture_one/EXERCISE_S2/EXERCISES")


Data_set <- read.csv("GSSsubset.csv")
head(Data_set)


# a) Create a histogram of income changing the breakpoints between histogram bars to 25.

x <- Data_set$income

summary(x)

hist(x,
     col = "steelblue",
     frame= FALSE,
     breaks = 25,
     main = "Income")


# b) Create a density plot of income and fill the density plot with a colour of choice.

Plot1 <- density(Data_set$income)

# plot density
plot(Plot1,
     col ="red",
     frame = FALSE,
     main = "Density plot of income")

polygon(Plot1, col= "green")


# c) Create a barplot of the degrees variable giving each bar a different colour. Hint: first tabulate the number of participants for each degree level using table(GSSsubset$degree) and allocate this an object name to be used for plotting.

Data_set

Degree <- Data_set $ degree

table(Degree)


Degree <-  table(Degree)

barplot(Degree,
        col = c("red", "blue", "green", "purple","black"),
        main="DEGREES",
        xlab="SCHOOL DISTRIBUTION")


# d) Change the barplot in d) above into a horizontal bar plot (adding the argument horiz = TRUE).

barplot(Degree,
        col = c("red", "blue", "green", "purple","black"),
        horiz = TRUE,
        main="DEGREES",
        xlab="SCHOOL DISTRIBUTION")


# e) Save the barplot using:
# • RStudio Plots Panel, and directly as a pdf to your working directory.




# Exercise 2
# a) Plot a pie chart of the degree variable. Hint: use table function to create table as done in Exercise 1 above about change this table into a dataframe using .data.frame(table) then asign a name to this dataframe.


pie(Degree,
    main = "Distribution of Degree",
    col = c("red", "blue", "green", "purple","black"),
    radius = 1)



# b) CDegreeb) Convert this pie chart into a 3D pie chart.

library(plotrix)

pie3D(Degree,
      main = "Distribution of Degree",
      col = c("red", "blue", "green", "purple","black"),
      radius = 1)



# Exercise 3
# a) Make a multiple groups boxplot of income classified into sex and degree giving different colors for each group.
attach(Data_set)

boxplot(income)

boxplot(income ~ sex*degree,
        data = Data_set,
        col = c("blue","green", "red"),
        frame = FALSE)



# b) Provide a relevant title and axis labels and remove frame.

boxplot(income ~ sex*degree,
        data = Data_set,
        col = c("blue","green", "red"),
        xlab = "Sex in Relation to Degree",
        ylab= "Income",
        frame = FALSE)



# c) Save this a pdf of a suitable page size in your working directory. Hint: PDF’s are 7x7 inches by default, and each new plot is on a new page. The page size can be changed to for instance 20x15 inches by adding the arguments pdf(“plots.pdf”, width=20, height=15). Change the page size to save into a suitably presentable plot.
pdf("plots.pdf",
    width=20,
    height=15,
)


# Exercise 4
# a) Plot the bivariate relationship relationships between hours of work (hrswrk) and income: provide title and axis labels, and Add a regression and a lowess line.
# dev.off()

plot(hrswrk,
     income,
     main = "Relationship between Hours of work and Income",  
     xlab = "Hours of Work ",
     ylab = "Income",
     pch = 19,
     frame=FALSE)

abline(lm(hrswrk ~ income), col="red")
lines(lowess(hrswrk,income), col="blue")


# b) Use function scatterplot3D to make a 3D plot similar to a) above.
# • Change color of the boxes grouping by degree.
# • Add grids and remove the box around the plot.
# • Change axis label names.

install.packages("scatterplot3d")
library("car")
library(scatterplot3d)

scatterplot(hrswrk ~ income,
            data = Data_set)

scatterplot3d(hrswrk,income,
              
              pch = 16,
              grid = TRUE,
              color = c("red"),
              box = FALSE,
              xlab ="Working Hours",
              ylab = "Income")



# Exercise 5: Graphical Parameters
# a) Randomly select 200 rows from the dataset GSSsubset to create GSSsubset2.
dim(Data_set)
Data_set

Data_set2 <- Data_set [sample(1:200, replace=FALSE),]



# b) Make a plot of hours of work (hrswrk) (y-axis) versus income (x-axis) using GSSsubset2.

Data_set2
attach(Data_set2)
plot(income,
     hrswrk,
     Xlabel = "Income",
     ylabel = "Hours of Work")



# c) Add a main title and x and y axis labels using a separate title() code line.

plot(income,
     hrswrk)

title("Relationship between income and Hours of Work", xlab="Income", ylab="Hours of Work")

dev.off()

# d) Assuming some cut-off levels of interest for income and hours of work are known from literature. Add reference lines to the graph using the abline( ) function:
# • solid horizontal lines at y= 20 and 55,
# • dashed blue verical lines at x = 10000,50000 and 100000.

abline(h=c(20,55))  # solid horizontal lines at y= 20 and 55,


abline(v=c(10000,50000,100000),
       lty=10,
       col="blue")
help("abline")



# Exercise 6
# Multiple plots can be combined into one overall graph par( ) function.

par(mfrow=c(2,2))


#   a) Create the following four figures from the GSSsubset2 arranged in 2 rows:
#   • scatter plot of plot income versus hours of work (insert title),

scatterplot(income,hrswrk,main="Income vs Hours of work")

# • scatterplot plot age verus hours of work (insert title),

scatterplot(age,hrswrk,main="Age vs Hours of work")

# • Make a histogram of height (insert title),

hist(height,main = "Height")


# • Make a boxplot of income by degree, add title, y and x axis labels, and different colour for each degree.

boxplot(income ~ degree,main="Relationship between income and degree", xlab = "Income", ylab= "Degree")



# b) Save the graph from a) above directly to your working directory in a suitable page size.

