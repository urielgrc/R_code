#########################################################################
## PB HLTH 145: Statistical analysis of continuous outcome data        ##
## Lecture 1 (Aug 27) Course overview, examining data                  ##
## Teacher: John Marshall (john.marshall@berkeley.edu)                 ##
## GSI: Lina Montoya (lmontoya@berkeley.edu)                           ##
######################################################################### 

#########################################################################
## 0. Introduction to R:                                               ##
#########################################################################

## Comments: 
## Anything on a line that follows a "#" in R is interpreted as a
## comment. E.g.

3 + 1 # This is a comment

## R version:
## It'll be helpful if we're all running the same version of R in class,
## so if you could make sure you're running the latest version (3.2.2),
## that'd be great.
## To check your version, you can type:

R.Version()$version.string

## If your version is smaller than 3.2.2, you can update by following the
## instructions here:
## http://cran.rstudio.com/
## If you need to update, note that newer versions of R are installed in
## separate folders (so you can keep old versions if you want) and you 
## need to copy previously-installed packages to the new folder. To do
## so, follow these steps:
## 1. Uninstall the current version of R (the "library" folder will 
##    still be there)
## 2. Download and install the new version of R
## 3. Copy previously-installed packages from the "library" folder of the
##    old version into the "library" folder of the new version
## 4. Delete anything that's left in the old R folder
## 4. Run the following code in the new version:

update.packages(checkBuilt=TRUE, ask=FALSE)

## Here are a few commands to give you an idea of how R can be used as a
## calculator:

2 + 2 * 3
(2 + 2) * 3
2 / 3 # division
2 ^ 3 # exponentiation

## Several functions are available that take variables as input and produce
## useful output such as the logarithm function:

log(100) # natural logarithm (base e)
log10(100) # logarithm (base 10)
log(100, base=10) # logarithm (base 10)

## You can obtain information about a function using the "help" function or 
## the "?" operator:

help(log)
?log

## You can also assign values to variables in R. E.g.

x <- 3 
x 
y <- 5 - 2 * x
y

## You can view the variables stored in R's memory at any time by typing:

ls()

## If you'd like to remove a specific variable, e.g. "y", you can type:

rm(y)

## Often it's helpful to start a new session by removing all stored
## variables by typing:

rm(list = ls())

## Most of the examples we'll be using from the Fox (2016) text book, 
## "Applied Regression Analysis & Generalized Linear Models" will require
## the "car" package, which can be installed and loaded by writing:

install.packages("car")
library(car)

## We will also use the following packages in this session:

install.packages("rgl")
library(rgl)
install.packages("plotrix")
library(plotrix)

#########################################################################
## 1. Vectors:                                                         ##
#########################################################################

## Vectors are ordered collections of simple elements such as numbers or
## strings. They can be created with the "c()" command. E.g.

a <- c(1, 3, 6, 1)
a

## The element at position i if a vector can be accessed with "[i]". E.g.

a[2]

## Several functions exist to quickly create simple vectors. E.g.

rep(3, times = 10)              # repeats 3 10 times
3:11                            # from 3 to 11 by 1
seq(1, 4)				  # from 1 to 4 by 1
seq(from = 3, to = 11, by = 2)  # from 3 to 11 by 2
seq_len(5)                      # from 1 to 5 by 1

## We can also redefine a variable any time, and we don't need to tell the
## interpreter how many values to hold.
## Here, we redefine "a" as 100 standard normal random numbers (mean of 0, 
## standard deviation of 1):

a <- rnorm(100)

## Several functions are available that take vectors as input and produce
## useful output such as:

a
mean(a)
median(a)
var(a)
length(a)

## Vectors can also take on non-numeric values, such as characters. E.g.

words <- c("to", "be", "or", "not", "to", "be")
words
words[2] # second element of words vector

## Vectors can also take on logical data. E.g.

vals <- c(TRUE, TRUE, FALSE, TRUE)
vals
vals[3] # third element of vals vector
vals[2:4] # elements 2-4 of vals vector

#########################################################################
## 2. Anscombe's quartet:                                              ##
#########################################################################

require(stats)
require(graphics)

## Data sets for Anscombe's quartet:

anscombe

## This is some tricky code I stole from this website to calculate the
## four regressions in a loop:
## https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/anscombe.html

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
	ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
	mods[[i]] <- lmi <- lm(ff, data = anscombe)
	print(anova(lmi))
}

## Now, let's see what the regression lines look like:
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
	ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
	plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
		xlim = c(3, 19), ylim = c(3, 13))
	abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

## Now let's see how the statistical parameters differ between the data
## sets / regression model fits:

sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## The implication here is that these four x-y datasets have the same
## (or almost the same) traditional statistical properties (mean,
## variance, correlation, regression line, etc.); but are very different
## in reality. This highlights the need to always plot the data!

#########################################################################
## 3. Examining distributions using histograms:                        ##
#########################################################################

## First, let's have a look at the education/income/prestige data set
## that's widely analyzed in the Fox (2016) book:

head(Prestige)

## We want to look specifically at the income data. We can extract this and
## assign it its own vector with the following code:

income <- Prestige$income

## The following code creates a histogram of income from this data

hist(income)

## The default number of bins here is quite small (i.e. each bin covers
## quite a large range of income values and obscures the pattern of the
## data).
## To use the Freedman & Diaconis (1981) recommendation on the number of 
## bins, we type:

hist(income, breaks="FD")

## And to set the color of the bars to gray, we have:

hist(income, breaks="FD", col="gray")

## There are lots of arguments you can specify for a histogram:

args(hist.default)
?hist

#########################################################################
## 4. Density estimation:                                              ##
#########################################################################

## Nonparametric density estimation often produces a more satisfactory
## representation of a distribution by smoothing the histogram. A kernel
## function is used to estimate at the value x of a variable X. The
## kernel function is generally a symmetric, single-peaked density
## function, like the normal distribution. A quantity, h (called the
## bandwidth), controls the degree of smoothness of the density estimate.
## The default density function in R uses a normal kernel and a
## reasonable method for selecting h.
## Applying this to the education/income/prestige data set, we have:

plot(density(income), lwd=2, main="Density plot of income")

## Here, "lwd=2" denotes a double thick line.

## To produce a rougher density estimate with a bandwidth of half the 
## default value, we use:

plot(density(income, adjust=0.5), lwd=1, main="Density plot of income")
rug(income)

## Here, "lwd=1" denotes a single thick line, and "rug(income)" draws a
## 1D scatterplot (or rug-plot) at the bottom of the graph.

## Putting this all together with the histogram plotted earlier, we have:

hist(income, breaks="FD", freq=FALSE, ylab="Density")
lines(density(income), lwd=2)
lines(density(income, adjust=0.5), lwd=1)
rug(income)
box()

#########################################################################
## 5. References:                                                      ##
#########################################################################

## 1. R Manual
## http://cran.r-project.org/doc/manuals/R-intro.html

## 2. R Programming Wikibook: Data types
## http://en.wikibooks.org/wiki/R_Programming/Data_types

## 3. R Language Specification: Indexing
## http://cran.r-project.org/doc/manuals/R-lang.html#Indexing
