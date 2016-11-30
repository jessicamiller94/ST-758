

# To create a package in RStudio, simply choose
# file > New Project ...
# then select New Directory, R Package,
# then name it, select the directory, and
# click Create Project
# This will create a new directory with the
# minimum of files required to be a package
# It also creates a .Rproj file that contains
# some settings for the project

# I've already created one called 'tourney'
# go ahead and pull it from the github
# page and look at the contents


# add Imports line to DESCRIPTION and
# import matrixStats package
devtools::use_package("matrixStats")


# add documentation for the scoreBracket function
devtools::document()
?scoreBracket


# vignettes
browseVignettes("knitr")


# create our own vignette
devtools::use_vignette("my-vignette")
# open up bracketVignette.Rmd


# namespaces help ensure that the correct
# functions are called by your package's functions.
# this is important because you cannot predict
# every user's environment. The user may have
# done something silly, like overwritten the sum
# function. If your functions call sum, then you
# want to make sure that the original sum is used

# example from Hadley's R packages
nrow
dim <- function(x) c(1, 1) # write over dim function
dim(mtcars)
nrow(mtcars)
search()

# difference between loading and attaching
# a package
# first install testthat, if not already
# install.packages("testthat")

# using :: operator tells R what namespace to look in
# this also loads the package into memory
old <- search()
testthat::expect_equal(1, 1)
setdiff(search(), old)

# the package is only loaded, not attached, so
# functions are not yet available without ::
expect_true(TRUE)

# calling library attaches the package
library(testthat)
expect_equal(1, 1)
setdiff(search(), old)

# this works now
expect_true(TRUE)

# The Imports line in DESCRIPTION loads but does
# not attach packages
colMins
matrixStats::colMins


# first open NAMESPACE with a text editor
# (it should be nearly empty)
# NAMESPACE tells us which functions to
# export and import. More on that below

# Exporting functions:

# we need to export functions that we want the
# users to have access to directly, but we want
# to export the minimal number of functions.
# this is because exported functions can
# create conflicts with other functions
# to export a function, simply add
#' @export
# above where it is defined. Then call
devtools::document()
# this will write to NAMESPACE

# Importing Functions:

# As I wrote above, the Imports line in
# DESCRIPTION does not actually make the imported
# functions available without the :: operator.
# NAMESPACE imports do that.
# go ahead and add the line
#' @import matrixStats
# above the definition of scoreBracket and run
devtools::document()



# compiled Code

# R packages can also use compiled C or C++ code.
# to use Rcpp, run this command
devtools::use_rcpp()
# nothing in the src directory, but it's there now
# click File, New File, C++ File
# save the example file in src
# also add the two lines in the command console
# above scoringBracket
devtools::document()
devtools::load_all()
# it works!
timesTwo(c(4,7))


# automatically check your packages for problems
devtools::check()


# build your package
devtools::build()   # or cmd+shift+B
# close and restart R (if not done alrady)
library(tourney)
sv <- scoreBracket(c(1,2,1),c(4,2,2))
sv
simulateTourney(4,0.6)
# why didn't this work, and what do we have to do?




# Sharing your package

# You can put all of your source files in a
# public github repository. Then users can intall
# your package with
# devtools::install_github("username/packagename")

# If you want more people to use your package
# you should consider publishing it on CRAN
# this is a longer and more strict process, but
# worth the trouble if you want more visibility




