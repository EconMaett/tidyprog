# ****************************************************************
# 08 - Best practices ----
# URL: https://krlmlr.github.io/tidyprog/best-practices.html
# ****************************************************************

# It is useful to organize your code in a standardized way that is
# familiar to most users.

# - Create a "DESCRIPTION" file to declare dependencies and allow
# easy reloading of the functions you defined.

# - Sore your functions in .R files in the "R/..." directory of your project.
#   - Scripts that you execute live in a "script/..." directory.

# - Use the "roxygen2" package to document your functions close to the source.

# - Write tests for your functions with the "testthat" package.


## 8.1 Description ----

# Create and open a new RStudio project.
# Then, create a DESCRIPTION file with usethis::use_description():
usethis::use_description()
# Setting active project to 'C:/path/to/project'
# Writing 'DESCRIPTION'
# Package: tidyverse.programming
# Title: What the Package Does (One Line, Title Case)
# Version: 0.0.0.9000
# Authors@R (parsed):
#     * First Last <first.last@example.com> [aut, cre] (YOUR-ORCID-ID)
# Description: What the package does (one paragraph).
# License: `use_mit_license()`, `use_gpl3_license` or friends to pick a license.
# Encoding: UTF-8
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 7.0.0

# Double-check success
devtools::load_all()

# Declare that your package requires the tidyverse and the here package:
usethis::use_package(package = "here")
# Adding 'here' to Imports field in DESCRIPTION
# - Refer to functions with `here::fun()`

usethis::use_package(package = "tidyverse")
# Error: 'tidyverse' is a meta-package and it is rarely a good idea to depend on it.
# Please determine the specific underlying package(s) that offer the function(s) you need
# and depend on that instead.
# For data analysis projects that use a package structure but do not implement a formal
# R package, adding 'tidyverse' to Depends is a reasonable compromise.
# Call `usethis::use_package("tidyverse", type = "depends")` to achieve this.
usethis::use_package(package = "tidyverse", type = "depends")
# Adding 'tidyverse' to Depends field in DESCRIPTION
# - Ar you sure you want Depends? Imports is almost always the better choice.


## 8.2 R ----

# With a DESCRIPTIN file defined, create a new .R file and save it in the R/ directory.
# Create theis directory if it does not exist.
# Create a function and save the file:
# hi <- function(text = "Hello, world!") {
#   print(text)
#   invisible(text)
# }

# Do not source the file
# Restart R with Ctrl + Shift + F10 in RStudio
# Run devtools::load_all() again, you can use the shortcut
# Ctrl + Sift + L in RStudio.
devtools::load_all()

# Check that you can run hi() in the console:
hi()
# "Hello, world!"
hi(text = "Wow!")
# "Wow!"

# Edit the function:
# hi <- function(text = "Wow!") {
#   print(text)
#   invisible(text)
# }

# Save the file, but do not source it.
# Run devtools::load_all() or use the shortcut Ctrl + Shift + L.
devtools::load_all()
# Check that the new implementation of hi() is active:
hi()
# "Wow!"

# All functions that are required for your project are stored in the "R/" directory.
# All executable scripts are stored in the "script/" directory.


## 8.3 roxygen2 ----

# The following intuitive annotation syntax is a standard way to create documentation
# for your functions:
#' Print a welcome message
#' 
#' This function prints "Wow!", or a custom text, on the console.
#' 
#' @param text The text to print, "Wow!" by default.
#' 
#' @return Thee `text` argument, invisibly
#' 
#' @examples
#' hi()
#' hi("Hello!")

# This annotation can be rendered to a nice HTML page with the
# packages "roxygen2" and "pkgdown". All you need to do is provide and maintain it.


## 8.4 testthat ----

# Automated tests make sure that the functions you write today continue
# working tomorrow.
# Create your first test with usethis::use_test():
usethis::use_test(name = "hi")
# Adding 'testthat' to Suggests field in DESCRIPTION
# Adding '30 to Config/testthat/edition
# Creating 'tests/testthat/'
# Writing 'tests/testthat.R'
# Writing 'tests/testthat/test-hi.R'
# - Modify 'tests/testthat/test-hi.R'

# Run the new test with devtools::test(), you can use the shortcut
# Ctrl + Shift + T
devtools::test()
# One test is failing now.

# END