library(testthat)
context("Newick.R")

# Run the script: "Newick.R"
# Test the code from the Newick-string with test_file("test_newick.R") 

dataset <- "((A,B),(((C,(D,E)),F),G),H)"
dataset1 <- "((A,B),C)"
dataset2 <- "(((A,B),(C,(D,E)),F),G,H)"
dataset3 <- "((((A,B),C),D),E)"
dataset4 <- "(A,B)"

dataset_false_levels <- "((((A,B)"
dataset_false <- "(A,B),?)"
dataset_false_brackets <- "(A,B),C)"

result  <- data.frame(letters= c("A","B","C","D","E","F","G","H"),level_newick = c(2,2,4,5,5,3,2,1))
result1 <- data.frame(letters= c("A","B","C"),level_newick = c(2,2,1))
result2 <- data.frame(letters= c("A","B","C","D","E","F","G","H"),level_newick = c(3,3,3,4,4,2,1,1))
result3 <- data.frame(letters= c("A","B","C","D","E"),level_newick = c(4,4,3,2,1))
result4 <- data.frame(letters= c("A","B"),level_newick = c(1,1))

# Get the letters and count them for the first three datasets
split_data <- strsplit(dataset,"") 
for (i in split_data){
  rows_data <- length(i[grepl("[A-Z]",i)])}

split_data1 <- strsplit(dataset1,"") 
for (i in split_data1){
  rows_data1 <- length(i[grepl("[A-Z]",i)])}

split_data2 <- strsplit(dataset2,"") 
for (i in split_data2){
  rows_data2 <- length(i[grepl("[A-Z]",i)])}

test_that("Right results",{ 

  # Compare input with the expected result
  expect_identical(myProgram(dataset) , result)
  expect_identical(myProgram(dataset1), result1)
  expect_identical(myProgram(dataset2), result2)
  expect_identical(myProgram(dataset3), result3)
  expect_identical(myProgram(dataset4), result4)
  
  # Is the max level_newick =< number of letters and greater than 0?
  expect_lte(max(myProgram(dataset)$level_newick) ,rows_data)
  expect_lte(max(myProgram(dataset1)$level_newick),rows_data1)
  expect_lte(max(myProgram(dataset2)$level_newick),rows_data2)
  
  expect_gt(max(myProgram(dataset)$level_newick) ,0)
  expect_gt(max(myProgram(dataset1)$level_newick),0)
  expect_gt(max(myProgram(dataset2)$level_newick),0)})

test_that("Shape from the dataframe",{
  
  expect_is(myProgram(dataset), "data.frame")
  expect_is(myProgram(dataset)$letters, "factor")
  expect_is(myProgram(dataset)$level_newick, "numeric")
  
  # Expect output is 2 columns [letters und level_newick] and as many rows as letters
  # Check with the first three datasets
  expect_equal(ncol(myProgram(dataset)) , 2)
  expect_equal(ncol(myProgram(dataset1)), 2)
  expect_equal(ncol(myProgram(dataset2)), 2)
  expect_equal(nrow(myProgram(dataset)) , rows_data)
  expect_equal(nrow(myProgram(dataset1)), rows_data1)
  expect_equal(nrow(myProgram(dataset2)), rows_data2)})

test_that("Check if errors came from the false Newick-Strings",{

  expect_error(myProgram(dataset_false))
  expect_error(myProgram(dataset_false_brackets))
  expect_error(myProgram(dataset_false_levels))})
