library(testthat)
context("Newick.R")

dataset  <- "((A,B),(((C,(D,E)),F),G),H)"
dataset1 <- "((A,B),C)"
dataset2 <- "(((A,B),(C,(D,E)),F),G,H)"
dataset3 <- "((((A,B),C),D),E)"
dataset4 <- "(A,B)"

dataset_false <- "(A,B),?)"
dataset_false_levels <- "((((A,B)"
dataset_false_brackets <- "(A,B),C)"

result  <- data.frame(Letter= c("A","B","C","D","E","F","G","H"), Level = c(2,2,4,5,5,3,2,1))
result1 <- data.frame(Letter= c("A","B","C"), Level = c(2,2,1))
result2 <- data.frame(Letter= c("A","B","C","D","E","F","G","H"), Level = c(3,3,3,4,4,2,1,1))
result3 <- data.frame(Letter= c("A","B","C","D","E"), Level = c(4,4,3,2,1))
result4 <- data.frame(Letter= c("A","B"), Level = c(1,1))

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
  expect_identical(NewickReader(dataset) , result)
  expect_identical(NewickReader(dataset1), result1)
  expect_identical(NewickReader(dataset2), result2)
  expect_identical(NewickReader(dataset3), result3)
  expect_identical(NewickReader(dataset4), result4)
  
  # Is the max level_newick =< number of letters and greater than 0?
  expect_lte(max(NewickReader(dataset)$Level) ,rows_data)
  expect_lte(max(NewickReader(dataset1)$Level),rows_data1)
  expect_lte(max(NewickReader(dataset2)$Level),rows_data2)
  
  expect_gt(max(NewickReader(dataset)$Level) ,0)
  expect_gt(max(NewickReader(dataset1)$Level),0)
  expect_gt(max(NewickReader(dataset2)$Level),0)})

test_that("Shape from the dataframe",{
  
  expect_is(NewickReader(dataset), "data.frame")
  expect_is(NewickReader(dataset)$Letter, "factor")
  expect_is(NewickReader(dataset)$Level, "numeric")
  
  # Expect output is 2 columns [letters und level_newick] and as many rows as letters
  # Check with the first three datasets
  expect_equal(ncol(NewickReader(dataset)) , 2)
  expect_equal(ncol(NewickReader(dataset1)), 2)
  expect_equal(ncol(NewickReader(dataset2)), 2)
  expect_equal(nrow(NewickReader(dataset)) , rows_data)
  expect_equal(nrow(NewickReader(dataset1)), rows_data1)
  expect_equal(nrow(NewickReader(dataset2)), rows_data2)})

test_that("Check if error when false Newick-Strings",{

  expect_error(NewickReader(dataset_false))
  expect_error(NewickReader(dataset_false_brackets))
  expect_error(NewickReader(dataset_false_levels))})
