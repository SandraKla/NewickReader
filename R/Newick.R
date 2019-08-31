#' A Newick-reader
#'
#' This function allows you to read Newick-Strings
#' @param Newick-String
#' @export Dataframe with the Level in the Newick-String
#' @examples ((A,B),C)
#' Newick_reader()

Newick_reader <- function(x) {
  
  x <- strsplit(x,"") 
  
  for (i in x){ #Search the different characters
      letter <- grepl("[A-Z]",i)
      bracket_open <- grepl("\\(",i);
      bracket_closed <- grepl("\\)",i);
      comma <- grepl(",",i)
      letters <- i[letter]}

  for (i in seq(1,length(letter))){
    if (letter[i]==FALSE && bracket_open[i] == FALSE && bracket_closed[i] == FALSE &&
        comma[i] == FALSE){
    stop("Please check your input! An Example for a Newick String is ((A,B),C). Use only capital letter, brackets or commas!")}}

  index_open = 0
  brackets_open = c()
  index_closed = 0
  brackets_closed = c()
  
  # Count the open brackets to each character of the string
  for (i in bracket_open){
    if (i == TRUE)
      {index_open = index_open + 1} 
    brackets_open = rbind(brackets_open,index_open)}
  
  # Count the closed brackets to each character of the string
  for (j in bracket_closed){
    if (j == TRUE)
    {index_closed = index_closed - 1}; 
    brackets_closed = rbind(brackets_closed,index_closed)}

  data_newick <- data.frame(brackets_open,brackets_closed,letter) 
  
  # Calculate the sum of the index from the open and the closed brackets
  # Make a Data Storage for the letters and their levels
  sum_brackets <- brackets_open+brackets_closed
  level_newick <- sum_brackets[letter]
  storage <- cbind(data.frame(letters), level_newick)
  
  # Level is between 1 and the number of letters, otherwise false opened or closed brackets
  if (min(storage$level_newick)<1){stop("Make sure if your brackets are properly opened and closed")}
  if (max(storage$level_newick)>nrow(storage)){stop("Make sure if your brackets are properly opened and closed")}
  
  # Print the tree for the Newick String
  for(j in seq(0,length(letters))){
    index <- level_newick[j]
    letter_newick <- letters[j]
  for (i in index){
    replication <- rep(" ",i*5)
    print(cat(replication,letter_newick, fill = TRUE))}
  }
 return(storage)
}