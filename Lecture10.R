rm(list=ls()) #clean, clc, close all
# ***************
# R version 4.4.2 / win
# author: Nikola Jordánová
# *************

# Path
setwd('V:/MPA-PRG/exercise_10') # set working directory

library(Biostrings)

# Task 1
SuffixArray <- function(DNA_object){
  DNA_object <- as.character(DNA_object)
  list_of_suffixes <- vector("list", length(DNA_object))
  
  for (i in 1:nchar(DNA_object)){
    list_of_suffixes[[i]] <- substr(DNA_object, i, nchar(DNA_object))
  }
  
  suffix_array <- order(unlist(list_of_suffixes))
  #list_of_suffixes <- sort(unlist(list_of_suffixes))
  
  #return (list_of_suffixes)
  return (suffix_array) #vector of integers 
}

DNA <- DNAString("CTAATAATG")
SuffixArray(DNA)


# Task 2
InverseSuffixArray <- function(suffix_array){
  offset <- 1:length(suffix_array)
  inverse_suffix_array <- integer(length(suffix_array))
  
  for (i in 1:length(offset)){ # procházíme čísla od 1 do 9
    for (j in 1:length(suffix_array)){ # procházíme čísla suffix array
      if (offset[i] == suffix_array[j]){
        inverse_suffix_array[[i]] <- offset[j]
      }
    }
    
  }
  
  
  return (inverse_suffix_array)
}


DNA <- DNAString("CTAATAATG")
#SuffixArray(DNA)
InverseSuffixArray(SuffixArray(DNA))


# Task 3
LCPArray <- function(text, SA, ISA){
  text <- as.character(text)
  n <- nchar(text)
  
  LCP <- integer(length(SA) + 1)
  LCP[1] <- -1
  LCP[length(SA) + 1] <- -1
  
  l <- 0
  
  for (i in 1:n){
    j <- ISA[i]
    if (j > 1){
      k <- SA[j - 1]
      while (i + l <= n &&
             k + l <= n &&
             substring(text, i + l, i + l) ==
             substring(text, k + l, k + l)) {
        l <- l + 1
      }
      LCP[j] <- l
      l <- max(l - 1, 0)
    }
  }
  
  return(LCP)
}

text <- DNAString("CTAATAATG")
SA <- SuffixArray(DNA)
ISA <- InverseSuffixArray(SA)
print(LCPArray(text, SA, ISA))


# Task 4
BinarySearchSA <- function(pattern, text, SA){
  
  text <- as.character(text)
  pattern <- as.character(pattern)
  m <- nchar(pattern)
  
  minIndex <- 1
  maxIndex <- length(SA)
  
  # ---- find First ----
  while (minIndex < maxIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    suffix <- substring(text, SA[midlIndex], nchar(text))
    prefix <- substr(suffix, 1, m)
    
    if (pattern <= prefix){
      maxIndex <- midlIndex
    } else {
      minIndex <- midlIndex + 1
    }
  }
  
  First <- minIndex
  maxIndex <- length(SA)
  
  # ---- find Last ----
  while (maxIndex > minIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    suffix <- substring(text, SA[midlIndex], nchar(text))
    prefix <- substr(suffix, 1, m)
    
    if (prefix <= pattern){
      minIndex <- midlIndex + 1
    } else {
      maxIndex <- midlIndex
    }
  }
  
  Last <- maxIndex - 1
  
  if (Last < First){
    return("Pattern does not appear in text")
  } else {
    return(c(First, Last))
  }
}




text <- DNAString("CTAATAATG")
SA <- SuffixArray(text)
print(SA)
pattern <- DNAString("AT")

BinarySearchSA(pattern, text, SA)
