## JHU - R Programing course - WK2 Assignment 2

## Purpose : Caching the Inverse of a Matrix 
## Details : These functions can be used to define a square matrix and fetch the inverse of it from cache or compute it if its not already available.
##           (This illustrates application of Lexical Scoping & Usage of functions as parameters)
## 

# Version : 1.0
# Author : thenuv
# Created on : 2018-06-22
# Modified on : 2018-06-22

## makeCacheMatrix defines functions to assign/fetch a matrix value in/from cache, 
## defines/gets the inverse of the matrix if available in cache 
## It has 4 functions and 2 matrix variable and returns a list of all functions
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  define <- function (funcMCM) {
    x <<- funcMCM
    i <<- NULL
  }
  
  fetch <- function() x
  
  setInverse <- function (inv) i <<- inv
  
  getInverse <- function () i
  
  list (
    define = define,
    fetch = fetch,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
}


## cacheSolve takes an instance x of makeCacheMatrix function as parameter
## Uses the inverse of the matrix x if available in cache else sets and returns using solve funtion
cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  
  if(!is.null(i)){
    #print("Fetching data from Cache")
    return(i)
  }
  
  mtx <- x$fetch()
  i <- solve(mtx, ...)
  x$setInverse(i)
  i
  
}

## Sample calls for testing 
# m <- matrix (rnorm(9), nrow = 3, ncol = 3)
# mc <- makeCacheMatrix(matrix (numeric(), nrow = 3, ncol = 3))
# mc$fetch()
# mc$getInverse()
# mc$define(m)
# cacheSolve(mc)
# mc$getInverse()
