####################################################################
##                                                                ##
##    Programming Assignment 2: Caching the Inverse of a Matrix   ## 
##                                                                ##
##          Written and submitted by: Chris Havenstein            ##
##                                                                ##
####################################################################

## This R Script contains two functions which must be excuted in order.
## These two functions will create a matrix that may be cached and
## invert the matrix if it may be inverted. If the matrix has already
## been inverted, then the cached result will be returned instead of
## recalculating the inverted matrix.



## The makeCacheMatrix function makes a matrix that may be cached.

## Please provide makeCacheMatrix with a square matrix that may be inverted.
## (E.G.) x <- matrix(rnorm(25), 5,5)
##  test <- makeCacheMatrix(x)


makeCacheMatrix <- function(x = matrix()) {
  
  ##Make a cache matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## The cacheSolve function takes the output of the makeCacheMatrix function
## to first check if the matrix has already been inverted, and if so provide
## the cached data. Otherwise, cacheSolve will check to see if the matrix 
## can be inverted. If it can, then it will solve for the inverted matrix.


## Please provide cacheSolve with the output from the makeCacheMatrix function.
## (E.G.) test <- makeCacheMatrix(x)
##        cacheSolve(test)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## test to see if "data" matrix can be inverted
  test_intertiblity <- det(data)
  
  ##If matrix cannot be inverted, throw an error to user
  if(test_intertiblity == 0){
    message("Sorry, your matrix cannot be inverted because the determinant of the matrix is 0.")
    message("")
    message("Please provide a matrix as input that may be inverted.")
  }
  
  ##Else provide inverse of matrix  :)
  else {
    
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
  }
  
}