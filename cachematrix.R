## Put comments here that give an overall description of what your
## functions do


## my solution: the two functions makeCacheMatrix and cacheSolve try to find the inverse of a computed matrix

## makeCacheMatrix stores a matrix and a cached value to create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # set the initial value to null 
  elc <- NULL
  # store a matrix
  set <- function(y) { 
    x <<- y
    elc <<- NULL
  }
  
  get <- function() x
  setreverse<- function(reverse) elc <<-reverse
  getreverse <- function() elc
  
# return a list
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## cacheSolve computes the inverse of the  matrix created by 
## makeCacheMatrix. The function would retrieve the inverse from the cache
## if the inverse has already been calculated.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  elc <- x$getreverse()
  if (!is.null(elc)) {
    message("getting cached reververse matrix")
    return(elc)
  } else {
    elc <- solve(x$get())
    x$setreverse(elc)
    return(elc)
  }
}


## Test my functions
## m1 = matrix(c(1,2,3,4),ncol = 2)
## x = makeCacheMatrix(m1)

## > cacheSolve(x)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(x)
## getting cached reververse matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
