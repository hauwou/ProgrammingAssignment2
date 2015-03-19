## Put comments here that give an overall description of what your
## functions do

## These functions help demonstrate lexical scoping 
## rules used by R



## Write a short comment describing this function

## The makeCacheMatrix take an input in the form of a matrix,
## if no input param is provided, it will assign a matrix to an X var by default




makeCacheMatrix <- function(x = matrix()) {

  ## The x var belongs to this scope, it is declared as a 
  ## param of the function, no need to declare it again.
  
  invMatx <- NULL
  
  set <- function(y) {
    x <<- y ## if the set function is called, set x equals y input matrix
    invMatx <<- NULL ## reset matx if set is called because
                  ## matx will need to be recalulated with new input matrix
  }
  
  get <- function(){
    x ## Return x matrix if get function is called
  }
  
  setInverse <- function(inverse) {
    matx <<- inverse ## set matx to newly calculated inverse matrix
  }
  
  getInverse <- function() {
    invMatx ## return inverse matrix
  }
  
  ## This is just to create a list named "list" to contain
  ## the functions declared above, this list will be returned
  ## when makeCacheMatrix is called.
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

## This cacheSolve function solve for the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse() ## assign inverse matrix from getInverse to inv variable
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    ## if an inverse matrix existed (aka cached), return that matrix, exit function
  }
  
  data <- x$get() ## This gets executed when the "if" above is false (aka inverse matrix does not exist)
  
  inv <- solve(data, ...) ## solve for inverse matrix
  
  x$setInverse(inv) ## call and set inverse to cache
  
  return (inv)

  
  
}
