## Put comments here that give an overall description of what your
## functions do

## These functions help demonstrate lexical scoping 
## rules used by R



## Write a short comment describing this function

## The makeCacheMatrix takes an input in the form of a matrix,
## if no input param is provided, it will assign a matrix to an X var by default

## Testing process
## Type: a<-makeCacheMatrix(), this will create an "a" object, with empty 'x' matrix
## Type: a$get(), will return NULL because 'x' matrix is empty
## Type: a$set(matrix(1:4,2,2)) to set 'x' OR Type: a<-makeCacheMatrix(matrix(1:4,2,2)) to skip some steps
## Type: a$get(), and see the matrix that just got set
## Type: a$getInverse(), see NULL since the cacheSolve has not been called
## *** this current version does not prevent the direct call to set inverse >> a$setInverse(matrix(1:4,2,2))
## Type: cacheSolve(a), this will return the inverse matrix WITHOUT the message "getting cached data"
## Type: cacheSolve(a) again, will return the inverse matrix with the message "getting cached data"


makeCacheMatrix <- function(x = matrix()) {

  ## The 'x' var belongs to this scope, it is declared as a 
  ## param of the function, no need to declare it again.
  
  invMatx <- NULL
  
  set <- function(y) {
    x <<- y ## if the set function is called, set x equals to the y input matrix
    invMatx <<- NULL ## reset matx if set is called because
                  ## matx will need to be recalulated with new input matrix
  }
  
  get <- function(){
    x ## Return x matrix if get function is called
  }
  
  setInverse <- function(inverse) {
    invMatx <<- inverse ## set matx to newly calculated inverse matrix
  }
  
  getInverse <- function() {
    invMatx ## return inverse matrix
  }
  
  ## This is just to create a list named "list" to contain
  ## the functions declared above, this list will be returned
  ## when makeCacheMatrix is called. To call functions, use "varName"$get()
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

## This cacheSolve function solve for the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Start the clock!
  ptm <- proc.time()
  ##print(ptm)
  inv <- x$getInverse() ## assign inverse matrix from getInverse to inv variable
  
  if(!is.null(inv)) {
    message("getting cached data")
    print(proc.time() - ptm)
    return(inv)
    ## if an inverse matrix existed (aka cached), return that matrix, exit function
  }
  
  data <- x$get() ## This gets executed when the "if" above is false (aka inverse matrix does not exist)
  
  inv <- solve(data, ...) ## solve for inverse matrix, solve() is a built in function
  
  x$setInverse(inv) ## call and set inverse to cache
  
  Sys.sleep(2) ## simulate long calculation process
  
  # Stop the clock
  print(proc.time() - ptm)
  
  return (inv)  
  
}
