## R Programing, Assignment 2 (week 3)

## Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(a) {
    x <<- a
    inv <<- NULL
  }
  
  get <- function() 
    x 
  
  setInverse <- function(inverse) 
    inv <<- inverse
  
  getInverse <- function() 
    inv
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  ## return the inverse if its already set
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  inv <- solve(data, ...) 
  
  x$setInverse(inv)
  
  inv
}
