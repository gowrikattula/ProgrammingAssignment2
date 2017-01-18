

## This function creates an object with input 
## matrix and set and get functions for both matrix
## and its inverse
 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse = setinverse,
       getinverse=getinverse)
}


## This function retrieves the inverse of the matrix
## from above functions object and if the inverse doesn't
## exist in the cache it calculates the inverse and stores
## in the cache

cacheSolve <- function(z, ...) {
  inv <- z$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- z$get()
  inv <- solve(data, ...)
  z$setinverse(inv)
  inv
       
}
