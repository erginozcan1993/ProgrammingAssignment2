## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)){ 
    if(x$setmatrix() == x$getmatrix()) 
      return(m)
  }
  y <- x$getmatrix() 
  x$setmatrix(y) 
  m <- solve(y, ...) 
  x$setinverse(m)
}
