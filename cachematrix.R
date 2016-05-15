## Purpose of the two functions below is to calculate and cache the inverse of a square matrix
## If the inverse of a square matrix is already calculated and cached, then the cacheSolve
## function will pull the cached solution as opposed to re-performing the inverse function "solve"

## This function creates the vector that stores the matrix and inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## This function determines if the matrix inverse has already been calculated and then pulls
## the inverse from cache or calculates the inverse if not already done so

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve( data, ... )
    x$setinv(m)
    m
}
