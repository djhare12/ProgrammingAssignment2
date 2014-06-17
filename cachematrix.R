## This script caches the inverse of a matrix. If the matrix's 
## inverse has already been calculated, then the script will
## forgo the calculation and return the cached value.

## The makeCacheMatrix function will take a matrix as an argument
## and return a list. The list contains functions that...
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse by calculation
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## The cacheSolve function will take the list of functions that
## are output by the makeCacheMatrix function. It will check to
## see if the inverse has already been calculated for that matrix.
## If so, it will return that cached value. If not, it will calculate
## the inverse.

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
