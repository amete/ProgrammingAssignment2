## A simple R file that contains two functions (makeCacheMatrix 
## and cacheSolve) to demonstrate the concept of data caching in R.
## makeCacheMatrix creates a list of functions to set/get the
## properties of a matrix and its inverse,
## and cacheSolve provides means to calculate the inverse of
## this matrix and cache this information for future use 
## without the need of recomputation. 

## makeCacheMatrix is a function that returns a list of four functions:
## set, get         : Sets and gets the matrix
## setinv, getinv   : Sets and gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(z) inv <<- z
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## cacheSolve is a function that takes the return value (a list)
## of makeCacheMatrix, computes the inverse of the matrix
## only if it doesn't exist, and returns it.
## If the cached value is returned, a message is also printed
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
