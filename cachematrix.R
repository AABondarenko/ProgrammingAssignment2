## Following functions compute the inverse matrix of the given matrix
## or get cache data if it was already computed.

## Function makeCacheMatrix creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         # 'inv' will contain the value of the inverse matrix
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) {
        inv <<- solve(x)
    }
    
    getsolve <- function() inv
    
    ## List of methods:
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function cacheSolve returns cashed value of the inverse matrix
## or computes it for a matrix from makeCacheMatrix if cashed value is NULL.

cacheSolve <- function(x, ...) {
    ## getting the value of inverse matrix.
    inv <- x$getsolve()
    
    ## If the value is not NULL then print cashed data
    ## else compute inverse matrix and cache the value.
    if(!is.null(inv)) {
        message("Getting cashed data")
    } else {
        inv <- solve(x$get())
        x$setsolve(inv)
    }
    inv
}