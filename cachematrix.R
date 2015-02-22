## Following functions compute the inverse matrix of the given matrix
## or get cache data if it was already computed.

## Function makeCacheMatrix creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
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
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    
    if(!is.null(inv)) {
        message("Getting cashed data.")
        #return inv
    } else {
        inv <- solve(x$get())
        x$setsolve(inv)
    }
    inv
}
