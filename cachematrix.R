## This code contains two functions 
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates the object that is the matrix and
## create four functions for object manipulation
## 1. set(y) set the value of te object (Exchange the object if needed/called)
## 2. get() returns x value
## 3. setinv(y) set the inverse matrix (does not compute the inverse)
## 4. getinv() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the Inverse matrix with the NULL value
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setinv <- function(y) mInv <<- y
    getinv <- function() mInv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function works almost the exact copy of cachemean 
## it checkes if the inverse matrix is cached
## if it is the function returns the cache otherwise make the inverse of the matrix object from makeCachematrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getinv()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setinv(mInv)
    mInv
}
