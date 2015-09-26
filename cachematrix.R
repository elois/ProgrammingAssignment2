## Put comments here that give an overall description of what your
## functions do
## 

## Write a short comment describing this function
## This function creates the object that is the matrix and
## create four function for object manipulation
## 

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the Inverse matrix with the empty one
    mInv <- NULL
    ##Exchange the object if needed/called
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


## Write a short comment describing this function
## this function works almost the exact copy of cachemean 
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
