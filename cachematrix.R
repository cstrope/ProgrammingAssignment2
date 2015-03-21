## These functions cache the inverse of a given matrix to reduce the computation
## time.

## This function supplies a list of functions for setting the inverse of the matrix x 
## and getting into cache. 
makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinv <- function(mean) matinv <<- mean
    getinv <- function() matinv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function checks to see if the inverse of the matrix has been cached. If so, it
## retrieves the cached value. Otherwise, it calculates the inverse, and sets the value
## into cache (setinv)
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    matinv <- x$getinv()
    if(!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    data <- x$get()
    matinv <- solve(data, ...)
    x$setinv(matinv)
    matinv
}
