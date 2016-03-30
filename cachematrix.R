## These functions cache the inverse of a matrix, so that it is not re-computed if it already exists.
## This is programming assignment 2 from the Coursera Course Learn R-Programming
## Andy Pickering

## This function creates a list containing a function to calculate the inverse of a matrix and cache
# for repeated calculations. Modeled after makeVector.R
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function comptues the inverse of a matrix. If the result is already cached, it uses that 
# instead of re-computng. Modeled after cachemean.R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
