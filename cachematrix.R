## cachematrix
## The following functions provide caching functionality when calculating the 
## inverse of a matrix. This ensures that the inverse is not recalculated if
## the previous calculation of the inverse is still up to date.

## makeCacheMatrix
## makeCacheMatrix constructs a matrix which allows its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    getInverse <- function() {
        inverse
    }
    
    setInverse <- function(newInverse) {
        inverse <<- newInverse
    }
    
    list(
        get = get,
        set = set,
        getInverse = getInverse,
        setInverse = setInverse
    )
    
}

## cacheSolve returns the inverse of the matrix x (which is created with
## makeCacheMatrix). It returns the cached result if x has not changed
## and the inverse was calculated previously. Otherwise, it calculates
## the inverse and caches it.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if (!is.null(inverse))
    {
        message("Getting cached inverse")
        return(inverse)
    }
    
    internalMatrix <- x$get()
    inverse = solve(internalMatrix, ...)
    x$setInverse(inverse)
    inverse
}
