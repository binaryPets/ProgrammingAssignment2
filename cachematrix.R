## Describtion of the Functions :
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##      the inverse from the cache.

## creates a special "matrix" object that can cache its inverse.
## Arguments:
##      x       an invertible matrix 
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inversedMat) inverse <<- inversedMat?
        getInverse <- function() inverse

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" Object.
## details: 
##      If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.
## Arguments:
##      x       matrix Object (returned By the function makeCacheMatrix)
##      ...     one or more matrix Objects to have there inversed matrix calculated
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
}
