## Cached inverse matrix computation
## makeCacheMatrix will create list of functions
## cacheSolve will return the inverse of the matrix
##
## Example:
## > x <- matrix(c(1,0,2,1), nrow=2, ncol=2)
## > xxx <- makeCacheMatrix(x)
## > cacheSolve(xxx)              # computation
## > cacheSolve(xxx)              # cached access


## Creates list of functions for manipulating the matrix and its 
## inverse
## @param x matrix to invert
makeCacheMatrix <- function(x = matrix())
{
    # define inverse
    inverse <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse matrix
    setInverse <- function(pinverse) inverse <<- pinverse
    
    # get the value of the inverse matrix
    getInverse <- function() inverse
    
    # return list of functions
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Returns inverse of the matrix
## first it looks for the cached value
## if no cache exists it computes it
## @param x list of functions to manipulate the matrix
cacheSolve <- function(x, ...)
{  
    # try to get the value from the cache
    inverse <- x$getInverse()
    
    # if cache exists, return that value
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # otherwise compute it
    data <- x$get()
    inverse <- solve(data, ...)
    
    # remember the value for the future use
    x$setInverse(inverse)
    
    # finally return the result
    inverse
}
