## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set value of matrix
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get value of matrix
    getMatrix <- function() x
    ## set value of inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    ## get value of inverse matrix
    getInverse <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check if inverse has been calculated
    inv <- x$getInverse()
    if (!is.null(inv)) {
        ## if so, get mean from cache
        message("getting cached data")
        return(inv)
    } else {
        ## if not, calculate inverse of matrix
        dataMatrix <- x$getMatrix()
        inv <- solve(dataMatrix, ...)
        ## set value of inverse in the cache via setInverse
        x$setInverse(inv)
        inv
    }
}
