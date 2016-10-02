## Defines a matrix object with cachable inverse value

## Defines a Matrix object which can cache its inverse

makeCacheMatrix <- function( x = matrix() ) {
    inverse <- NULL
    set <- function( y ){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function( inv) inverse <<- inv
    getInverse <- function ( ) inverse
    list( set = set, get = get,
    setInverse = setInverse, getInverse = getInverse)
}


## Takes a cache Matrix object, checks if the inverse has already been
## computed, calculates if it has not, and then returns the inverse

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if( !is.null(inverse) ){
        message("getting cached data")
        return( inverse )
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse   
}
