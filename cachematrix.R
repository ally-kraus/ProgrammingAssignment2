## Creates the inverse of a Matrix and caches it 

## This function creates a special "matrix" object that can cache 
## its inverse. Will only work on invertable matricies.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(solve) {
        m <<- solve
    }
    getInverse <- function() {
        m
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function.  If the inverse 
## has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
