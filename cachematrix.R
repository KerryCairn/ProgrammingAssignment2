## This pair of functions, makeCacheMatrix and cacheSolve, can compute the
## inverse of a matrix and cache the result.

## The makeCacheMatrix function creates a "special" matrix object that can 
## cache the inverse of the matrx.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## is unchanged), then the cachesolve function retrieves the cached inverse.

cacheSolve <- function(x, ...) {
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
