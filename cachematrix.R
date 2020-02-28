## These functions establish a cache that can store the inverse of a matrix and can return the inverse of a matrix, saving calculation time by retrieving from the cache if possible.

## This function returns a list of functions that can be performed upon a matrix: get, set, getinverse, and setinverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function returns the inverse of a matrix.  If the inverse was previously calculated, then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        library(matlib)
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
