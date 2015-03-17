## The following funtions describe two methods that can be used to keep a cache of the
## inverse of a matrix, to speed-up calculations

# Creates an object that can be used by cacheSolve() to cache the inverse of a matrix
# @param x Matrix with the required data
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # inverse
        
        # Change the matrix and 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Returns the matrix
        get <- function() x

        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv

        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse)
}

## Function that leverages the object created by makeCacheMatrix() to cache the inverse
## of the original matrix
# @param x Object created by calling makeCacheMatrix()
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
