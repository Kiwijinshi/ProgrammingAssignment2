## The combination of two functions allows to create and store
## in cache a matrix, then to return its inversion.

## Function description : creates in cache the special matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x' stored in cache

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting solved matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## These functions have been tested with the following matrix object :
## matest <- matrix(c(1,2,1,3,5,8,5,9,7), nrow = 3, ncol = 3)