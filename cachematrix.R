## This function is use to cache a inverse matrix.
##
## First you should use makeCacheMatrix(x) function to make
## a custom matrix include set(),get(),setsolve(),getsolve()
## function. The argument x is the original matrix.
##
## Then you can use cacheSolve(x) to get inverse matrix of
## original matrix. Here the argument x is the custom matrix
## we just made.

## Make a custom cache matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Get a inverse matrix of cache matrix
## When the cache data is null then try to caculate inverse
## matrix and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
