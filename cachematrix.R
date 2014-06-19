## matrix that allows caching of "compute inverse" operation

## create object (a list) that stores getters and setters for matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## check if matrix has cached, if not - compute inverse, cache and return it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
