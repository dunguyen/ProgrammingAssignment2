## makeCacheMatrix is a function that creates a matrix object that caches its 
## inverse.
## cacheSolve computes the inverse using the matrix object given by
## makeCacheMatrix. If the inverse has already been calculated then cacheSolve
## will return the inverse from the cache

## makeCacheMatrix is a function that creates a matrix object that caches its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse using the matrix object given by
## makeCacheMatrix. If the inverse has already been calculated then cacheSolve
## will return the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv))
        {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
