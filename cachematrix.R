## These functions store a matrix and cache the matrix inverse

## Creates a list containing functions to set/get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {        
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Checks if matrix inverse has previously been calculated:
        ## if so, return inverse from cache
        ## if not, calculates matrix inverse and stores in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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