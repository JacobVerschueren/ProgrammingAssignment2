## The functions in this file take a invertable matrix and compute, store and/or retrieve its inverse

## This function will makes an matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) m <<- inversed
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will compute the inverse of the matrix from makeCacheMatrix and cache if not done so before
## If the inverse is already cached it retrieves this instead

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
