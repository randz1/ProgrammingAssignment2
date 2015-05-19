## Rprogramming Programming Exercise 2

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions are used to create matrices whose inverse is
## cacheable


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## by using cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        ## initialisations
        inv <- NULL
        
        ## set: sets matrix data. resets inv to NULL to invalidate cached
        ## inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get: gets matrix data 
        get <- function() x
        
        ## setinv: sets matrix inverse by calling solve(...)
        setinv <- function(solve) inv <<- solve
        
        ## getinv: gets matrix inverse
        getinv <- function() inv
        
        ## exposes set, get, setinv and getinv methods
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check to see if a cached inverse exists
        ## and if so, return the cached inverse
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        
        ## otherwise compute the inverse by calling solve(...)
        ## and then cache the result before retuning it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)        
        inv
}
