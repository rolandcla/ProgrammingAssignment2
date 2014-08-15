## Caching the Inverse of a Matrix.
## --------------------------------
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## For that purpose, it returns 4 access methods:
##  - set : set the matrix (to be inverted).
##  - get : get the matrix.
##  - setInverse : set (cache) the inverted matrix.
##  - getInverse : get (from cache) the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## The "cached" inverse
    cached_inv <- NULL
    
    ## The 4 access methods
    set <- function(y) {
        x          <<- y
        cached_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) cached_inv <<- inv
    getInverse <- function() cached_inv
    
    ## Return the 4 access methods
    list( set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse )
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## IF the "inverse" was already computed, return it...
    inv <- x$getInverse()
    if ( !is.null(inv) ) {
        # message("getting cached data")
        return( inv )
    }
    ## ELSE compute the "inverse", put it in the cache and return it...
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setInverse(inv)
    inv
}


