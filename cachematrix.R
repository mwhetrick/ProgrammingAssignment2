##Matrix inversion is usually a costly computation and their may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse
        invr <- NULL
        
        # set matrix value
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        
        # get matrix value
        get <- function() x
        
        # set inverse matrix
        setinvr <- function(invr_) invr <<- invr_
        
        # get inverse matrix
        getinvr <- function() invr
        
        # list functions
        list(set = set, get = get,
             setinvr = setinvr, getinvr = getinvr) 
        }

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # is inverse already cached?
        invr <- x$getinvr()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        # if not cached, assign matrix
        data <- x$get()
        # solve(x) returns inverse of square matrix
        invr <- solve(data, ...)
        # cache it
        x$setinvr(invr)
        ## return the inverse of 'x'
        invr
}
