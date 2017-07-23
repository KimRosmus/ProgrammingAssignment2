## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {invs <- NULL
        set <- function(y) {						## set the value of the matrix
                x <<- y
                invs <<- NULL
        }
        get <- function() x						## get the value of the matrix
        setinverse <- function(inverse) invs <<- inverse    ## set the value of the inverse matrix
        getinverse <- function() invs				## get the value of the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {<- function(x, ...) {
        invs <- x$getinverse()
        if(!is.null(invs)) {					      ## if inverse is not NULL
                message("getting cached data")			## type message "getting cached data"
                return(invs)						## return the inverse matrix
        }
        mat <- x$get()							## get the Matrix data, not inverse
        invs <- solve(mat, ...)					## use solve to inverse the matrix
        x$setinverse(invs)						## set the inverse matrix
        return (invs)							## return a matrix that is the inverse of 'x'
}
