## This function() creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {	
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmean <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


# This function() computes the inverse of the special matrix returned by 
# makeCacheMatrix (above). If the inverse has already been calculated (and 
# not changed), then cacheSolve simply fetches the inverse matrix from 
# the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- (solve(matrix(data, ...)))	
              x$setmatrix(m)
              m
}