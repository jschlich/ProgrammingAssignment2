##      These functions cache the inverse of a matrix.
##
##      The first function, "makeCacheMatrix," creates a special "matrix", which is really a list containing 
##      a function to:
##
##              Set the value of the matrix
##              Get the value of the matrix
##              Set the value of the inverse
##              Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Set the value of the Matrix
        m <- NULL
        set <- function(y)      {
                
        x <<- y
        m <<- NULL
        }
        ## Get the value of the Matrix
        get <- function() x
        ## Set the value of the inverse
                setinv <- function(solve) m <<- solve
        ## Get the value of the inverse
                getinv <- function() m
                
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


##      The second function "cacheSolve" computes the inverse of the special "matrix" returned 
##      by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), 
##      then the "cachesolve" function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
                                        }


