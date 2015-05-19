## In order to avoid unnecessary repetitions of cumbersome computation,
## "makeCacheMatrix" function will create the list of functions whose
## environmet includes a cache "m" for an inverse matrix. 
## "cacheSolve" computes the inverse, but bypasses the computation if
## the inverse matrix is already stored in m.

## makeChacheMatrix creates a "matrix", which carries the list of functions.

makeCacheMatrix <- function(x = matrix()) { # x is the argument matrix.
    m <- NULL                               # m is the storage for the inverse matrix.
    set <- function(y) {                    # "set" function substitutes a new matrix into x.
        x <<- y                             # x is in the parent environment, thus <<- is used here.
        m <<- NULL                          # The old cache is cleared out when the old matrix is replaced.
    } 
    get <- function() x                     # Get the current matrix.
    setsolve <- function(solve) m <<- solve # Store "solve" matrix into m. "solve" will be given by cacheSolveMatrix function.
    getsolve <- function() m                # Get the cached inverse matrix.
    list(set = set, get = get,              # Return the list of functions.      
         setsolve = setsolve,
         getsolve = getsolve)
}

## CacheSolve computes the inverse of the data matrix of x.
## However it avoids computation if the inverse matrix is already stored in x.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()                       # Get the currently-cached inverse matrix in x.
    if(!is.null(m)) {                       # See if any matrix is stored in m.
        message("getting cached data")      # If an inverse matrix is already cached,
        return(m)                           # then cacheSolve just returns the cache without computing again.
    }
    data <- x$get()                         # If an inverse matrix is not cached in m, then cacheSolve needs to compute it again.
    m <- solve(data, ...)                   # The inverse matrix is computed here.
    x$setsolve(m)                           # Using "setsolve" function, the computed inverse matrix is stored into x.
    m                                       # Return the freshly calculated inverse matrix. 
}


# TEST RUNNING ------------------------------------------------------------

Test <- makeCacheMatrix()                       # Test is the list of functions.
randMatrix <- matrix(runif(16),nrow=4,ncol=4)   # make an example 4*4 random matrix.

Test$set(randMatrix)                            # set the data into Test
cacheSolve(Test)                                # Cache an inverse matrix.
                                                # Since this is the 1st time to calcuate randMatrix's inverse,
                                                # this should just return the inverse matrix 
                                                # without the "getting cached data" message.
cacheSolve(Test)                                # Attempting to caching again.
                                                # Since this is the 2nd time, this should results in
                                                # "getting chached data" message, meaning that the
                                                # function bypassed the cumbersome computation.

round(randMatrix %*% cacheSolve(Test))          # Check the cached m is really the inverse matrix.
                                                # (I rounded the matrix to make it clear.)
