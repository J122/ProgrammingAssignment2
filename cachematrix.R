## Put comments here that give an overall description of what your
## functions do

## the below function creates a matrix object that can cache the input matrix and its reverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # provides a default if cacheSolve has not yet been used
        set <- function(y) { # set the value of the matrix
                x <<- y  # caches the inputed matrix so that cacheSolve can check whether it has changed
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, # creates a list to contain the four functions
                setmatrix = setmatrix,
                getmatrix = getmatrix)
}


## calls functions stored in the special "matrix" returned by makeCacheMatrix (above). If the inverse has already been calculated,
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix() # if an inverse has already been calculated this gets it
        if(!is.null(m)){ # check to see if cacheSolve has been run before
                message("getting cached data")
                return(m)
        }
        y <- x$get() # run the getmatrix function to get the value of the input matrix
        m <- solve(y, ...) # compute the value of the inverse of the input matrix
        x$setmatrix(m)
        m # return the inverse
}
