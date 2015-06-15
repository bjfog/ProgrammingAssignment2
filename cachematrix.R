#-----------------------------------------------------------------------------------
# cachematrix.R
#-----------------------------------------------------------------------------------
# This file contains two functions:
#
#   makeCacheMatrix - this creates a new matrix object type that can cache the
#                     inverse of the original matrix once it has been calculated.
#                     If the inverse is available in the cache, then that is 
#                     returned in preference to needing to recalculate the value.
#
#   cacheSolve      - this takes a matrix object of the type returned by makeCacheMatrix
#                     and returns the inverse, either from a cached copy or by
#                     solving the inverse for the first time.
#
# Note that there is an assumption that the matrix used in 'makeCacheMatrix' is
# invertable. However, this is not examined until the 'cacheSolve' function is
# actually invoked. 
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
##  makeCacheMatrix
##
## This function creates an object that contains a matrix, may contain its inverse,
## and returns a list of functions that perform the following operations on that
## object:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix (if invertible, NA otherwise)
##   4. get the value of the inverse of the matrix (if invertible, from cache if there)
makeCacheMatrix <- function(x = matrix()) {
    ## We perform a sanity check here, to make sure that x is a matrix, and that it
    ## is at least a square matrix:
    if (!is.matrix(x)) {
        message("WARNING: non-matrix argument")
        return(NULL)
    }
    d <- dim(x)
    if (d[1] != d[2]) {
        message("WARNING: non-square-matrix argument")
        return(NULL)
    }
    ## We instantiate a null matrix here. This will hold the inverse if there is one:
    IM <- NULL
    ## The following function is used to set the matrix object to the matrix passed in:
    set <- function(Y){
        x <<- Y
        IM <<- NULL
    }
    ## The following function just returns the original matrix:
    get <- function() x
    ## The following function assigns the value of the inverse to M:
    setInverse <- function(inverse) IM <<- inverse
    ## The following functions just returns the value of M to the caller:
    getInverse <- function() IM
    ## Now that the four operations have been defined, we return a list that
    ## can be used to invoke them on this object:
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
##  cacheSolve
##
## The following function takes an argument that must be the same type as that
## returned by the 'makeCacheMatrix' function above.
##
## If the inverse of this matrix has already been calculated, then the 'cacheSolve' 
## function will return the cached value of the this inverse. It will also print a
## message indicating the cached value has been returned. This message may be
## supressed by using the 'quiet = TRUE' option when the function is invoked.
##
## If the inverse of this matrix has not already been calculated, then the 'cacheSolve'
## function will calculate it (or throw an error if it's not invertable), cache the
## inverse and return that inverse as the result.
##
cacheSolve <- function(x, quiet = FALSE, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## First off, we check to see if the inverse has already been calculated. Note
    ## that 'x' must have been created by the makeCacheMatrix function, or an error
    ## will be thrown:
    Query <- x$getInverse()
    ## If Query is NULL, then the inverse hasn't been calculated yet. We do so, and
    ## assign it to the cache for later:
    if (!is.null(Query)) {
        if (!quiet) message("Retrieving cached inverse.")
        return(Query)
    }
    ## At this point, we know that there was no cached inverse yet. We calculate it, 
    ## cache it and return it:
    Mtemp <- x$get()
    IMtemp <- solve(Mtemp, ...)
    x$setInverse(IMtemp)
    IMtemp
}
