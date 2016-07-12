## In this library there are two functions; makeCasheMatrix and cacheSolve.
## The purpose of the library is to create a matrix and its inverse.
## Matrix Inverse is saved in the environment cache to save inverse process time and
## resources.

## Input: a variable of type "Matrix" that will be used to create the inverse
## output: function returns a list of commands that can be used to access the main
## matrix and the inversed one.

makeCacheMatrix <- function(x = matrix()) {
    my_inversed <- NULL
    set <- function(y) {
        x <<- y
        my_inversed <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) my_inversed <<- solve
    getInverse <- function() my_inversed
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## input: an object of type function that is created by makeCacheMatrix.
## output: the inversed matrix of the input function
## This function tries to get the cached inversed matrix from the main input.
## If the inversed matrix is not calculated before, this functions calculates it.
## If the inversed matrix is created before, this function returns the cached inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat_inverse <- x$getInverse()
    if(!is.null(mat_inverse)) {
        message("getting cached data")
        return(mat_inverse)
    }
    data <- x$get()
    mat_inverse <- solve(data, ...)
    x$setInverse(mat_inverse)
    mat_inverse
}
