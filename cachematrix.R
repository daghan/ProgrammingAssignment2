## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and generates a list that 
## contains for elements. The purpose is to cache the 
## inverse value of the original matrix
## x$set() sets / resets the matrix value
## x$get() returns the current matrix value
## x$setinv() sets the inverse of the matrix
## x$getinv() returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes a cachec matrix structure
## and checkes to see if the inverse value is 
## already computed. If not it computes the inverse
## of the matrix and caches the result in the original
## cached matrix data structure
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
