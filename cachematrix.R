## The programming assignment was to create a pair of functions that carry out a matrix inverse,
## caching the result to avoid repeating the calculation. The solution below follows the 
## pattern of the vector mean example very closely.

## This function creates a list of setters and getters for the matrix
## and its inverse. It is just a matrix version of makeVector.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse.in) inverse <<- inverse.in
        getinverse <- function() inverse
        list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)    
}


## This function checks if the inverse has been cached and uses
## the result if it has. Otherwise, it carries out the inverse by calling solve.
## It is very similar to cachemean.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        mat.temp <- x$get()
        inverse <- solve(mat.temp, ...)
        x$setinverse(inverse)
        inverse
}
