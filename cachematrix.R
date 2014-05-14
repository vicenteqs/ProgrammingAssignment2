## This is the implementation of the Programming Assigment 2. The main goal is 
## to cache the inverse of a matrix. For that matter, we will create a special 
## "matrix" object that will contain the original matter and some functions to 
## achieve our goal. There will be another function that will return the cached
## inverse of the matrix or calculate and cache it in case it wasn't.

## It is assumed that the matrix used will be always an square invertible matrix,
## as said in the assignment.

## This function acept as parameter a matrix and will create a 
## special "matrix" object which contains a list of functions(4):
##          - get: returns the matrix passed as parameter
##          - set: set the matrix and clears the cached inverse as the data has 
##            changed.
##          - setinverse: sets the cached inverse
##          - getinverse: returns the current inverse if cached
# x is the original matrix.
# i is the inverse. It is initialized to null.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() { i }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function acepts as parameter a special "matrix" object like the ones 
## created with makeCacheMatrix function
##
## It will return the inverse of the matrix in the special object. If the
## inverse isn't cached, the function will calculate it, set it in the special
## object and return its value. If it's cached it will just return value
## conteined in the special object.

cacheSolve <- function(x, ...) {    
    i <- x$getinverse()
    if(!is.null(i)) {
        # If the inverse is already cached, we just return it and inform that
        # it is cached data.
        message("getting cached data")
        return(i)
    }
    # In case the inverse is not cached, it gets the original matrix,
    # calculates the inverted one, cache it and return it.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i        
}
