##Programming Assignment 2: Lexical Scoping
## Making CacheMatrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function ( x = matrix()){
    inv <- NULL
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    set_inverse <- function(solve) inv <<- solve
    get_inverse <- function () inv
    
    list ( set = set, get = get, 
           set_inverse = set_inverse, get_inverse = get_inverse)
    
}


##Writing cacheSolve function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function (x, ...){
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message("getting cached data")
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

