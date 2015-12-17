########
## This file has 2 functions.
## 1. makeCacheMatrix: caches an inverse of a given matrix.
## 2. cacheSolve: Returns an inverse of a given matrix.
##                It uses makeCacheMatrix() to cache.
########

####
## makeCacheMatrix creates a matrix object and caches it.
##
## Input:  A matrix that needs to be inverted.
## Output: depends on the "method" called with-in.
##         
####

makeCacheMatrix <- function(x = matrix()) {
    ## Cache an inverse of a given matrix.
    
    inv <- NULL # Initialize the inverse matrix object to NULL.
    
    setmatrix <- function(inputmatrix) {
        x <<- inputmatrix  # Cache the input matrix
        inv <<- NULL # Cache the inverse of the input matrix.
    }
    
    # get the passed matrix as is.
    getmatrix <- function() x 
    
    # Set and cache the inverse of input matrix.
    setinverse <- function(inputmatrix) inv <<- inputmatrix
    
    # Get the cached inverse matrix
    getinverse <- function() inv
    
    # Register List of Functions implemented.
    list(set = setmatrix, get = getmatrix, setinv = setinverse, getinv = getinverse)
    
}

####
## cacheSolve returns an inverse of a given matrix.
##
## Input:  A matrix that needs to be inverted.
## Output: Inverse of the given matrix.
##         Check to see if a cached inverse matrix is avialabe if so return it.
##         else compute an inverse of the given matrix, calls makeCacheMatrix()
##         to cache the inverse.
####

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    # Try and get the cached inverse of the matrix x.
    inv <- x$getinv()
    
    # If found return it.
    if (!is.null(inv))  return(inv)
    
    # Get the original matrix.
    data <- x$get()
    
    # Compute the inverse of the matrix.
    inv <- solve(data)
    
    # Cache the inverse of the matrix.
    x$setinv(inv)
    
    # Return the inverted matrix
    inv
    
}
