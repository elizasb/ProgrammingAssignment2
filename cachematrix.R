## File: cachematrix.R
## Created: 2014-10-18
## Author: E.S.B.
## Association: Coursera R Programming Course - 
## -- https://class.coursera.org/rprog-008
## -- Programming Assignment 2: Lexical Scoping
## Purpose - Cache the inverse of a matrix
## Includes - Two (2) functions (makeCacheMatrix, cacheSolve)
## -- makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.
## -- cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve should
##    retrieve the inverse from the cache.

## To TEST:
## - based on discussion forum: 
## - https://class.coursera.org/rprog-008/forum/thread?thread_id=174
## m<-matrix(c(1,2,3,4),nrow=2,ncol=2)
## m<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## m$get()
## cacheSolve(m) # - first times calculates inverse, since not in cache
## cacheSolve(m) # - second time says "getting cached data" 
## m$getinverse() # - another way to return the inverse for m
## m$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix (sets 
##      inverse to NULL)
## cacheSolve(m) # Computes, caches, and returns new matrix inverse

## CODE:

## makeCacheMatrix
## - This function creates a special "matrix" object that can cache its inverse.
## It contains a list for set, get, setinverse, and getinverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve
## - This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the matrix inverse is not cached, then calculate it and assign
        ## that to the setinverse property for x.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
