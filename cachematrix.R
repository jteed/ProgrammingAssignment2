## jteed 
## R programming class at Coursera
## Assignment 2
## written: 05/27/2014

## Since matrix inversion is computationally intensive,
## the makeCacheMatrix() function enables creation of a "special" 
## matrix data type which caches the inverse when the inverse is
## calculated by the cacheSolve() function.

## makeCachMatrix -
## given a matrix, x, which is square and invertible, this function
## creates a new matrix which can store the inverse, computed by the
## other function in this file.
makeCacheMatrix <- function(x = matrix()) 
{
    cm <- NULL
    set <- function (y)
    {
        x <<- y
        cm <- NULL
    }
    get <- function() x
    setcache <- function(solve) cm <<- solve
    getcache <-function() cm
    
    list(set=set, get=get, setcache=setcache, getcache=getcache )
}


## cacheSolve works on a special matrix, x, as created by makeCacheMatrix() above.
## purpose:  computes the inverse of x, and caches it for later use.
## assumptions:  x is a square and invertable matrix
cacheSolve <- function(x, ...) 
{
    cm <- x$getcache()
    if(!is.null(cm))
    {
        message("getting cached matrix")
        return(cm)
    }
    data <- x$get()
    cm <- solve(data, ...)
    x$setcache(cm)
    cm
        ## Return a matrix that is the inverse of 'x'
}


## The code was tested with the following:
##      m <- matrix(c(4, 3, 3, 2))
##      sm <- makeCacheMatrix(m)
##      cm <- cacheSolve(sm)
##      cm
##  which displays the inverse of the original matrix:
##       [,1] [,2]
##  [1,]   -2    3
##  [2,]    3   -4
##      
##      cm <- cacheSolve(sm)  # Repeating the solve
##  Results in message:  getting cached matrix
##      cm
##  Displays the matrix