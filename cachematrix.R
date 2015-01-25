

## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## We create two functions to maintain a special "matrix" object which also can store/cache its inverse.


# makeCacheMatrix, the first of the two functions creates the special matrix object. It has four components:
    #set the value of the matrix
    #get the value of the matrix
    #set the value of the inverse
    #get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(calcinv) inv <<- calcinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve does the main job of computing the inverse. It takes the special matrix object as an input. If an inverse component is already set, it is returned immediately. Else, it is computed and then returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("using cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}



## Execution instructions (for command line)
## source("cachematrix.R")
## x=matrix(rnorm(4),2,2)
## t<-makeCacheMatrix(x)
## i<-cacheSolve(t)
