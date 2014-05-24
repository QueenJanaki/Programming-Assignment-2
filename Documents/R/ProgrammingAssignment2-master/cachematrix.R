## makeCacheMatrix and cacheSolve form a pair of functions that cache 
## the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. The "matrix" object is a list containing
## a function to
## 1. set the value of the "matrix" object
## 2. get the value of the "matrix" object
## 3. set the value of its inverse
## 4. get the value of its inverse
## The <<- (= superassignment) operator below causes a search to be 
## made through parent environments for an existing definition of the 
## variable "s" being assigned. If the variable "s" is found (and its 
## binding is not locked) then its value is redefined, otherwise the
## assignment takes place in the global environment.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## object returned by the makeCacheMatrix function above. In R, this is 
## done with the help of the solve function. If x is a square invertible 
## "matrix", then solve(x) returns its inverse. 
## If the inverse has already been calculated (and the "matrix" has not 
## changed), then the cacheSolve function should retrieve the inverse 
## from the cache and skip the computation. Otherwise, it should calculate 
## the inverse of the data via the solve function and set the value of the 
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
