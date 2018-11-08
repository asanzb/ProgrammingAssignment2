## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores the data for a given matrix (passed as an argument) and its inverse
## (obtained through one of the functions defined within makeCacheMatrix). It returns a list
## of functions to set and get values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL ## initializing the inverse matrix
        
        set <- function(y) { ## resets the matrix and its inverse
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x ## gets the matrix
        
        set_inv <- function (i) inv <<- i ## sets the value for the inverse matrix
        
        get_inv <- function() inv ## gets the value of the inverse matrix
        
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv) ## return list of functions
        
}

## cacheSolve returns the inverse of a matrix that is within the argument passed (object of
## type makeCacheMatrix). If the inverse was already calculated, the function gets the value from
## the object, else, it calls the object's function to calculate the inverse and update the
## object to store it through another object's function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv() ## getting data for the inverse matrix
        if (!is.null(inv)) { ## if the inverse was already calculated, return it
                message('getting the inverse data from the cache')                        
                return(inv)
        } 
        
        ## if we reach this point is because the inverse has not been calculated yet
        data <- x$get() ## get the matrix
        inv <- solve(data) ## calculate the inverse
        x$set_inv(inv) ## update the x object to the value of the inverse, just calculated
        return(inv)
}