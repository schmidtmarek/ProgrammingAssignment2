## Author: Marek Schmidt
## Date: 2014-09-21
## Example of special "matrix" object with persistent data
## holding "matrix" and its "inversion matrix" (if calculated)

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # initialization of "inversion matrix" to NULL
        inv_m <- NULL
        
        # setter function to store data value for "matrix"
        # and "reset" its "inversion matrix" value to NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        
        # getter to retrieve "matrix" itself 
        get <- function() {
                x
        }
        
        # setter to store "inversion matrix"
        set_inv <- function(inverted_m) {
                inv_m <<- inverted_m
        }
        
        # getter to retrieve "inversion matrix"
        get_inv <- function()   {
                inv_m
        }

        # list of available functions 
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## Computes inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        # Return a matrix that is the inverse of 'x'
        # cached from storage in 'x' object 
        inv_m <- x$get_inv()
        
        # when not NULL value is present return it without calculation 
        if(!is.null(inv_m)) {
                message("getting cached inversion matrix")
                return(inv_m)
        }
        
        # otherwise get "matrix" value from 'x' object        
        reg_m <- x$get()
        
        # and calculate its "inversion matrix"
        inv_m <- solve(reg_m, ...)
        
        # and store its value in 'x' object for later retrieval 
        x$set_inv(inv_m)
        
        # return newly calculated "inversion matrix
        inv_m
}

