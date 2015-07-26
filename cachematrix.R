## The pair of functions below are used to create a special object
## that stores a matrix and cache's its inverse. 

## makeCacheMatrix creates a special "matrix" object that can cache 
## the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # sets the value of i to NULL 
        set <- function(y) { 
                x <<- y # changes the matrix stored in the main function
                i <<- NULL # restores to NULL the value of the inverse
        }
        get <- function() x # returns the matrix stored in the main function
        setinverse <- function(inverse) i <<- inverse
        # stores the value of the input (inverse) in a variable i 
        getinverse<- function() i # returns the value i
        list(set = set, get = get, # creates a list to store the 4 functions
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # verifies if an inverse has already been calculated
        if(!is.null(i)) { # verifies if the value is not NULL
                message("getting cached data") 
                # returned a message "getting cached data"
                return(i) # returns the value of i 
        }
        data <- x$get() 
        # if an iverse has not already been calculated, gets the value of the input matrix
        i <- solve(data, ...) # computes the inverse of the matrix
        x$setinverse(i) # sets the value of the inverse in the cache
        i # returns the inverse
}
        

