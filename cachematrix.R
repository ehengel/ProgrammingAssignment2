# This scripts caches the inverse of matrices. Matrix inversion can be heavy
# operations for the computer so this script can save time reusing cached data.

# Example usage:
#       m <- matrix(c(-1, -2, 1, 1), 2,2)  -- create a matrix
#       x <- makeCacheMatrix(m)            -- setup cache matrix
#       inv <- cacheSolve(x)               -- solve matrix
#       inv <- cacheSolve(x)               -- 2:nd time cached data is used

# makeCacheMatrix creates a list of functions that can be applied on the matrix
#       set - sets the value of the matrix
#       get - gets the value of the matrix
#       setinverse - sets the inverse value of the matrix
#       getinverse - gets the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #sets the inverse of the matrix
        setinverse <- function(solve) m <<- solve
        
        #gets the inverse of the matrix
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        #gets inverse from cache if available
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #gets matrix
        data <- x$get()
        
        #solves the matrix
        m <- solve(data, ...)
        
        #sets the inverse of matrix 'x' and returns inverse
        x$setinverse(m)
        m
}
