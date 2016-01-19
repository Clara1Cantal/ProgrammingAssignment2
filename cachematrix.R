## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix function establishes a matrix and sets the value of it in 
#   the cache.It provides methods to store and retrieve the original value as
#   well as store and retrieve the inverse. These values are cached to an
#   environment.
# The cacheSolve function retrieves the inverse from the cache if it was already 
#   calculated or calculates the inverse matrix from x and stores in the cache
#   provided by makeCacheMatrix.

## Write a short comment describing this function

# We take a matrix input = x.
# We establish functions to set and retrieve the value x.
# We establish functions to store the inverse of x and to retrieve the value.
# We return a list of set and get functions for the original value and the 
#    inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
      
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# We take a list input = x.
# We get the inverse value of x from the cache and store it in m.
# If m is different from null, we return the matrix previously calculated from 
#   the cache (function stops).
# Else, we get x and store it into data.
# We invert the matrix data and store it into m.
# We store the inverse so this object can be retrieved from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
