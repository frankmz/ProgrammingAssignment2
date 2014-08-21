## Functions to create a matrix data structure that is able to cache its inverse 
## and to calculate and cache the inverse of a matrix

## Function to create a matrix data structure that is able to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        # data structure for storing the cached inverse
        inv <- NULL

        # definition of the set function to assign a new value to the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # definition of the get function to retrieve the matrix 
        get <- function() x

        # caching functions to store and retrieve the cached inverse of the 
        # matrix
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv

        # return list of functions on matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function to calculate and cache the inverse of a given matrix; 
## if result had been cached already, return cached value

cacheSolve <- function(x, ...) {
        
        # check if result is cached; if yes, return cached result
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # calculate inverse and cache the result
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        # return result
        inv
}
