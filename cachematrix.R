
## basically I took the example and converted it so that it is able to take a (square invertible) matrix and calculate its inverse

## the makeCacheMatrix function does exactly the same like the makeVector function within the example
## it creates an object consisting of the given matrix and the corresponding functions to get and set the given matrix and to set and get the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the cacheSolve function checks if the inverse matrix is already calculated, if it is it takes the cached data and returns it and if not it calculates the 
## inverse matrix, saves it within the matrix object and returns the inverse matrix
## utilising the setter and getter functions of the makeCacheMatrix object 
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
