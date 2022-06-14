## A pair of functions that cache the inverse of a matrix

## Function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize inverse property
        i <- NULL
        
        # Method to set, i.e. mutate, the matrix
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        # Method to get, i.e. access, the matrix 
        get <- function() x
        
        # Method to set inverse
        setinverse <- function(inverse) i <<- inverse
        
        # Method to get inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function computes the inverse of special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
