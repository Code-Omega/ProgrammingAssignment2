## This pair of functions create an object to stores a matrix,
## calculate its inverse, and cache it.

## "makeCacheMatrix" creates a list containing a function to
## 1. "set" -- set the value of the matrix
## 2. "get" -- get the value of the matrix
## 3. "setinverse" -- set the value of the inverse matrix
## 4. "getinverse" -- get the value of the inverse matrix

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


## "cacheSolve" calculates the inverse of the matrix created above. 
## It first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
