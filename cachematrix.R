#  The following two functions, makeCacheMatrix and cacheSolve cache the inverse
#  of a matrix.


## makeCacheMatrix first generates a matrix and then calculates the inverse of 
## the matrix if the inverse has already been calculated. If not, the function
## will simply return it in the cache.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix function above. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setmean function.
it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv          ### Return a matrix that is the inverse of 'x'
}

## Test:
### m <- makeCacheMatrix(cbind(c(1,2),c(3,4)))
### m$get()
### cacheSolve(m)