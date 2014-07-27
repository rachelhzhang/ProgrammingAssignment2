## This function creates a special "matrix" object that can cache the inverse of matrix 'x'
## 1. Set value of matrix 'x'
## 2. Get value of matrix 'x'
## 3. Set value of inverse matrix 'inverse'
## 4. Get value of inverse matrix 'inverse'

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache
## Otherwise, compute the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(x)
    x$setinverse(inverse)
    inverse
}
