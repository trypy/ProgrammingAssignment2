## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## , then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
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

# Test case for unchanged matrix
# Message "getting cached data" should print when 
# cacheSolve called second time
mTestSpecialMatrix <- makeCacheMatrix(cbind(c(1,2,3),c(2,1,4),c(3,4,1)))
cacheSolve(mTestSpecialMatrix)
cacheSolve(mTestSpecialMatrix)

# Test case for matrix changed
mTestSpecialMatrix <- makeCacheMatrix(cbind(c(1,2,3),c(2,1,4),c(3,4,1)))
cacheSolve(mTestSpecialMatrix)
mTestSpecialMatrix$set(cbind(c(1,2,3),c(2,1,5),c(3,5,1)))
cacheSolve(mTestSpecialMatrix)
