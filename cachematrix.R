## makeCachematrix stores multiple functions that can:
## a) set: receives and stores the matrix
## b) get: returns the original matrix
## c) setinverse: stores the inverse of the original matrix
## d) getinverse: returns the inverse of the original matrix

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


## Cache solve:
## 1) m is defined by the value store in x$getinverse (give x is your input matrix)
## 2) if the inverse is already defined/stored we will see "getting cached data"
## 3) the original matrix will be stored as 'data' variable
## 4) m will then stored as an inverse of the original
## 5) the function will pass through the makeCachematrix function setinverse to store the matrix calculate in step 4
## 6) the inverse of the original matrix will be shown
## note: if m is null, only 'getting cached data' message will show because there is noting assigned m

cacheSolve <- function(x=matrix(), ...) {
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
