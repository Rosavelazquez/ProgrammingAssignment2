## These functions create a special "matrix" that stores a matrix 
## and caches its inverse

## The first function creates the special "matrix" that sets and gets the value 
#of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This second function calculates the inverse of the special "matrix" we just
# created. It first checks if the inverse is already calculated and gets the 
# mean from the cache. If not it calculates the inverse of the data and sets
# it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() 
  if(!is.null(m)) { 
        message("getting cached data")
        return(m)
  }
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m     
}
