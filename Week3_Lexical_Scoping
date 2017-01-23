## I have created two functions. makeCacheMatrix has all the getters
##and setters for initializing the matrix and its inverse values. The
##cachesolve matrix computes the inverse and access makeCachematrix
## parameters and functions through an object. 

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## This function gets an object of makeCachematrix as an input. 
##Through that object, it accesses all the variables and functions
##of that environment. This is how caching is made possible. 

cacheSolve <- function(x) {
        
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
