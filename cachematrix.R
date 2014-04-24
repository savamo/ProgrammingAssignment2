## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## example of usage:
## > mmm <- replicate(20, rnorm(20))
## > cm <- makeCacheMatrix(mmm)
## > xx <- cacheSolve(cm) # getting NOT cached data
## > xx <- cacheSolve(cm) # getting cached data


makeCacheMatrix <- function(x = matrix()) {

  # random matrix: replicate(20, rnorm(20))
   
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve  <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
  
}
