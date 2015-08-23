## The first function "makeCacheMatrix" is used to make a cache matrix of inverse matrix of 'x'
## The second function "cacheSolve" is used to solve inverse matrix of 'x' via cache from the 
## first function "makeCacheMatrix". If the matrix is not solved yet i.e not cache-d, 
## the function "cacheSolve" will compute the inverse matrix of 'x' and return the new value

## This function is used to make a cache matrix of inverse matrix of 'x'.
## This function consists of four sub functions - set, get, setinv, getinv
## set function is used to set the matrix 'x'
## get function is used to call the matrix 'x'
## setinv function is used to set the inverse matrix of 'x'
## getinv function is used to call the inverse matrix of 'x'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function is used to get the inverse matrix of 'x' from function "makeCacheMatrix" above
## If there is no calculation has been made, this function will calculate the inverse matrix of 'x'
## and return the value of inverse matrix of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
