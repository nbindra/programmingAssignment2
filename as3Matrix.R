
## Make function that stores 4 functions
## set - sets the value of the input vector
## get - gets the value of the input vector
## setMatrix - sets Inv of Matrix
## getMatrix - get Inv Matrix for cache
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse matrix
  im <- NULL
  
  ## stores x in different env.
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## gets the value of x
  get <- function() x
  
  ## sets value of inv matrix in differrent env
  setMatrix <- function(solve) im <<- solve
  
  ## gets the value of the inverse env
  getMatrix <- function() im
  
  ## list of functions the makeCacheMatrix function stores
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## Cache function that retrieves and stores data to/from cache 
## this uses above function subfunctions to do this.
##
cacheSolve <- function(x, ...) {
  ## try to get the inverse matrix from cache.
  im <- x$getMatrix()
  
  ## if found in cache, return the matrix from cache
  if(!is.null(im)) {
    message("getting cached Matrix data")
    return(im)
  }
  
  ## matrix not in cache. Get the in input vector
  data <- x$get()
  
  ## compute inverse matrix
  im <- solve(data, ...)
  
  ## store inverse matrix in case for next retrival
  x$setMatrix(im)
  
  ## print inverse matrix
  im
}
