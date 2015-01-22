## writing 2 functions - makeCacheMatrix & cacheSolve that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## Initializes matrix m to save inv matrix
## Funtion get() to obtain raw matrix
## setInvMatrix() to assign computed inverse matrix (of x) to m
## getInvMatrix() to obtain the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(InvMatrix) m <<- InvMatrix
  getInvMatrix <- function() m
  list (set=set,get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if (!is.null(m)) {
    message ("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  message("Not Cached. Calculating...")
  m <- solve(data,...)
  x$setInvMatrix(m)
  m
}
