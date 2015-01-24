## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##    set the value of the matrix
##    get the value of the matrix
##   set the inverse of the matrix
##   get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been gotten. 
##    If so, it gets the inverse from the cache and skips the computation. 
##    Otherwise, it gets the inverse of the matrix and sets it in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
