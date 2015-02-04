## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##    set the value of the matrix
##    get the value of the matrix
##   set the inverse of the matrix
##   get the inverse of the matrix

akeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) { #sets matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x #gets matrix from cache
  setsolve <- function(solve) i <<- solve #this function obtains the inverted matrix
  getsolve <- function() i #gets the inverted matrix from cache
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
  i <- x$getsolve() #assigns the inverted matrix from the cache to i
  if(!is.null(i)) { #checks if the inverted matrix existed
    message("getting cached data")
    return(i) #returns the matrix
  }
  data <- x$get() #gets the original matrix from the cache
  i <- solve(data, ...) #sets the inverted matrix to i
  x$setsolve(i) #sets the value of the inverted matrix
  i #returns the inverted matrix
}