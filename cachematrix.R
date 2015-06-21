# This function, named "makeCacheMatrix", creates a special "matrix" object that 
# can cache its inverse.

# This function is really a list containing a function to
# set the value of the matrix, get the value of the matrix
# set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
  invrs <- NULL 
  
  # set the value of the matrix
  set <- function(y) { 
    x <<- y 
    invrs <<- NULL 
  } 
  
  # get the value of the matrix
  get <- function() x 
  
  # set the vaule of the inverse
  setinverse <- function(inverse) invrs <<- inverse 
  
  # get the value of the inverse
  getinverse <- function() invrs
  
  # return a list of the above functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 



# The following function calculates the inverse of the special "matrix" created 
# with the above function. 
# It first checks to see if the inverse has already been calculated. If so, 
# it gets the inverse from the cache and skips the computation. Otherwise, 
# it calculates the inverse of the matrix and sets the value of the inverse in the
# cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  # check if inverse is already cached. If so, get the inverse from the cache.
  invrs <- x$getinverse() 
  if(!is.null(invrs)) { 
    message("getting cached data.") 
    return(invrs) 
  } 
  
  # else, get the matrix and calculate the inverse.
  data <- x$get() 
  invrs <- solve(data, ...) 
  
  # cache the inverse of the matrix and return the result.
  x$setinverse(invrs) 
  invrs 
} 


