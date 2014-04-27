## The following function creates a special matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize inv
  i <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # to set the inverse
  
  setInverse <- function(inverse) i <<- inverse
  
  # to get the inverse
  
  getInverse <- function() i
  
  # return a list of all the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special
## matrix created above. It first checks to see if the inverse 
## already exists.
## If so, it gets the inverse from the cache and if not, it 
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
 
 # check if the inverse exists
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # set matrix in the data
  data <- x$get()

  # compute the inverse
  inv <- solve(data, ...)

  # cache the inverse
  x$setInverse(inv)
  
  inv
}

