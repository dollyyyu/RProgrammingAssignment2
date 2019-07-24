## Here we have two functions. The first function named makeCacheMatrix is 
##designed to cache the inverse of the given matrix. This function includes 
##both setters and getters to set/get the inverse of matrix. The second 
##function named cacheSolve is designed to get the result, i.e. the inverse 
##of given matrix. It will first check if the variable m has been assigned 
##the value of mean, and if not, we call the solve() function with data 
##(inverse of matrix) to calculate the inverse of matrix. 

## This function, as described above, is to set and get the 'special' matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function, as described above, is to return a matrix that is the inverse 
##of 'x'. If it's already calculated, we give a message saying that we cached the
##inverse of given matrix; if not, then we calculate the inverse of given matrix
## and return it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("cached inverse of given matrix.")
    return (m)
  }
  ##calculate the inverse of 'x'
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

