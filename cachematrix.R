
# Function makeCacheMatrix creates matrix that can cache its inverse. It is consisted of set,get,setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initializing inverse as null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x # function to get matrix x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv # function to obtain inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function cacheSolve  computes the inverse of the matrix object returned by makeCacheMatrix function previusly defined.
#If the inverse has already been calculated and there is no changes in the matrix, then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){ # checking is it inverse null
    message("getting cached data")
    return(inv)      # returns inverse value 
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      # it returns a matrix that is a inverse of x
}












