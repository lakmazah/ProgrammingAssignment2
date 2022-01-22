## Creates a list of cached elements of a matrix: get,set,getinverse,setinverse

makeCacheMatrix <- function(x = matrix()) {
  #creates a null variable for the inverse
  inverse <- NULL
  #defines the set function; assigns the argument to the cache copy of
  #the matrix itself
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #returns the cached matrix
  get <- function() x
  
  #returns the cached inverse
  getinverse <- function() inverse
  
  #sets the cached inverse
  setinverse <- function(inv) inverse <<- inv
  
  #returns the list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Solves the inverse matrix if it doesn't already exist
#returns the inverse if it already exists

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  #if the inverse is not NULL, return from the cache
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #otherwise, solve for the inverse
  #then cache the inverse
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}
