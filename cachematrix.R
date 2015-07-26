makeCacheMatrix <- function(x = matrix()){
  InverseX <- NULL #empty variable for the inversion.
  set <- function(y){
    x <<- y
    InverseX <<- NULL
  }
  get <- function() x #pulls the input matrix.
  setTHEx <- function(solve) InverseX <<- solve #sets the inverse matrix.
  getTHEx <- function() InverseX #pulls the inverse matrix.
  list(set = set, get = get,
       setTHEx = setTHEx,
       getTHEx = getTHEx)
}
cacheSolve <- function(x, ...) {
  InverseX <- x$getTHEx() #pulls value from cache if not empty; skips SolveInverse.
  if(!is.null(InverseX)){
    message("getting cached data")
    return(InverseX) #displays the value in the cache.
  }
  SolveInverse <- x$get()
  InverseX <- solve(SolveInverse, ...) #calculates the inverse if the cache was empty.
  x$setTHEx(InverseX) #sets the value in the cache.
  return(InverseX) #displays the value calculated.
}