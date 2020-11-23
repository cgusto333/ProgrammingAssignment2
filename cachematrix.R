  makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL
    set <- function(y) {
      x <<- y
      Inverse <<- NULL
    }
    get <- function() {x}
    setInvert <- function(Invert) (Inverse <<- Invert)
    getInvert <- function() {Inverse}
    list(set = set, get = get,setInvert = setInvert,getInvert = getInvert)
  }


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInvert()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  matrix <- x$get()
  Inverse <- solve(matrix, ...)
  x$setInvert(Inverse)
  Inverse
}
