## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix produces a function object that caches a matrix inverse for fast retrieval

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve takes a makeCacheMatrix object, checks whether the matrix is cached, caches it if cached, and then returns the inverse

cacheSolve <- function(x, ...) {
  m <- x[[4]]() ## runs the getInv function to retrieve the inverse if present
  if(!is.null(m)){
    message("Getting cached data...")
    return(m)
  }
  
  data <- x[[2]]() ## runs the get function
  m <- solve(data, ...)
  x[[3]](m) ## runs the setInv function
  return(m)
}
