#function with default matrix 
#initialize inver as NULL to store value of matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {       #to assign new matrix
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert      # returns value of the matrix argument
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getInverse()
  if (!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  mat <- x$get()
  invert <- solve(mat, ...)
  x$setInverse(invert)
  invert
}
source("ProgrammingAssignment2/cachematrix.R")
my_matrix <- makeCacheMatrix(matrix(c(1,3,5,7,9,0,8,6,4), 3, 3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix) 
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

