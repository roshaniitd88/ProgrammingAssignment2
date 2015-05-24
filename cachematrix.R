## This function creates a special "matrix" object that can cache its inverse
## input : x is the matrix ( please check the determinant of the matrix by using function - det(x))
## if det(x) = 0, solve(x) will give an error. 
## output : this function returns the list of 4 functions : getMatrix , setMatrix , setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  invmatrixVal <- NULL
  setMatrix <- function(m) {
    x <<- m
    invmatrixVal <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverseValue) invmatrixVal <<- inverseValue
  getInverse <- function() invmatrixVal
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## input : x is the matrix ( please check the determinant of the matrix by using function - det(x))
## if det(x) = 0, solve(x) will give an error. 
## output : Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inverseMatrixVal <- x$getInverse()
  if(!is.null(inverseMatrixVal)) {
    message("getting cached data...")
    return(inverseMatrixVal)
  }
  data <- x$getMatrix()
  inverseMatrixVal <- solve(data, ...) 
  x$setInverse(inverseMatrixVal)
  inverseMatrixVal      ## output
}


## to run : 
## assign a matrix into a variable say, mat
## mat <- matrix(1:4, nrow = 2, ncol = 2)
## value <- makeCacheMatrix(mat)
## cacheSolve(value)
## - returns the inverse of the matrix. 