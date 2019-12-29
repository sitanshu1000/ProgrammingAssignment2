## There are 2 functions here, makeCacheMatrix() and cacheSolve()
## The goal of this assignment is to display lexical scoping along with behaviour of functions in the Environment

## The makeCacheMatrix() function takes input from the user and creates a special data type of the type 'makeCacheMatrix'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The cacheSolve() function checks if the inverse is asked for same matrix as is stored in the cache
## If it is the same matrix, inverse calculation is skipped and the previously stored inverse is displayed along with a message
## If the matrix is different, the inverse is calculated and stored in the makeCacheMatrix format

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## You can use the below methods to test out my work. 
## Please feel free to use your own statements too. However, please check if matrix inputted is invertible
test <- makeCacheMatrix(rbind(c(1, 2), c(3, 4)))
test$get()
cacheSolve(test)
test$set(rbind(c(61, 32, 93), c(23, 5, 43), c(21, 89, 83)))
cacheSolve(test)
