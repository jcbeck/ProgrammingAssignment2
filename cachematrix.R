## Below are a pair of functions that will be used to create a special 
## obj that stores a matrix and caches its inverse.

## Function creates special "matrix" obj which will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function computes inverse of special "matrix" created by makeCacheMatrix
## in previous exercise. If inverse has already been calculated (matrix needs
## to remain unchanged), then inverse from cache should appear.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv)
  return(inv)
  
}

test = function(mat) {
  
    temp = makeCacheMatrix(mat)
  
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
 
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
}

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
