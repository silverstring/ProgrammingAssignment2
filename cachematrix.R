## makeCacheMatrix will take a matrix and cache the inverse of the matrix.
## cacheSolve converts the matrix that it gets from makeCacheMatrix

## makeCacheMatrix will take a matrix and cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  # establish inv as NULL
  inv = NULL
  
  set = function(y){
    # set x to be y outside the environment, and set inv to be NULL as well
    x <<- y
    inv <<- NULL
  }
  
  get = function() {
    # get will return x from the set function
    x
  }
  
  setinverse = function(inverse){
    # make inv return from inverse outside the environment
    inv <<- inverse
  }
  
  getinverse = function(){
    # return inv from the setinverse function
    inv
  }
  
  # this list is returned
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve converts the matrix that it gets from makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # inv uses getinv on x
  inv = x$getinv()
  
  # if the inverted matrix already exists, just use cache!
  if(!is.null(inv)){
    message("Getting Cached Data!")
    return(inv)
  }
  
  # use solve to invert matrix
  mat.data = x$get()
  inv = solve(mat.data,...)
  
  # use setinv on x
  x$setinv(inv)
  
  # return our inverted matrix
  return(inv)
}

## the flipper function executes the above and displays the result
flipper = function(){
  #my matrix is an invertible matrix that we will "flip"
  r = rnorm(1000000)
  mymatrix = matrix(r, nrow=2, ncol=2)
  
  print(mymatrix)
  
  # put matrix in make and use cacheSolve on it!
  make = makeCacheMatrix(mymatrix)
  cacheSolve(make)
}