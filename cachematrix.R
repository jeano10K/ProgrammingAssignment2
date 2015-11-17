## makeMatrix resets the input values that were last used by cacheSolve to calculate a matrix 
## stores the fuctions used by cacheMatrix to get new and set input values  
makeCacheMatrix <- function(x = numeric(), ...) {
  minv <- NULL                                ## sets the matrix inverse value to NULL    
  set <- function(y) {                        ## resets the cached matrix input values and the inverse value
    x <<- y
    a1 <- NULL
    a2 <<- NULL
    minv <<- NULL                             
  }
  getx <- function() x
  setminv <- function(solve) minv <<- solve   ## function to set minv to the inverse result
  getminv <- function() minv                  ## function to get the set value of minv for evaluation in the cacheMatrix function 
    list(set = set, getx = getx,              ## creates a list of the funtions to be used in cacheMatrix
    setminv = setminv,
    getminv = getminv) 
}


## cacheSolve checks whether the inverse for the inputed matrix already exists
## If it does, it returns the cached value and stops
## otherwise it calls makeMatrix, calculates the matrix inverse, caches the resulting inverse 

cacheSolve <- function(x, nrow = 2, ncol = 2) {
  mCache <- x$getminv()               ## sets the minv variable to the inputs last used to create an inverse matrix
  if(!is.null(mCache)) {              ## evaluates whether the inverse of the matrix has already been calculated, if so returns the stored value
    print("getting cached data")
    return(mCache)
  }
  a1 <- nrow                       ## if the matrix inverse hasn't been calculated, gets the inputs for creating the new matrix
  a2 <- ncol  
  minv <- solve(matrix(x$getx(), nrow = a1, ncol = a2))         ## inverses the matrix
  x$setminv(minv)                  ## sets the inverse value for the given matrix input 
  minv                             ## shows the  value of the matrix inversion
}
