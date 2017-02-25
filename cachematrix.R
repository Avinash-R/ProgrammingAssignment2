## There are two functions, makeCacheMatrix & cacheSolve
## 1st function defines the matrix. 2nd fetches/computes its inverse
## makeCacheMatrix initialises mat & invmat.

makeCacheMatrix <- function(mat = matrix()) {
  
  invmat <- NULL
  
  
  set <- function(newmat) {
    
    mat <<- newmat               
    invmat <<- NULL
    
  }
  
  get <- function() mat      
  setsolve <- function(solve) invmat <<- solve
  
  getsolve<- function() invmat     
  
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
  
}


## after initialization, it fetches or computes inverse
## the computed inverse is returned to makeCacheMatrix

cacheSolve <- function(mat, ...) {
  
  invmat <- mat$getsolve()
  
  
  if(!is.null(invmat)) {
    
    message("getting cached data")
    
    return(invmat)
    
  }
  
  
  tempmat <- mat$get()
  
  invmat <- solve(tempmat, ...)
  
  mat$setsolve(invmat)
  
  invmat
}
