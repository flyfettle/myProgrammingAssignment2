## These 2 functions attempt to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
    
    r<-NULL
    set<-function(y){
      x<<-y
      r<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) r<<- solve
    getmatrix<-function() r
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
  }
  
  ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x=matrix(), ...) {
    r<-x$getmatrix()
    if(!is.null(r)){
      message("retreiving cached data")
      return(r)
    }
    matrix<-x$get()
    r<-solve(matrix, ...)
    x$setmatrix(r)
    r
  }
  

list