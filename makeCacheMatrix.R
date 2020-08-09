##  it is used for Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation  
## it is used to catch inverse rather than doing samething repeatedly 
##  it stores a matrix
## This function take inverse of a matrix 
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<-NULL
  }
  get <- function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve<-function(x,...){
  ##  it returns  a matrix  inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
