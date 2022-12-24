## Put comments here that give an overall description of what your
## functions do

## This function stores old data for inverse and pass new matrix to calculate inverse

makeCacheMatrix <- function(x = matrix()) {
  minv=NULL
  set=function(y)
  {
    x<<-y
    minv<<-NULL
  }
  get=function()x
  setinv=function(inv) minv<<-inv
  getinv=function() minv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function checks if matrix is same then do not calculate inverse and then it calculates inverse for new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv=x$getinv()
  if(!is.null(minv))
  {
    message("getting chached data")
    return(minv)
  }
  data=x$get()
  minv=solve(data)
  x$setinv(minv)
  minv
}
