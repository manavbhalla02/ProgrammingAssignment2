## The below function uses the concept of lexical scoping which is cappable to
## cache time consuming computation.

## This function makeCacheMatrix creates a special matrix which is a list which create function to
## Set value of matrix
## get value of matrix
## set value of inverse 
## get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)m<<-solve
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function calculates the inverse of the special matrix create with above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$set(m)
  m
}
