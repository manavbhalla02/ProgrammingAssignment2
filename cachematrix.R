## The below function uses the concept of lexical scoping which is cappable to
## cache time consuming computation.

## This function makeCacheMatrix creates a special matrix which is a list which create function to
## Set value of matrix
## get value of matrix
## set value of inverse 
## get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  q<-NULL
  set<-function(y){
    x<<-y
    q<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)q<<-solve
  getinverse<-function()q
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function calculates the inverse of the special matrix create with above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  q<-x$getinverse()
  if(!is.null(q)){
    message("getting cached data")
    return(q)
  }
  dataofinverse<-x$get()
  q<-solve(dataofinverse,...)
  x$set(q)
  q
}



##Testing Function

##Inverse using solve()
test<-matrix(2:5,2,2)

print(solve(test))


##Inverse using above functions
test2<-makeCacheMatrix(test)


print(cacheSolve(test2))