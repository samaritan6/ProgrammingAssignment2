## Aim is to write a pair of functions that cache the inverse of a matrix
## I have set x as a special matrix object, then set the value j as Null
## then changed every reference to "cache to Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  j <-NULL
  set <-function(y){
    x<<-y
    J <<-NULL
  }

  get <-function()x
  setInverse <-function(Inverse)j <<-inverse
  getInverse <-function()j
  list(set=set,get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cachsolve function computes the inversal of the special matrix
## returned by mackeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachsolve should retrive the inverse 
## from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    
    return(j)
  }
  mat <-x$get()
  j <-solve(mat,...)
  x$setInverse (j)
}
