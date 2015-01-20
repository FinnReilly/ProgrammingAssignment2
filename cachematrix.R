##example use:
##test<-makeCacheMatrix()
##test$set(replicate(2,rnorm(2)))     -This is a nice way to create a random matrix.
##cacheSolve(test)                    -This will calculate the inverse - run twice to 
##                                     see different behaviour when already cached.

## makeCacheMatrix
## Works almost like a Class in C# with its own methods, must be constructed before it can be used
## caches the input matrix with set, all methods relate to setting or retrieval of data only

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(data) inv<<-(data)
  getinv<-function() inv
  list(                                
    set=set,                           
    get=get,
    setinv=setinv,
    getinv=getinv
  )
}

#cacheSolve
#takes an 'object instance' of the above function and accesses its methods, as in OOP
#returns an inverse of the previously cached matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Already in cache!")
    return(inv)
  }
  message("Calculating...")  #included this message so you can tell whether cached 
  input<-x$get()             #inverted matrix has been calculated yet
  
  x$setinv(solve(input))
  x$getinv()
}
