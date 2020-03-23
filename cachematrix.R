## Understanding lexical scoping via making a matrix and getting it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Initializing the objects
  m <- NULL
  set <- function(y) {
    ##This function takes an argument "y"
    ##This function will assign the imput to the x object in the parent environment.
    x <<- y
    ## "<<-" is used to assign a value to an object in a parent scope.
    ##While if we used "<-" then it will create a new object that is local to a function.
    m <<- NULL
  }
  get <- function() x ##This function retrieves x from the parent enviroment of makeCacheMatrix()
  setinverse <- function(solve) m <<- solve ##Defines the setter for the inverse m.
  getinverse <- function() m ##Getter for the inverse m.
  ##List is a new object which allows us to use the "$" operator to access the functions by name
  ##rather than using the [[ form of the extract operator.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is required to retrieve the inverse from an object of type makeCacheVector().
## It starts with a single argument, x, and an ellipsis that allows the caller to pass additional arguments into the function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()        ##First it calls getinverse()
  ##It checks to see whther the result is NULL.
  ##If the result of !is.null(m) is False, cacheSolve() gets the vector from the input object,
  ## calculates the inverse using solve().
  if(!is.null(m)){
    message("getting cached data/matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
