## by PAOLO MASOTTI

## function "cashSolve" does the inverse of matrix of argument x; if the inverse matrix has been already calculate
## it avoid to recalculate it and caches the inverse from the memory

## function "makeCacheMatrix" creates the methods mutator (Set/setInv) and accessor (get/getInv)
## of the objects passed as argument; note that the function produces as output the list of the method mutator/accessor
## that are called within CashSolve and the pointer of memory where values are store, making an S3 type object
## IMPORTANT: TO TEST THE CODE CREATE A INVERTIBLE MATRIX (MUST BE SQUARE AND WITH DETERMINANT <> 0); THEN ASSIGN
## TO A VARIABLE XXX <- makeCacheMatrix(INVERTIBLE MATRIX); THEN CALCULATE THE INVERSE AS cacheSolve (XXX)

makeCacheMatrix <- function(x = matrix()) {
  # create the methods mutator/accessor; the output is a list of functions that can be used as methods
  # to manipulate le matrix x - the output store also the x values as input
  # this object is S3 type, thus it creates  4 methods and
  # it also store the x=matrix that contains the matix with values and the alcready calculated matInv

  # the main body do objects initialization for k (martrix as function argument) and matInv assigned to NULL
  # x must be inizitializated as matrix in argument to avoide x$get return an error
  matInv<-NULL #initialize The inverse of matrix to NULL
  
  # method set can be used to assign a new value (y) to the x in the parent envinroment; in this case matInv is set to NULL    
  set <- function (y) { #set the value of  the matrix
    x<<-y # "<<-" assign assign a value to an object in the parent environment - the x of parent enviroment 
    matInv<<-NULL
  }
  
  # method get is used to retrieve the value of x; since is not the argument is a parent environment variable
  get <- function() {
    x #get the value of the matrix - remind that this is a S3 object, the memory pointer is passed, and the data in x are accessible 
  }
  
  # method setInv setInv assign to matInv(globEnv) the argument solve  
  setInv <- function(solve) {
    matInv <<- solve 
  }  
  
  #method get the value of the inverse from parent enviroment- variable matInv
  getInv <- function() matInv  
  
  #assigns each of these methods (set,get,setInv,getInv) as an element within a list(), and returns it to the parent environment.
  # each element of the list (leftside) is assigned with the name of the method (right side)
  # the object makeCacheMatrix() can be used in the cachesolve and is provided with the four methods 
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}

# The following function calculates the mean of the special "matrix" created with the makeCacheMatrix() function. 
# it first checks to see if the Inverse has already been calculated. 
# If so, it gets the Inverse from the cache and skips the computation. 
# Otherwise, it calculates the Inverse of the data and sets the value of the Inverse
# in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInv() # getInv the inverse of x; S3 object make value in othe fuction accessible
  if(!is.null(matInv)) { #is mAtInv exist the is passed as funtion output 
    message("getting cached data")
    return(matInv) #exit the function
  }
  # else..
  data <- x$get()  #S3 object: recover the data passed in the makeCacheMatrix function
  matInv <- solve(data, ...) #calculate the inverse
  x$setInv(matInv) #set the inverse in makeCacheMatrix
  matInv #produce the output
}

#===============END==================================
#=====================================================

