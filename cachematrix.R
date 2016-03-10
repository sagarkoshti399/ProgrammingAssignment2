## Function to return the List of Matrix

makeCacheMatrix <- function(x = matrix()) {
	##### Set inverse to NULL
	cachInv <- NULL 
  
	##### Set input value to z
	set <- function(vInput = matrix()) {
		z <<- vInput 
		##### Set inverse to NULL if set already
		cachInv <<- NULL
	}
	
	##### get
	get <- function() z
  
	##### Set inverse variable 
	setinverse <- function(inverseVal) {
		cachInv <<- inverseVal 
		##### Return the inverse
		return(cachInv)
	}
  
	##### getinverse
	getinverse  <- function() cachInv
	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Function to calculate Inverse of Matrix using solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	##### Check calculated inverse
	calculatedInv <- x$getinverse() 
  
	##### Check for matrix and values
	if(!is.null(calculatedInv) && is.matrix(calculatedInv)) { 
		message("Cached data found:")
		return(calculatedInv)
	}
  
	##### Get the matrix
	matrixToSolve <- x$get()  
  
	##### Solve function
	calculatedInv <- solve(matrixToSolve)
  
	##### Set inverse
	message("Matrix Inverse:  ") 
	x$setinverse(calculatedInv)
}
