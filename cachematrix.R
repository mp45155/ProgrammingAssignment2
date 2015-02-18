## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## The "special matrix" object encapsulates 2 objects: 
##            - the actual matrix object
##            - the inverse object (matrix which is the inverse matrix of the actual matrix object)
makeCacheMatrix <- function( x = matrix() ) 
{  
   i <- NULL                 #initially, upon calling the creation function, inverse is null
   
   set <- function(y)        # "set" called upon "special matrix" sets its matrix object, and initializes its inverse object to null
		  {
			x <<- y
			i <<- NULL       # new "special matrix" object will not yet have its inverse object defined
		  }
		  
	get <- function() x      # "get" called upon "special matrix" returns its actual matrix object
		
	setinverse <- function(solve) i <<- solve   # "setinverse" called upon "special matrix" defines its inverse object
	getinverse <- function() i        			# "getinverse" called upon "special matrix" returns its inverse object
		
	list( set 	  	 = set, 
		  get 	  	 = get,
		  setinverse = setinverse,
		  getinverse = getinverse   )

}

## Function "cacheSolve" returns a matrix that is the inverse of actual matrix stored in "special matrix" 'x'
##	First it fetches cached data (cached inverse). 
##   if something is found there, function returns it
##   if nothing was found there, function calculates (solves) the inverse, caches it, and returns it.
cacheSolve <- function(x, ...) 
{   ## This function computes the inverse of the "special matrix" returned by makeCacheMatrix above. 	 
	 # Assignment's assumption: x is an invertible matrix; that's why we won't check it

	 i <- x$getinverse()        # variable i fetches what's stored as the inverse 

	 if (!is.null(i))			# if something is stored, this is happily finished...
	 {
		message("getting cached data")
        return(i)
	 }
								# ...or otherwise, no inverse was cached, we have work to do:
	 matrixData <- x$get()        # fetch the actual matrix data from the "special matrix"
	 i <- solve(matrixData)       # calculate the inverse
	 x$setinverse(i)              # cache the calculated inverse
	 i							  # return the inverse
}