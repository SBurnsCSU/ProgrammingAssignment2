## makeCacheMatrix creates a matrix of your definition by following a series of functions in a list as follows:
##  1)set the value of a vector
##  2)get the value of a vector
##  3)set the value of a matrix
##  4)get the value of a matrix



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m<<-solve
        getmatrix <- function() m
        list (set =set, get = get,
              setmatrix=setmatrix,
              getmatrix=getmatrix)
}


## cachSolve calculates and returns a matrix that is the inverse of the matrix set in makeCacheMatrix; however
##      it first checks to see if the inverse matrix has already been calculated. If so, it returns that value,
##      otherwise, it calculates the new matrix and returns it to you!

cacheSolve <- function(x=matrix(), ...) {
       m<-x$getmatrix()
      if(!is.null(m)){
           message("getting cached data")
           return(m)
       }
       matrix<-x$get ()
       m<-solve(matrix, ...)
       x$setmatrix(m)
       m
}    

