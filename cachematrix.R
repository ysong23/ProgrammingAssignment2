## cache the inverse of a matrix

## create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                set<-function(y) {
                   x<<-y
                   m<<-NULL
                }
                get<-function() x
                setinverse<-function(solve) m<<-solve
                getinverse<-function() m
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## calculate the inverse of special matrix
## first check if inverse has already been calculated
## if yes, get the inverse from cache; if not, calculate the inverse of special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m<-x$getinverse()
          if(!is.null(m)) {
              message("getting cache data")
              return(m)
          }
          data<-x$get()
          m<-solve(data, ...)
          x$setinverse(m)
          m
}



