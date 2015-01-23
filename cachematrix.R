# Assignment
# -----------------------------------------------------------------------------------------------
#
# Matrix inversion is usually a costly computation and their may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
# matrix inversion that we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.
#
# Write the following functions:
#  
# - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# - cacheSolve: This function computes the inverse of the special "matrix" returned by 
#               makeCacheMatrix above. If the inverse has already been calculated (and the matrix
#               has not changed), then the cachesolve should retrieve the inverse from the cache.
#
# -----------------------------------------------------------------------------------------------

# Comments
# - This assignment would have been much clearer if the assignment description would 
#       have come out and admitted this is an exercise in OOP in R, as hideous as the syntax may be.

# 'makeCacheMatrix' function
# --------------------------
# Accepts an invertible matrix as an input; returns an list of functions as a cached matrix object

makeCacheMatrix <- function(x = matrix()) {
  # Add a field to the object to store the inverted matrix
  inverted.x <- NULL;
  
  # Add getter/setter methods for the class fields
  set <- function(my.matrix) {x <<- my.matrix; inverted.x <<- NULL}
  get <- function() x;
  set.inverse <- function(my.inverse) inverted.x <<- my.inverse;
  get.inverse <- function() inverted.x;
  
  # return the list of methods
  list(set = set,
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse);
}


# 'cacheSolve' function
# --------------------------
# Accepts a list of functions (acting as cached matrix object) as an input; 
#  returns the inversion of the cached matrix.

cacheSolve <- function(x, ...) {
        # retrieve the inverse matrix stored in the makeCacheMatrix function environment
        inverse <- x$get.inverse()
        # if value is not null, return the cached value
        if (!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        # if cache value is null:
        #   1. Compute the inversion
        #   2. Cache the result
        #   3. Return the result
        inverse <- solve(x$get(), ...)
        x$set.inverse(inverse)
        inverse
}

# Examples/Demonstration of functionality
# -----------------------------------
a = matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, ncol = 3) # A 3x3 identity matrix
                                                     # inverse(a) = a
b = matrix(c(0, -0.8, -0.6, 0.8, -0.36, 0.48, 0.60, 0.48, -0.64), nrow = 3, ncol = 3) 
                                                     # A 3x3 Orthoginal matrix
                                                     # inverse(b) = transpose(b)
c = matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3, ncol = 3, byrow = TRUE)
                                                     # A 3x3 invertible matrix
                                                     # inverse(c) = [-24  18  5]
                                                     #              [ 20 -15 -4]
                                                     #              [ -5   4  1]


ia = solve(a)
ib = solve(b)
ic = solve(c)

M = makeCacheMatrix(a)
iM = cacheSolve(M)
all(iM == ia)
iM = cacheSolve(M)
all(iM == ia)

M$set(b)
iM = cacheSolve(M)
all(iM == ib)
iM = cacheSolve(M)
all(iM == ib)

M$set(c)
iM = cacheSolve(M)
all(iM == ic)
iM = cacheSolve(M)
all(iM == ic)

# Output from demonstration of functionality
# -----------------------------------
# > ia = solve(a)
#
# > ib = solve(b)
#
# > ic = solve(c)
#
# > M = makeCacheMatrix(a)
#
# > iM = cacheSolve(M)
#
# > all(iM == ia)
# [1] TRUE
#
# > iM = cacheSolve(M)
# getting cached data
# 
# > all(iM == ia)
# [1] TRUE
#
# > M$set(b)
#
# > iM = cacheSolve(M)
#
# > all(iM == ib)
# [1] TRUE
#
# > iM = cacheSolve(M)
# getting cached data
# 
# > all(iM == ib)
# [1] TRUE
#
# > M$set(c)
# 
# > iM = cacheSolve(M)
# 
# > all(iM == ic)
# [1] TRUE
# 
# > iM = cacheSolve(M)
# getting cached data
#
# > all(iM == ic)
# [1] TRUE
