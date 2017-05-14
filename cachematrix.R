#This function creates a special "MATRIX" which is a list containing functions to:
#1.	set the value of the Matrix
#2.	get the value of the Matrix
#3.	set the value of the Inverse Matrix
#4.	get the value of the Inverse Matrix
#Return Object is a list of the above 4 function names
#The caluated Inverse Matrix is stored as a cache

makeCacheMatrix <- function(x = matrix()) {
  #x is input arg, default as empty matrix
  m <- NULL   #initialised as a null object
  
  #set new Matrix to parent x(matrix) & reset values in parent m(list)
  set <- function(y) {
    x <<- y            #assign y(input arg) to the parent object x(overite x)
    m <<- NULL         #reset the parent m to NULL
  }#set
  
  #get the matrix values
  get <- function() x  #return parent matrix x
  
  #Set the inverse matrix in the parent object m
  setinverse <- function(inverse) m <<- inverse  #pass arg mean and assign to parent object m
  
  #get the Inverse Matrix
  getinverse <- function() m  #return the parent object m
  
  #assign each object name with the above function names
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse) 
}#makeCacheMatrix


#-----------------------------------------------------------------------
#This function calculates the Inverse of the special "MATRIX" created with makeCacheMatrix()
#If the Inverse has already been calculated, it gets the result from the cache and skips the working.
#Otherwise, it calculates the Inverse of the matrix and,
#sets the value of the Inverse in the cache via setinverse() in makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a object from the makeCacheMatrix()
  m <- x$getinverse()
  
  #checks if m is NOT NULL, ie. the inverse has already been created
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  #Return m, No working needed
  }
 
  #calculates & create the inverse of the data
  #Sets the  inverse in the cache via the setinverse() makeCacheMatrix()
  data <- x$get()
  m <- solve(data, ...)  #Worked out the Inverse
  x$setinverse(m)       # Store inverse in object m of makeCacheMatrix()
  m
}#cacheSolve

#------------------------------------------------------------------------------------
# To Check functions in catchematrix.R
# execute the following testing modules, line by line.

#------------ Testing 1A:  set 1 : m1 & n1 -----------------------
#Create a SQUARE 2x2 Matrix m1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow=2, ncol=2)

#Call makeCacheMatrix() to create the special "MATRIX" object
myMatrix_object_1 <- makeCacheMatrix(m1)

#Call cacheSolve() to create the Inverse Matrix in the same special "MATRIX" object
cacheSolve (myMatrix_object_1)

#Get the Inverse Matrix from the special "MATRIX" object, call it n1
n1 <- myMatrix_object_1$getinverse()

#Use propertise of multiplication of Inverse Matrix to get Identity Matrix
m1 %*% n1

#----Sucessful on Inverse-------

#-------- Testing 1B: of retrieving Inverse Inverse from Chache -----
cacheSolve (myMatrix_object_1)  #Message showed "getting cached data" - No need to calculate Inverse

#-----Change the Matrixnin the Object myMatrix_object_1 with Matrix n1 - which was originaly the INverse----------
myMatrix_object_1$set(n1)

#Call cacheSolve() to create the Inverse Matrix in the same special "MATRIX" object
cacheSolve (myMatrix_object_1) #Inverse calculated

#Get the Inverse Matrix from the special "MATRIX" object, call it p1
p1 <- myMatrix_object_1$getinverse()

#Use propertise of multiplication of Inverse Matrix to get Identity Matrix
n1 %*% p1   #Values with precision accuracy but approaching zeros

#----Sucessful on Inverse-------

#--------------------------------------------------------------------------------------
# Testing on second set : m2, n2 ------
#Create a SQUARE 2x2 Matrix m2
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow=2, ncol=2)

#Call makeCacheMatrix() to create the special "MATRIX" object
myMatrix_object_2 <- makeCacheMatrix(m2)

#Call cacheSolve() to create the Inverse Matrix in the same special "MATRIX" object
cacheSolve (myMatrix_object_2)

#Get the Inverse Matrix from the special "MATRIX" object, call it n2
n2 <- myMatrix_object_2$getinverse()

#Use propertise of multiplication of Inverse Matrix to get Identity Matrix
m2 %*% n2

#----Sucessful on Inverse-------

#---- End of Testing ------------------------------------
