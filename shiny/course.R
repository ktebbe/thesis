courses <- read.csv("courses.csv", as.is=TRUE)
courses <- courses[,-1]

selectClasses <- function(){
  courseNumbers <- sample(1:nrow(courses), 2)
  Source <- courses$Dept1[courseNumbers[1]]
  Target <- courses$Dept1[courseNumbers[2]]
  return(c(Source, Target))
}

getName <- function(name){
  ## Get entire department name & number from code (e.g. "AFAM060").
  deptNum <- 1
  row <- which(courses$Dept1 == name)
  toPrint <- courses[row, paste("Dept", deptNum, sep="")]
  deptNum <- deptNum + 1
  while(!is.na(courses[row, paste("Dept", deptNum, sep="")])){
    toPrint <- paste(toPrint, "/", courses[row, paste("Dept", deptNum, sep="")])
    deptNum <- deptNum + 1
  }
  toPrint <- paste(toPrint, ": ", sep="")
  toPrint <- paste(toPrint, courses[row, "Title"], sep="")
  
  return(toPrint)
}


