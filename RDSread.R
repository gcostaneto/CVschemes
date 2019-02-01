#' returns a dataframe using a set of rds files within a specific directorie
#' .path   :   directorie path
#' .pattern: file identification pattern (see list.files() function)
#' .id     : name of the index column. If .id = NULL, in this case, "id" is used as colum name.
RDSread <- function(.path,.pattern,.id="id"){
  setwd(.path)
  LISTF <- list.files(pattern = .pattern)
  LEG <- length(LISTF)
  cat(paste("Total of",LEG,"files in the directorie",.path,"\n",sep=""))
  output <- c()
  for(L in 1:LEG){
    output <- rbind(output,data.frame(readRDS(LISTF[L]),.id = LISTF[[L]]))
  }
  names(output)[dim(output)[2]]<-.id
  return(output)
  cat(paste("Data frame containing",dim(output)[1],"rows and", dim(output)[2],"columns \n",sep=""))

}
