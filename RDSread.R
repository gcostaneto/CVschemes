#' returns a dataframe using a set of rds files within a specific directorie
#' ,path   :   directorie path
#' ,pattern: file identification pattern (see list.files() function)
#' ,id     : name of the index column. If .id = NULL, in this case, "id" is used as colum name.
RDSread <- function(.path,.pattern,.id="id"){
  setwd(.path)
  LISTF <- list.files(pattern = .pattern)
  output <- c()
  for(L in 1:length(LISTF)){
    output <- rbind(output,data.frame(readRDS(LISTF[L]),.id = LISTF[[L]]))
  }
  names(output)[dim(output)[2]]<-.id
  return(output)
}