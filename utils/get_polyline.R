# get the line from route line column value and returns a matrix with lon in firt column and ñlat in second
get_polyline_matrix <- function(x){
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub("\\[", " ", x)
  x <- gsub("\\]", " ", x)
  x <- gsub("\\,", " ", x)
  
  return(matrix(scan(text = x), ncol = 2, byrow = TRUE))
}


