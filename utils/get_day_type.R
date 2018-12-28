get_day_type <- function(x){
  if (x <= 5) {
    return('Laboral')
  }
  else if (x == 6) {
    return('Sábado')
  }
  else{
    return('Domingo')
  }
}