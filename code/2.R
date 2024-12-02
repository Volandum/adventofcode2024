input_lines = readLines('./inputs/2.txt')

line_data = lapply(strsplit(input_lines, ' '), as.numeric)

check_safety = function(line){
  differences = diff(line)
  if(all(differences < 0 & differences > -4)){
    return(TRUE)
  } else if(all(differences > 0 & differences < 4)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

safety_status = sapply(line_data, check_safety)

sum(safety_status) #564

check_dampened_safety = function(line){
  if(check_safety(line)){
    return(TRUE)
  }
  for(index in 1:length(line)){
    if(check_safety(line[-1 * index])){
      return(TRUE)
    }
  }
  return(FALSE)
}

safety_status_v2 = sapply(line_data, check_dampened_safety)

sum(safety_status_v2) #604
