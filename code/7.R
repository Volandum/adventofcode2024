input_lines = readLines('./inputs/7.txt')

# work backwards to avoid creating lots of big numbers

input_line_outputs = as.numeric(split_vectorised(input_lines, ':', 1))

input_line_values = lapply(strsplit(split_vectorised(input_lines, ': ', 2), ' '), as.numeric)

all(sapply(input_line_values, function(values){all(values > 0)})) # TRUE

verify_input_line_recursive = function(line, possible_outputs){
  if(length(line) == 1){
    return(line %in% possible_outputs)
  }
  last_value_in_line = line[length(line)]
  dividable_items = possible_outputs[(possible_outputs %% last_value_in_line) == 0]
  division_possibilities = dividable_items/last_value_in_line
  subtractable_items = possible_outputs[possible_outputs > last_value_in_line]
  subtraction_possibilities = subtractable_items - last_value_in_line
  possibilities = c(division_possibilities, subtraction_possibilities)
  if(length(possibilities) == 0){
    return(FALSE)
  }
  return(verify_input_line_recursive(line[1:(length(line) - 1)], possibilities))
}

verification_outcomes = mapply(verify_input_line_recursive, input_line_values, input_line_outputs)

options(scipen=999)
print(sum(input_line_outputs[verification_outcomes])) #1038838357795

verify_input_line_recursive_with_concat = function(line, possible_outputs){
  if(length(line) == 1){
    return(line %in% possible_outputs)
  }
  last_value_in_line = line[length(line)]
  dividable_items = possible_outputs[(possible_outputs %% last_value_in_line) == 0]
  division_possibilities = dividable_items/last_value_in_line
  subtractable_items = possible_outputs[possible_outputs > last_value_in_line]
  subtraction_possibilities = subtractable_items - last_value_in_line
  
  last_value_in_line_digits = floor(log10(last_value_in_line)) + 1
  concatenable_items = possible_outputs[
    (possible_outputs >= (10^last_value_in_line_digits) + last_value_in_line) &
      ((possible_outputs - last_value_in_line) %% (10^last_value_in_line_digits) == 0)]
  concatenation_possibilities = (possible_outputs - last_value_in_line) / (10^last_value_in_line_digits)
  
  possibilities = c(division_possibilities, subtraction_possibilities, concatenation_possibilities)
  if(length(possibilities) == 0){
    return(FALSE)
  }
  return(verify_input_line_recursive_with_concat(line[1:(length(line) - 1)], possibilities))
}


verification_outcomes_with_concat = mapply(verify_input_line_recursive_with_concat, input_line_values, input_line_outputs)

print(sum(input_line_outputs[verification_outcomes_with_concat])) #254136560217241
