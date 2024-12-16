input_lines = readLines('./inputs/15.txt')

break_point = which(nchar(input_lines) == 0)

input_grid = lines_to_matrix(input_lines[1:(break_point - 1)])

grid_size = nrow(input_grid)

commands = unlist(strsplit(input_lines[(break_point + 1):length(input_lines)], ''))

starting_at_location = which(input_grid == '@', arr.ind = TRUE)

apply_command = function(grid, at_location, command){
  if(command == 'v' | command == '^'){
    relevant_area = grid[, at_location[2]]
    at_position = at_location[1]
  } else {
    relevant_area = grid[at_location[1],]
    at_position = at_location[2]
  }
  if(command == 'v' | command == '>'){
    relevant_area = rev(relevant_area)
    at_position = grid_size + 1 - at_position
  }
  # Only consider the moving left scenario
  at_position_temp = at_position
  last_hash = max(which(relevant_area[1:at_position] == '#')) #should exist as there is always a # at 1
  remaining_area = relevant_area[(last_hash + 1):at_position]
  if(any(remaining_area == '.')){
    # if not then do nothing
    last_dot = max(which(remaining_area == '.'))
    remaining_area[last_dot:length(remaining_area)] = c(
      rep('O', length(remaining_area) - last_dot - 1),'@', '.')
    at_position = at_position - 1
  }
  relevant_area[(last_hash + 1):at_position_temp] = remaining_area
  if(command == 'v' | command == '>'){
    relevant_area = rev(relevant_area)
    at_position = grid_size + 1 - at_position
  }
  if(command == 'v' | command == '^'){
    grid[, at_location[2]] = relevant_area
    at_location[1] = at_position
  } else {
    grid[at_location[1],] = relevant_area
    at_location[2] = at_position
  }
  return(list(new_grid = grid, new_at_location = at_location))
}

current_grid = input_grid
current_at_location = starting_at_location
for(command in commands){
  result_of_command = apply_command(current_grid, current_at_location, command)
  current_grid = result_of_command$new_grid
  current_at_location = result_of_command$new_at_location
}

coords = which(current_grid == 'O', arr.ind = TRUE)
gps_coords = sum(coords[,1] - 1) * 100 + sum(coords[,2] - 1)

gps_coords

input_lines_subset = input_lines[1:(break_point - 1)]

split_input_lines_subset = strsplit(input_lines_subset, '')

to_double_width_map = function(char){
  return(c(`#` = '##',
           `O` = '[]',
           `.` = '..',
           `@` = '@.')[char])
}

temp = unlist(lapply(
  lapply(split_input_lines_subset, Vectorize(to_double_width_map)),
  function(bits){paste0(bits, collapse = '')}))

double_width_grid = lines_to_matrix(temp)
