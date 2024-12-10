input_lines = readLines('./inputs/10.txt')

input_grid = lines_to_matrix(input_lines)

grid_size = length(input_lines)

input_grid_coords = data.frame(y = rep(1:grid_size, each = grid_size), x = rep(1:grid_size, times = grid_size))

input_grid_coords$digit = as.numeric(input_grid[cbind(input_grid_coords$y, input_grid_coords$x)])

input_grid_edges = sqldf(
  "
  select first.digit as first_digit, first.x as first_x, first.y as first_y,
  second.digit as second_digit, second.x as second_x, second.y as second_y
  from input_grid_coords first inner join input_grid_coords second
  on first.digit = second.digit - 1 and 
  (
  (first.x = second.x and (first.y - second.y = 1 or second.y - first.y = 1))
  or 
  (first.y = second.y and (first.x - second.x = 1 or second.x - first.x = 1))
  )
  "
)

long_trails = sqldf(
  "
  with recursive partial_trail as (
  select first_digit, first_x, first_y, second_digit as last_digit, second_x as last_x, second_y as last_y
  from input_grid_edges where first_digit = 0
  union
  select partial_trail.first_digit, partial_trail.first_x, partial_trail.first_y, 
  next.second_digit as last_digit, next.second_x as last_x, next.second_y as last_y
  from partial_trail inner join input_grid_edges next
  on partial_trail.last_digit = next.first_digit 
  and partial_trail.last_x = next.first_x
  and partial_trail.last_y = next.first_y
  )
  select * from partial_trail where last_digit = 9
  "
)

nrow(long_trails %>% distinct(first_x, first_y, last_x, last_y)) #617

input_grid_edges_coords = sqldf(
  "
  select first.digit as first_digit, first.x as first_x, first.y as first_y, first.x || ',' || first.y as first_coord,
  second.digit as second_digit, second.x as second_x, second.y as second_y, second.x || ',' || second.y as second_coord
  from input_grid_coords first inner join input_grid_coords second
  on first.digit = second.digit - 1 and 
  (
  (first.x = second.x and (first.y - second.y = 1 or second.y - first.y = 1))
  or 
  (first.y = second.y and (first.x - second.x = 1 or second.x - first.x = 1))
  )
  "
)

long_trails_coords = sqldf(
  "
  with recursive partial_trail as (
  select first_digit, first_x, first_y, 
  second_digit as last_digit, second_x as last_x, second_y as last_y,
  first_coord || ';' || second_coord as path
  from input_grid_edges_coords where first_digit = 0
  union
  select partial_trail.first_digit, partial_trail.first_x, partial_trail.first_y, 
  next.second_digit as last_digit, next.second_x as last_x, next.second_y as last_y,
  path || ';' || next.second_coord as path
  from partial_trail inner join input_grid_edges_coords next
  on partial_trail.last_digit = next.first_digit 
  and partial_trail.last_x = next.first_x
  and partial_trail.last_y = next.first_y
  )
  select * from partial_trail where last_digit = 9
  "
)

nrow(long_trails_coords %>% distinct(path)) #1477
