input_lines = readLines('./inputs/6.txt')

input_grid = lines_to_matrix(input_lines)

starting_position = which(arr.ind = TRUE, input_grid == '^')

input_grid[90,83]

starting_position_y = 90
starting_position_x = 83
starting_direction = 'U'

turn_right_directions = c(U = 'R', R = 'D', D = 'L', L = 'U')

trace_df = data.frame(y = starting_position_y, x = starting_position_x, direction = starting_direction)

position_y = starting_position_y
position_x = starting_position_x
direction = starting_direction

next_step = function(x, y, dir){
  if(dir == 'U'){
    new_x = x
    new_y = y - 1
  }else if(dir == 'R'){
    new_x = x + 1
    new_y = y
  }else if(dir == 'D'){
    new_x = x
    new_y = y + 1
  }else if(dir == 'L'){
    new_x = x - 1
    new_y = y
  }else{
    assert(FALSE)
  }
  if((new_x < 1) | (new_x > 130) | (new_y < 1) | (new_y > 130)){
    return(NULL)
  }
  if(input_grid[new_y, new_x] == '#'){
    new_direction = turn_right_directions[dir]
    return(list(new_x = x, new_y = y, new_direction = new_direction))
  }
  return(list(new_x = new_x, new_y = new_y, new_direction = dir))
}

keep_going = TRUE
while(keep_going){
  next_position = next_step(position_x, position_y, direction)
  if(is.null(next_position)){
    keep_going = FALSE
  } else {
    position_x = next_position$new_x
    position_y = next_position$new_y
    direction = next_position$new_direction
    trace_df = rbind(trace_df,
                     data.frame(y = position_y, x = position_x, direction = direction))
  }
}

nrow(distinct(trace_df %>% select(y, x))) #5080

# Treat it as a graph theory problem

all_positions = which(input_grid != '#', arr.ind = TRUE)

all_positions_up = data.frame(y = all_positions[,1], x = all_positions[,2],
                              dir = 'U')

positions_up_next_steps = apply(all_positions, 
                                MARGIN = 1,
                                FUN = function(position){
                                  next_step(position[2], position[1], 'U')
                                },
                                simplify = FALSE)

node_to_string = Vectorize(function(y, x, dir){
  paste0(c(as.character(y), as.character(x), dir), collapse = ',')
})

up_can_continue = !sapply(positions_up_next_steps, is.null)
viable_next_steps_up = do.call(rbind, lapply(positions_up_next_steps[up_can_continue], data.frame))

all_edges_up = data.frame(
  starting_node = node_to_string(y = all_positions[up_can_continue,1],
                                 x = all_positions[up_can_continue,2],
                                 dir = 'U'),
  subsequent_node = node_to_string(y = viable_next_steps_up$new_y,
                                   x = viable_next_steps_up$new_x,
                                   dir = viable_next_steps_up$new_direction)
)

positions_right_next_steps = apply(all_positions, 
                                   MARGIN = 1,
                                   FUN = function(position){
                                     next_step(position[2], position[1], 'R')
                                   },
                                   simplify = FALSE)
right_can_continue = !sapply(positions_right_next_steps, is.null)
viable_next_steps_right = do.call(rbind, lapply(positions_right_next_steps[right_can_continue], data.frame))

all_edges_right = data.frame(
  starting_node = node_to_string(y = all_positions[right_can_continue,1],
                                 x = all_positions[right_can_continue,2],
                                 dir = 'R'),
  subsequent_node = node_to_string(y = viable_next_steps_right$new_y,
                                   x = viable_next_steps_right$new_x,
                                   dir = viable_next_steps_right$new_direction)
)

positions_down_next_steps = apply(all_positions, 
                                   MARGIN = 1,
                                   FUN = function(position){
                                     next_step(position[2], position[1], 'D')
                                   },
                                   simplify = FALSE)
down_can_continue = !sapply(positions_down_next_steps, is.null)
viable_next_steps_down = do.call(rbind, lapply(positions_down_next_steps[down_can_continue], data.frame))

all_edges_down = data.frame(
  starting_node = node_to_string(y = all_positions[down_can_continue,1],
                                 x = all_positions[down_can_continue,2],
                                 dir = 'R'),
  subsequent_node = node_to_string(y = viable_next_steps_down$new_y,
                                   x = viable_next_steps_down$new_x,
                                   dir = viable_next_steps_down$new_direction)
)

positions_left_next_steps = apply(all_positions, 
                                  MARGIN = 1,
                                  FUN = function(position){
                                    next_step(position[2], position[1], 'L')
                                  },
                                  simplify = FALSE)
left_can_continue = !sapply(positions_left_next_steps, is.null)
viable_next_steps_left = do.call(rbind, lapply(positions_left_next_steps[left_can_continue], data.frame))

all_edges_left = data.frame(
  starting_node = node_to_string(y = all_positions[left_can_continue,1],
                                 x = all_positions[left_can_continue,2],
                                 dir = 'R'),
  subsequent_node = node_to_string(y = viable_next_steps_left$new_y,
                                   x = viable_next_steps_left$new_x,
                                   dir = viable_next_steps_left$new_direction)
)

all_edges = rbind(all_edges_down, all_edges_left, all_edges_right, all_edges_up)

big_digraph = igraph::graph_from_data_frame(all_edges, directed = TRUE)

successors_of_start = subcomponent(big_digraph, '90,83,U', mode = 'out')

next_step_with_grid = function(x, y, dir, grid){
  if(dir == 'U'){
    new_x = x
    new_y = y - 1
  }else if(dir == 'R'){
    new_x = x + 1
    new_y = y
  }else if(dir == 'D'){
    new_x = x
    new_y = y + 1
  }else if(dir == 'L'){
    new_x = x - 1
    new_y = y
  }else{
    assert(FALSE)
  }
  if((new_x < 1) | (new_x > 130) | (new_y < 1) | (new_y > 130)){
    return(NULL)
  }
  if(grid[new_y, new_x] == '#'){
    new_direction = turn_right_directions[dir]
    return(list(new_x = x, new_y = y, new_direction = new_direction))
  }
  return(list(new_x = new_x, new_y = new_y, new_direction = dir))
}


test_added_obstacle = function(obstacle_y, obstacle_x){
  new_grid = input_grid
  new_grid[obstacle_y, obstacle_x] = '#'
  position_y = starting_position_y
  position_x = starting_position_x
  direction = starting_direction
  # trace_df = data.frame(y = starting_position_y, x = starting_position_x, direction = starting_direction)
  arrived_positions = character(0)
  keep_going = TRUE
  while(keep_going){
    next_position = next_step_with_grid(position_x, position_y, direction, new_grid)
    if(is.null(next_position)){
      keep_going = FALSE
    } else {
      position_x = next_position$new_x
      position_y = next_position$new_y
      direction = next_position$new_direction
      new_position = node_to_string(next_position$new_y, next_position$new_x, next_position$new_direction)
      if(new_position %in% arrived_positions){
        return(TRUE) #loop
      }
      arrived_positions = c(arrived_positions, new_position)
      # trace_df = rbind(trace_df,
      #                  data.frame(y = position_y, x = position_x, direction = direction))
    }
  }
  return(FALSE)
}

potential_obstacle_positions = distinct(trace_df %>% select(y, x))

#progress bar from https://stackoverflow.com/a/69603959/11732165
install.packages('svMisc')
library(svMisc)
rows = nrow(potential_obstacle_positions)
for (i in 1:rows){
  obstacle_causes_loop[i] = test_added_obstacle(obstacle_y = potential_obstacle_positions$y[i],
                                              obstacle_x = potential_obstacle_positions$x[i])
  progress(i,rows)
} 

install.packages('future.apply')
library(future.apply)
plan(multisession) ## Run in parallel on local computer

obstacle_causes_loop = future_sapply(
                                1:rows,
                                FUN = function(i){
                                  test_added_obstacle(obstacle_y = potential_obstacle_positions$y[i],
                                                      obstacle_x = potential_obstacle_positions$x[i])
                                })

sum(obstacle_causes_loop) #1919
