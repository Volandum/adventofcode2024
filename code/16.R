input_lines = readLines('./inputs/16.txt')

input_grid = lines_to_matrix(input_lines)

grid_size = nrow(input_grid)

input_grid_coords = data.frame(y = rep(1:grid_size, each = grid_size), x = rep(1:grid_size, times = grid_size))

input_grid_coords$char = input_grid[cbind(input_grid_coords$y, input_grid_coords$x)]

input_grid_with_orientations = rbind(
  input_grid_coords %>% mutate(orientation = 0),
  input_grid_coords %>% mutate(orientation = 1),
  input_grid_coords %>% mutate(orientation = 2),
  input_grid_coords %>% mutate(orientation = 3)
) #E/S/W/N respectively

input_grid_edges = rbind(
  input_grid_with_orientations %>% filter(orientation == 0) %>%
    mutate(other_y = y, other_x = x + 1, other_orientation = orientation, cost = 1),
  input_grid_with_orientations %>% filter(orientation == 1) %>%
    mutate(other_y = y + 1, other_x = x, other_orientation = orientation, cost = 1),
  input_grid_with_orientations %>% filter(orientation == 2) %>%
    mutate(other_y = y, other_x = x - 1, other_orientation = orientation, cost = 1),
  input_grid_with_orientations %>% filter(orientation == 3) %>%
    mutate(other_y = y - 1, other_x = x, other_orientation = orientation, cost = 1),
  input_grid_with_orientations %>% 
    mutate(other_y = y, other_x = x, 
           other_orientation = (orientation + 1) %% 4, cost = 1000),
  input_grid_with_orientations %>% 
    mutate(other_y = y, other_x = x, 
           other_orientation = (orientation - 1) %% 4, cost = 1000)
) %>%
  mutate(first_coord = paste0(y, ',', x, ',', orientation),
         second_coord = paste0(other_y, ',', other_x, ',', other_orientation))

valid_coords = input_grid_with_orientations %>% filter(char != '#') %>%
  mutate(coord = paste0(y, ',', x, ',', orientation))

valid_edges = input_grid_edges %>% filter((first_coord %in% valid_coords$coord) &
                                            (second_coord %in% valid_coords$coord))

transition_graph = igraph::graph_from_data_frame(
  valid_edges %>% transmute(node1 = first_coord, node2 = second_coord, weight = cost),
  directed = TRUE,
  vertices = valid_coords %>% select(coord, char)
)

starting_point = which(input_grid == 'S', arr.ind = TRUE)

starting_coord = paste0(starting_point[1], ',', starting_point[2], ',', 0)

ending_point = which(input_grid == 'E', arr.ind = TRUE)

ending_coords = paste0(ending_point[1], ',', ending_point[2], ',', c(0,1,2,3))

distances = distances(transition_graph,
          starting_coord, ending_coords, 
          mode = 'out', weights = NULL)

min(distances) #122492

best_target = colnames(distances)[which.min(distances)]

valid_paths = all_shortest_paths(
  transition_graph,
  starting_coord, best_target, 
  mode = 'out', weights = NULL)

valid_vertices = names(unlist(valid_paths$vpaths))

valid_tiles = unique(gsub(
  r'((\d+,\d+),\d+)', '\\1', valid_vertices
))

length(valid_tiles)
