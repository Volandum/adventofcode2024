input_lines = readLines('./inputs/12.txt')

input_grid = lines_to_matrix(input_lines)

grid_size = length(input_lines)

input_grid_coords = data.frame(y = rep(1:grid_size, each = grid_size), x = rep(1:grid_size, times = grid_size))

input_grid_coords$plant = input_grid[cbind(input_grid_coords$y, input_grid_coords$x)]

input_grid_edges = rbind(
  input_grid_coords %>% mutate(other_x = x - 1, other_y = y),
  input_grid_coords %>% mutate(other_x = x + 1, other_y = y),
  input_grid_coords %>% mutate(other_x = x, other_y = y - 1),
  input_grid_coords %>% mutate(other_x = x, other_y = y + 1)
)

input_grid_edges$other_plant = '.'

input_grid_edges[input_grid_edges$other_x <= grid_size & input_grid_edges$other_x > 0 &
                   input_grid_edges$other_y <= grid_size & input_grid_edges$other_y > 0,]$other_plant = 
  input_grid[cbind(input_grid_edges[input_grid_edges$other_x <= grid_size & input_grid_edges$other_x > 0 &
                                                       input_grid_edges$other_y <= grid_size & input_grid_edges$other_y > 0,]$other_y, 
                                    input_grid_edges[input_grid_edges$other_x <= grid_size & input_grid_edges$other_x > 0 &
                                                                 input_grid_edges$other_y <= grid_size & input_grid_edges$other_y > 0,]$other_x)]

input_grid_edges$first_coords = paste0(input_grid_edges$x, ',', input_grid_edges$y)
input_grid_edges$second_coords = paste0(input_grid_edges$other_x, ',', input_grid_edges$other_y)

region_interior_edges = input_grid_edges %>% filter(plant == other_plant)

region_graph = region_interior_edges %>% select(first_coords, second_coords) %>%
  graph_from_data_frame(directed = FALSE, vertices = input_grid_edges %>% distinct(first_coords, plant))

components = components(region_graph)

component_df = data.frame(coords = V(region_graph)$name, regionid = components$membership)

input_grid_edges_with_regions = input_grid_edges %>% inner_join(component_df, by = c('first_coords' = 'coords'))

region_sizes = input_grid_edges_with_regions %>% group_by(regionid) %>%
  summarise(area = n_distinct(first_coords))

region_perimeters = input_grid_edges_with_regions %>%
  filter(plant != other_plant) %>% group_by(regionid) %>%
  summarise(perimeter = n())

fencing_details = region_sizes %>% inner_join(region_perimeters) %>%
  mutate(cost = area * perimeter)

sum(fencing_details$cost) #1387004

boundaries_data = 
  input_grid_edges_with_regions %>%
  filter(plant != other_plant) %>% 
  transmute(edgetype = paste0(other_x - x, ',', other_y - y),
                                              y, x, regionid) %>%
  mutate(edgeid = paste0(y, ',', x, ';', edgetype),
         coords = paste0(y, ',', x))

edge_contiguity = input_grid_edges %>% #source of adjacent points
  select(first_coords, second_coords) %>%
  inner_join(boundaries_data %>% transmute(first_coords = coords, first_edgeid = edgeid,
                                           first_edgetype = edgetype, first_regionid = regionid),
             by = "first_coords") %>%
  inner_join(boundaries_data %>% transmute(second_coords = coords, second_edgeid = edgeid,
                                           second_edgetype = edgetype, second_regionid = regionid),
             by = "second_coords") %>% 
  filter(first_edgetype == second_edgetype) %>% 
  filter(first_regionid == second_regionid)

edge_graph = 
  edge_contiguity %>% select(first_edgeid, second_edgeid) %>%
  graph_from_data_frame(directed = FALSE, vertices = edge_contiguity %>% distinct(first_edgeid))

edge_components = components(edge_graph)

edge_component_df = data.frame(edgeid = V(edge_graph)$name, edgegroupid = edge_components$membership)

boundaries_data_with_edgegroupids = 
  boundaries_data %>% left_join(edge_component_df, by = 'edgeid') %>%
  mutate(edgegroupid = coalesce(edgegroupid, 1000000 + 1:nrow(boundaries_data)))

region_edge_counts = boundaries_data_with_edgegroupids %>%
  group_by(regionid) %>%
  summarise(edges = n_distinct(edgegroupid))

fencing_details_discount = region_sizes %>% inner_join(region_edge_counts) %>%
  mutate(cost = area * edges)

sum(fencing_details_discount$cost) #844198
