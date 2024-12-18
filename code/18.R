input_lines = readLines('./inputs/18.txt')

dimension = 70

impact_locations = data.frame(
  id = 1:length(input_lines),
  x = as.numeric(split_vectorised(input_lines, ',', 1)),
  y = as.numeric(split_vectorised(input_lines, ',', 2))
)

nodes = data.frame(
  x = rep(0:dimension, times = dimension + 1),
  y = rep(0:dimension, each = dimension + 1)
) %>%
  mutate(coords = paste0(x, ',', y))

node_connectivity = rbind(
  nodes %>% mutate(new_x = x + 1, new_y = y),
  nodes %>% mutate(new_x = x - 1, new_y = y),
  nodes %>% mutate(new_x = x, new_y = y + 1),
  nodes %>% mutate(new_x = x, new_y = y - 1)) %>% 
  mutate(new_coords = paste0(new_x, ',', new_y)) %>%
  filter((new_x %in% 0:dimension) & (new_y %in% 0:dimension))

first_corruptions = input_lines[1:1024]

new_connectivity = node_connectivity %>% filter(!(coords %in% first_corruptions),
                                                !(new_coords %in% first_corruptions))

connectivity_graph = igraph::graph_from_data_frame(new_connectivity %>% select(coords, new_coords), 
                                           directed = FALSE, 
                                           vertices = nodes %>% select(coords) %>%
                                             filter(!(coords %in% first_corruptions)))

distances(connectivity_graph, v = "0,0", to = "70,70") #296

check_connectivity_after_n = function(n){
  first_corruptions = input_lines[1:n]
  
  new_connectivity = node_connectivity %>% filter(!(coords %in% first_corruptions),
                                                  !(new_coords %in% first_corruptions))
  
  connectivity_graph = igraph::graph_from_data_frame(new_connectivity %>% select(coords, new_coords), 
                                                     directed = FALSE, 
                                                     vertices = nodes %>% select(coords) %>%
                                                       filter(!(coords %in% first_corruptions)))
  
  distances(connectivity_graph, v = "0,0", to = "70,70")
}

check_connectivity_after_n(2048) #298
check_connectivity_after_n(4000) #Inf
check_connectivity_after_n(3000) #476
check_connectivity_after_n(3500) #Inf
check_connectivity_after_n(3250) #Inf
check_connectivity_after_n(3100) #Inf
check_connectivity_after_n(3050) #Inf
check_connectivity_after_n(3020) #476
check_connectivity_after_n(3035) #Inf
check_connectivity_after_n(3027) #Inf
check_connectivity_after_n(3023) #476
check_connectivity_after_n(3025) #476
check_connectivity_after_n(3026)
