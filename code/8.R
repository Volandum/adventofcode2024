input_lines = readLines('./inputs/8.txt')

input_grid = lines_to_matrix(input_lines)

antennas = which(arr.ind = TRUE, input_grid != '.')

input_grid_size = length(input_lines)

antenna_details_df = data.frame(
  y = antennas[,1],
  x = antennas[,2],
  frequency = input_grid[antennas]
)

antinodes = antenna_details_df %>% inner_join(
  antenna_details_df, by = 'frequency', relationship = 'many-to-many'
) %>% filter((x.x != x.y) | (y.x != y.y)) %>% mutate(new_x = 2 * x.y - x.x,
                                                     new_y = 2 * y.y - y.x) %>%
  filter(new_x >= 1, new_y >= 1, new_x <= input_grid_size, new_y <= input_grid_size)

antinodes %>% distinct(new_x, new_y) %>% nrow() #305

lines = antenna_details_df %>% inner_join(
  antenna_details_df, by = 'frequency', relationship = 'many-to-many'
) %>% filter((x.x != x.y) | (y.x != y.y))

potential_diffs = data.frame(y_minus_x_multiplier = -51:51)

resonant_antinodes = lines %>% cross_join(potential_diffs) %>%
  mutate(new_x = x.y + (y_minus_x_multiplier * (x.y - x.x)),
         new_y = y.y + (y_minus_x_multiplier * (y.y - y.x))) %>%
  filter(new_x >= 1, new_y >= 1, new_x <= input_grid_size, new_y <= input_grid_size)

resonant_antinodes %>% distinct(new_x, new_y) %>% nrow() #1150
