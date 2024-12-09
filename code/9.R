input_lines = readLines('./inputs/9.txt')
input_line_split = as.numeric(unlist(strsplit(input_lines, '')))

positions_df_basic = 
  data.frame(id = 1:length(input_line_split), len = input_line_split)

positions_df = positions_df_basic %>%
  mutate(new_id = ifelse(id %% 2 == 0, 
                         0, (id + 1)/2),
         end_position = cumsum(len)) %>%
  mutate(start_position = lag(end_position, default = 0) + 1)

total_numbers = sum(positions_df %>% filter(new_id != 0))
free_spaces = sum(positions_df %>% filter(new_id == 0) %>% pull(len))

position_level_df = data.frame(ids = rep(positions_df$new_id, times = positions_df$len) - 1,
                               position = 0:(sum(positions_df$len) - 1)) %>%
  mutate(has_free_space = cumsum(ids == -1), has_numbers_to_move = rev(cumsum(rev((ids != -1)))))
                               

pre_move = position_level_df %>% filter(has_free_space <= has_numbers_to_move)

post_move = position_level_df %>% filter(has_free_space >= has_numbers_to_move) #there is an overlap

sum(pre_move$ids == -1) == sum(post_move$ids != -1)

new_df = pre_move

new_df$ids[new_df$ids == -1] = rev(post_move$ids[post_move$ids != -1])

print(sum(new_df$ids * new_df$position)) #6349606724455 (sometimes the last one gets double counted dependong on how the equality between the two cumsums is attained)

positions_df_block_moves = positions_df %>% select(-id)
library(svMisc)

for(id in max(positions_df_block_moves$new_id):1){
  progress(10000-id,10000)
  block_size = positions_df_block_moves %>% filter(new_id == id) %>%
    pull(len)
  original_start_position = positions_df_block_moves %>% filter(new_id == id) %>%
    pull(start_position)
  replacement_candidates = positions_df_block_moves %>%
    filter(new_id == 0) %>% filter(len >= block_size) %>% filter(start_position <= original_start_position)
  if(nrow(replacement_candidates) > 0){
    chosen_candidate = replacement_candidates %>% slice_min(order_by = start_position, n = 1)
    new_start_position = chosen_candidate$start_position
    existing_gap_length = chosen_candidate$len
    if(existing_gap_length == block_size){
      positions_df_block_moves[positions_df_block_moves$start_position == new_start_position, 'new_id'] = id
      positions_df_block_moves[positions_df_block_moves$start_position == original_start_position, 'new_id'] = 0
      # Don't need to merge spaces because we won't be back here
    } else {
      positions_df_block_moves[positions_df_block_moves$start_position == new_start_position, 'len'] = existing_gap_length - block_size
      positions_df_block_moves[positions_df_block_moves$start_position == new_start_position, 'start_position'] = new_start_position + block_size
      positions_df_block_moves[positions_df_block_moves$start_position == original_start_position, 'end_position'] = new_start_position + block_size - 1
      positions_df_block_moves[positions_df_block_moves$start_position == original_start_position, 'start_position'] = new_start_position
    }
  }
}

positions_df_block_moves %>% filter(new_id != 0) %>% summarise(
  checksum = sum((new_id - 1) * (end_position + start_position - 2) * len / 2)
) # 6376648986651
