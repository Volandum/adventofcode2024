input_lines = readLines('./inputs/1.txt')

left_vector = as.numeric(split_vectorised(input_lines, ' +', 1))
right_vector = as.numeric(split_vectorised(input_lines, ' +', 2))

left_sorted = sort(left_vector)
right_sorted = sort(right_vector)

differences = left_sorted - right_sorted
sum(abs(differences)) #1506483

left_counts = data.frame(left_vector) %>% count(left_vector)
right_counts = data.frame(right_vector) %>% count(right_vector)

overlaps = left_counts %>% inner_join(right_counts, by = join_by(left_vector == right_vector))

overlaps %>% mutate(increment = left_vector * n.x * n.y) %>% pull(increment) %>% sum() #23126924
