input_lines = readLines('./inputs/3.txt')

mul_pieces = gregexpr('mul\\(\\d+,\\d+\\)', input_lines, perl = TRUE)

matches = regmatches(input_lines, mul_pieces)

unlisted_matches = unlist(matches)

match_df = data.frame(left = as.numeric(split_vectorised(unlisted_matches, 'mul\\(|,|\\)', 2)),
                      right = as.numeric(split_vectorised(unlisted_matches, 'mul\\(|,|\\)', 3)))

sum(match_df$left * match_df$right) #175015740

mul_pieces_and_conditions = gregexpr("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)", input_lines, perl = TRUE)
matches_with_conditions = regmatches(input_lines, mul_pieces_and_conditions)
unlisted_matches_with_conditions = unlist(matches_with_conditions)

matches_to_keep = character(0)

enabled = TRUE

for (item in unlisted_matches_with_conditions){
  if(item == 'do()'){
    enabled = TRUE
  }else if(item == "don't()"){
    enabled = FALSE
  }else if(enabled){
    matches_to_keep = c(matches_to_keep, item)
  }
}

match_df_after_conditions = 
  data.frame(left = as.numeric(split_vectorised(matches_to_keep, 'mul\\(|,|\\)', 2)),
             right = as.numeric(split_vectorised(matches_to_keep, 'mul\\(|,|\\)', 3)))

sum(match_df_after_conditions$left * match_df_after_conditions$right) #112272912
