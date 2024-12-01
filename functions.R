split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

print_large_number = function(number){
  format(number, scientific = F)
}

get_regex_match = Vectorize(function(text, pattern, position = 1){
  unlist(regmatches(text, regexec(pattern, text, perl = T)))[position + 1]
})

lines_to_matrix = function(lines){
  matrix(unlist(strsplit(lines, '')),
         nrow = length(lines), byrow = T)
}

process_lines_into_df = function(lines, line_processor){
  dfs = lapply(1:length(lines),
               function(index){
                 processed_df = line_processor(lines[index])
                 if(nrow(processed_df) > 0){
                   processed_df$line_id = index
                 }
                 return(processed_df)
               })
  return(do.call(rbind, dfs))
}
