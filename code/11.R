input_lines = readLines('./inputs/11.txt')

numbers = unlist(strsplit(input_lines, ' '))

library(gmp)

reduce_number = function(number_string){
  number_string_1 = paste0(trimws(substr(number_string, 1, nchar(number_string) - 1), which = 'left', whitespace = '0'), 
                           substr(number_string, nchar(number_string), nchar(number_string)))
  as.character(as.bigz(number_string_1))
}

evolve_number = function(number){
  if(number == '0'){
    return('1')
  } else if(nchar(number) %% 2 == 0){
    return(c(reduce_number(substr(number, 1, nchar(number)/2)),
             reduce_number(substr(number, nchar(number)/2 + 1, nchar(number)))))
  } else {
    return(as.character(as.bigz(number) * 2024))
  }
}

lapply(numbers, evolve_number)

evolve_list = function(character_vector){
  unlist(lapply(character_vector, evolve_number))
}

current_numbers = numbers

for(i in 1:25){
  current_numbers = evolve_list(current_numbers)
}

length(current_numbers)

current_numbers_backup = current_numbers

current_numbers = '1'
for(i in 1:25){
  current_numbers = evolve_list_memoised(current_numbers)
}


evolve_number_memoised = Vectorize(memoise::memoise(evolve_number))
# evolve_list_memoised = function(character_vector){
#   unlist(lapply(character_vector, evolve_number_memoised))
# }
# 
# length_after_25_runs = memoise::memoise(function(number_string){
#   current_numbers = number_string
#   for(i in 1:25){
#     current_numbers = evolve_list_memoised(current_numbers)
#   }
#   return(length(current_numbers))
# })
# 
# length_after_50_runs = memoise::memoise(function(number_string){
#   current_numbers = number_string
#   for(i in 1:25){
#     current_numbers = evolve_list_memoised(current_numbers)
#   }
#   return(sum(sapply(current_numbers, length_after_25_runs)))
# })
# 
# system.time(x <- length_after_25_runs('1'))
# system.time(x <- length_after_50_runs('1'))

numbers_df = as.data.frame(table(numbers)) %>% mutate(numbers = as.character(numbers))

evolve_df = function(numbers_df){
  numbers_df %>% mutate(new_numbers = evolve_number_memoised(numbers)) %>% unnest(new_numbers) %>% transmute(numbers = new_numbers, Freq) %>% group_by(numbers) %>% summarise(Freq = sum(Freq))
}

current_numbers_df = numbers_df

for(i in 1:25){
  current_numbers_df = evolve_df(current_numbers_df)
}

# run three times

options(scipen = 999)
sum(current_numbers_df$Freq) #248967696501656