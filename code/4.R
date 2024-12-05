input_lines = readLines('./inputs/4.txt')

input_grid = lines_to_matrix(input_lines)

line_to_df = function(line){
  line_vector = unlist(strsplit(line, ''))
  df = data.frame(column = 1:length(line_vector), character = line_vector)
}

input_as_df_list = lapply(1:length(input_lines),
                          function(line_number){
                            single_dataframe = line_to_df(input_lines[line_number])
                            single_dataframe['row'] = line_number
                            return(single_dataframe)
                          })

input_df = do.call(rbind, input_as_df_list)

xmas_locations = sqldf(
  "select x_location.column, x_location.row, m_location.column - x_location.column as column_offset, m_location.row - x_location.row as row_offset
  from input_df x_location
  inner join input_df m_location on x_location.character = 'X' and m_location.character = 'M' 
    and x_location.column - m_location.column <= 1 and x_location.column - m_location.column >= -1
    and x_location.row - m_location.row <= 1 and x_location.row - m_location.row >= -1
  inner join input_df a_location on a_location.character = 'A' 
    and m_location.column - a_location.column = x_location.column - m_location.column
    and m_location.row - a_location.row = x_location.row - m_location.row
  inner join input_df s_location on s_location.character = 'S' 
    and a_location.column - s_location.column = x_location.column - m_location.column
    and a_location.row - s_location.row = x_location.row - m_location.row
    "
)

nrow(xmas_locations) #2560

a_locations = input_df[input_df$character == 'A',]
m_locations = input_df[input_df$character == 'M',]
s_locations = input_df[input_df$character == 'S',]

x_mas_locations = sqldf(
  "
  select a_location.column, a_location.row
  from a_locations a_location
  inner join m_locations m_location_1 on 
    a_location.column - m_location_1.column in (-1, 1) and a_location.row - m_location_1.row in (-1, 1)
  inner join m_locations m_location_2 on
    a_location.column - m_location_2.column in (-1, 1) and a_location.row - m_location_2.row in (-1, 1)
    and (m_location_1.column = m_location_2.column or m_location_1.row = m_location_2.row)
    and (not (m_location_1.column = m_location_2.column and m_location_1.row = m_location_2.row))
  inner join s_locations s_location_1 on 
    a_location.column - s_location_1.column in (-1, 1) and a_location.row - s_location_1.row in (-1, 1)
  inner join s_locations s_location_2 on
    a_location.column - s_location_2.column in (-1, 1) and a_location.row - s_location_2.row in (-1, 1)
    and (s_location_1.column = s_location_2.column or s_location_1.row = s_location_2.row)
    and (not (s_location_1.column = s_location_2.column and s_location_1.row = s_location_2.row))
  "
)

nrow(distinct(x_mas_locations)) #1910