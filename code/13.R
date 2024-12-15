input_lines = readLines('./inputs/13.txt')

records = (length(input_lines) + 1)/4

machine_data = data.frame(id = 1:records,
                          ax = split_vectorised(input_lines[(1:records) * 4 - 3],
                                                '\\+|,', 2),
                          ay = split_vectorised(input_lines[(1:records) * 4 - 3],
                                                '\\+|,', 4),
                          bx = split_vectorised(input_lines[(1:records) * 4 - 2],
                                                '\\+|,', 2),
                          by = split_vectorised(input_lines[(1:records) * 4 - 2],
                                                '\\+|,', 4),
                          targetx = split_vectorised(input_lines[(1:records) * 4 - 1],
                                                '=|,', 2),
                          targety = split_vectorised(input_lines[(1:records) * 4 - 1],
                                                '=|,', 4)
) %>%
  mutate(ax = as.numeric(ax), ay = as.numeric(ay),
         bx = as.numeric(bx), by = as.numeric(by),
         targetx = as.numeric(targetx), targety = as.numeric(targety))

collinear_records = machine_data %>% filter(((by / bx) == (ay/ax)) | (bx == 0 & ax == 0)) # 0 records, so solutions are unique
machine_data %>% filter((ax == 0) | (ay == 0) | (bx == 0) | (by == 0)) # all are nonzero

# u * ax + v * bx = targetx, u * ay + v * by = targety
# by * u * ax + v * bx * by = targetx * by, bx * u * ay + v * bx * by = targety * bx
# u * (by * ax - bx * ay) = (targetx * by - targety * bx)

machine_data_solution = machine_data %>%
  mutate(u = (targetx * by - targety * bx) / (by * ax - bx * ay)) %>%
  mutate(v = (targetx - u * ax)/bx)

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

viable_solutions = machine_data_solution %>% filter(u <= 100 & u >= 0 & v <= 100 & v >= 0) %>%
  filter(is.wholenumber(u), is.wholenumber(v))

viable_solutions %>% mutate(cost = u * 3 + v) %>% pull(cost) %>% sum() #33209

machine_data_corrected = machine_data %>% 
  mutate(targetx = targetx + 10000000000000,
         targety = targety + 10000000000000)

machine_data_corrected_solution = machine_data_corrected %>%
  mutate(u = (targetx * by - targety * bx) / (by * ax - bx * ay)) %>%
  mutate(v = (targetx - u * ax)/bx)

viable_corrected_solutions = machine_data_corrected_solution %>%
  filter(is.wholenumber(u), is.wholenumber(v)) 
  
options(scipen = 999)
viable_corrected_solutions %>% mutate(cost = u * 3 + v) %>% pull(cost) %>% sum() %>% print #83102355665474
