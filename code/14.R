input_lines = readLines('./inputs/14.txt')

width = 101
height = 103

robot_data = data.frame(id = 1:length(input_lines),
                        px = as.numeric(split_vectorised(input_lines, '=|,| ', 2)),
                        py = as.numeric(split_vectorised(input_lines, '=|,| ', 3)),
                        vx = as.numeric(split_vectorised(input_lines, '=|,| ', 5)),
                        vy = as.numeric(split_vectorised(input_lines, '=|,| ', 6))
)

new_position = function(start, velocity, limit, steps){
  (start + velocity * steps) %% limit
}

after_100 = robot_data %>% mutate(new_px = new_position(px, vx, width, 100),
                                  new_py = new_position(py, vy, height, 100)) %>%
  mutate(quadrant_x = sign(new_px - (width - 1)/2),
         quadrant_y = sign(new_py - (height - 1)/2))

after_100 %>% count(quadrant_x, quadrant_y) %>%
  filter(quadrant_x != 0, quadrant_y != 0) %>%
  pull(n) %>% prod #225552000

display_state = function(time){
  new_state = robot_data %>% mutate(new_px = new_position(px, vx, width, time),
                                    new_py = new_position(py, vy, height, time))
  ggplot(new_state, aes(x = new_px, y = new_py)) +
    geom_point() +
    xlim(c(0, width)) +
    ylim(c(0, height))
  }


library(shiny)

ui = fluidPage(
  numericInput('steps', 'steps', 0, min = 1, step = 1),
  plotOutput('pattern'),
)

server = function(input, output){
  output$pattern = renderPlot({
    display_state(input$steps)
  })
}

shinyApp(ui, server)



ui = fluidPage(
  sliderInput('stepsix', 'steps option', 1, min = 1, max = length(interesting_times), step = 1),
  plotOutput('pattern'),
  textOutput('steps')
)

server = function(input, output){
  output$pattern = renderPlot({
    display_state(interesting_times[input$stepsix])
  })
  output$steps = renderText({
    interesting_times[input$stepsix]
  })
}

shinyApp(ui, server)

# look for when x 0-25 and y 0-25 are both empty?
interesting_times = numeric(0)

for(time in 1:(width * height)){
  new_state = robot_data %>% mutate(new_px = new_position(px, vx, width, time),
                                    new_py = new_position(py, vy, height, time))
  new_px_distribution = table(new_state$new_px %/% 26)
  if(max(new_px_distribution) - min(new_px_distribution) > 60){
    interesting_times = c(interesting_times, time)
  }
}

#7371