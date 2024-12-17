input_lines = readLines('./inputs/17.txt')

instructions = unlist(strsplit(input_lines[5], ': '))[2]

instructions_vector = as.numeric(unlist(strsplit(instructions, ',')))

#instructions_vector = c(0,1,5,4,3,0)

instructions_length = length(instructions_vector)

starting_registers = as.bigz(split_vectorised(input_lines[1:3], ': ', 2))

#starting_registers = as.bigz(c(729, 0, 0))

bitwise_xor_bigz = function(a, b){
  z = as.bigz(a)
  w = as.bigz(b)
  # https://stackoverflow.com/a/50077593/11732165
  z1 <- as.numeric(charToRaw(as.character(z, b=2)))-48
  w1 <- as.numeric(charToRaw(as.character(w, b=2)))-48
  mx <- max(length(z1), length(w1))
  z1 <- c(rep(0, mx-length(z1)), z1)
  w1 <- c(rep(0, mx-length(w1)), w1)
  zxorw <- as.bigz(paste0("0b", rawToChar(as.raw(1*(xor(z1,w1)) + 48))))
  return(zxorw)
}

interpret_combo = function(number, registers){
  if(number %in% 0:3){
    return(number)
  } else if(number %in% 4:6){
    return(registers[number - 3])
  } else {
    stop('combo number not valid')
  }
}

instructions = function(instruction, number, registers){
  #print(instruction)
  if(instruction == 0){
    dividend = as.bigz(2)^interpret_combo(number, registers)
    # divide register A by 2^combo and write to register A
    registers[1] = registers[1] %/% dividend
  } else if (instruction == 1){
    registers[2] = bitwise_xor_bigz(registers[2], as.bigz(number))
  } else if (instruction == 2){
    combo_operand = interpret_combo(number, registers)
    registers[2] = combo_operand %% 8
  } else if (instruction == 3){
    if(registers[1] != 0){
      return(list(jump = number))
    }
  } else if (instruction == 4){
    registers[2] = bitwise_xor_bigz(registers[2], registers[3])
  } else if (instruction == 5){
    combo_operand = interpret_combo(number, registers)
    return(list(output = combo_operand %% 8))
  } else if (instruction == 6){
    dividend = as.bigz(2)^interpret_combo(number, registers)
    # divide register A by 2^combo and write to register A
    registers[2] = registers[1] %/% dividend
  } else if (instruction == 7){
    dividend = as.bigz(2)^interpret_combo(number, registers)
    # divide register A by 2^combo and write to register A
    registers[3] = registers[1] %/% dividend
  }
  return(list(registers = registers))
}

simulate_step = function(position, registers){
  stopstatus = FALSE
  instruction_result = instructions(instruction = instructions_vector[position + 1], 
                                    number = instructions_vector[position + 2], registers = registers)
  output = NULL
  new_position = position + 2
  new_registers = registers
  if('jump' %in% names(instruction_result)){
    new_position = instruction_result$jump
  } else if('output' %in% names(instruction_result)){
    output = instruction_result$output
  } else {
    new_registers = instruction_result$registers
  }
  if(new_position > instructions_length - 2){
    stopstatus = TRUE
  } 
  return(list(new_position = new_position, output = output, new_registers = new_registers, stopstatus = stopstatus))
}

output = as.bigz(numeric(0))

registers = starting_registers

stopstatus = FALSE

position = 0

step_number = 0

while(stopstatus == FALSE){
  step_number = step_number + 1
  simulation_step = simulate_step(position, registers)
  position = simulation_step$new_position
  output = c(output, simulation_step$output)
  registers = simulation_step$new_registers
  stopstatus = simulation_step$stopstatus
}

paste0(as.character(output), collapse = ',')

get_output = function(register_a){
  output = as.bigz(numeric(0))
  
  registers = c(as.bigz(register_a), 0, 0)
  
  stopstatus = FALSE
  
  position = 0
  
  step_number = 0
  
  while(stopstatus == FALSE){
    step_number = step_number + 1
    simulation_step = simulate_step(position, registers)
    position = simulation_step$new_position
    output = c(output, simulation_step$output)
    registers = simulation_step$new_registers
    stopstatus = simulation_step$stopstatus
    if(step_number %% 8 == 0){
      #print(step_number)
      #print(output)
      #print(registers)
    }
  }
  
  #print(step_number)
  #print(paste0(as.character(output), collapse = ','))
  return(paste0(as.character(output), collapse = ','))
}

# So there is a loop of 8 steps
# 1. A mod 8 -> B
# 2. B XOR 1 -> B
# 3. A / (2^B) -> C
# 4. B XOR B -> B [B = 0]
# 5. B XOR C -> B [C -> B]
# 6. out B mod 8
# 7. A / 8 -> A
# 8. Jump to 0 unless A = 0

Vectorize(get_output)(as.bigz(0:7))

test_get_output = function(candidates, desired_output){
  outputs = Vectorize(get_output)(candidates)
  suitable_candidates = candidates[outputs == desired_output]
  return(suitable_candidates)
}

get_next_candidates = function(candidates){
  return(rep(candidates, each = 8) * 8 + rep(as.bigz(0:7), times = length(candidates)))
}

initial_candidates = as.bigz(0)

passing_candidates = initial_candidates

for(instructions_to_succeed_in in 1:instructions_length){
  valid_output = paste0(instructions_vector[(instructions_length - instructions_to_succeed_in + 1):instructions_length], 
               collapse = ',')
  print(valid_output)
  next_candidates = get_next_candidates(passing_candidates)
  passing_candidates = test_get_output(next_candidates, valid_output)
  print(passing_candidates)
}

print(min(passing_candidates)) #164278899142333
