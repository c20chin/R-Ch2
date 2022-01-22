# Chapter 5 Conditions and Loops


# ====================
# Exercise 5.1
# ====================
input <- stringr::sentences

for (i in 1:length(input)){
  char_num <- nchar(input)
}

df_sent <- data.frame(
  "ID" = c(1:length(input)),
  "Text" = input,
  "nChar" = char_num
)
# ====================
# END
# ====================



# ====================
# Exercise 5.2
# ====================
input_fru <- stringr::fruit

for (i in input_fru){
  
  if (substr(i,1,1) == "a" | substr(i,1,1) =="e" | substr(i,1,1) =="i" | substr(i,1,1) =="o" | substr(i,1,1) =="u"){
    print(i)
  }
}
# ====================
# END
# ====================



# ====================
# Exercise 5.3
# ====================

# The solution is to add a if-else loop outside of the while-loop

ans <- 90
guess <- 95
if (guess > ans){
  while(guess != ans){
    cat("Your `guess` is too large! \nThe system will take care for you!\n")
    guess <- guess - 1
    cat("Now the system is adjusting your `guess` to ", guess, "\n\n")
  }
}else if (guess < ans){
  while(guess != ans){
    cat("Your `guess` is too small! \nThe system will take care for you!\n")
    guess <- guess + 1
    cat("Now the system is adjusting your `guess` to ", guess, "\n\n")
  }
}else{
  "You've got it right!"
}

# ====================
# END
# ====================



# ====================
# Exercise 5.4
# ====================

setNewGuess <- function()
{ 
  guessn <- readline(prompt="Please guess my number(0~100):")
  if(!grepl("^[0-9]+$",guessn))
  {
    cat("<WARNING>Please behave. Enter an INTEGER!!!<WARNING>")
    return(setNewGuess())
  }
  return(as.numeric(guessn)) 
}


guessMyNumber <- function(){
  ## Randomly select an integer from 0 to 100
  answer <- sample(1:100, size = 1)
  
  ## Instructions for user
  print("Now I am thinking of a number between 0 and 100.")
  
  
  
  
  ## As long as user's guess is not the answer
  while(guessn != answer){
    guessn <- setNewGuess()
    ## if user's guess is smaller than the answer
    if(guessn < answer){
      writeLines("The answer is HIGHER.")
      
      ## if user's guess is larger than the answer
    }else{
      writeLines("The answer is LOWER")
      
    }
  }
  
  ## Exit the While-loop
  writeLines(paste0("Good Job! You had the correct answer! My number is ", guessn))
} # endfunc

guessMyNumber()
# ====================
# END
# ====================



