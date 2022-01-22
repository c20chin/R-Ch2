# Chapter 6 Functions


# ====================
# Exercise 6.1
# ====================
hello <- function(name) {
  cat("How are you doing,", name)
}
out <- capture.output(hello(name="Alvin"))
out
# ====================
# END
# ====================



# ====================
# Exercise 6.2
# ====================
hello2 <- function(name) {
  result <- paste0("How are you doing, ", name)
  return(result)
}
out <- hello2(name="Alvin")
out
# ====================
# END
# ====================



# ====================
# Exercise 6.3
# ====================
zigzag <- function(lap = 3, indent_max = 2) {
   tryCatch(
      expr = {
         lap_count = 0
         while (lap_count < lap){
            
            # Upper
            space <- 0
            for (i in 0:(indent_max - 1)) {
               for (j in 0:space) cat(" ")
               cat("***** ")
               cat("\n")
               space <- space + 2
               Sys.sleep(.3)
            }
            
            # Lower
            space = indent_max;
            for (i in (indent_max - 1):1) {
               for (j in 0:space) cat(" ")
               cat("***** ")
               cat("\n")
               space <- space - 1
               Sys.sleep(.3)
            }
            
            
            lap_count = lap_count + 2
         }
      },
      interrupt = function(e){ 
         print('Program interrupted!')
      }
   )
}

zigzag(lap = 3, indent_max = 10)
# ====================
# END
# ====================



# ====================
# Exercise 6.4
# ====================
wins <- 0
loses <- 0
ties <- 0


play_hand_game <- function(){
   #starting prompt
   cat("Rock, Paper, Sciccors Game")
   
   #player move input
   setNewMove <- function()
   { 
      move <- readline(prompt="Enter your move: (r)ock, (p)aper, (s)cissors or (q)uit:")
      if(!grepl("[rpsq]", move))
      {
         cat("Please follow the rule, and type in r, p, s, or q")
         return(setNewMove())
      }
      return(move) 
   }
   my_move <- setNewMove()

   
   #HANDLE PLAYER'S INPUT
   while (my_move != "q"){
   
      #rename or quit
      if (my_move == "r"){
         p_move <<- "Rock"
      }else if(my_move == "p"){
         p_move <<- "Paper"
      }else if(my_move == 's'){
         p_move <<- "Scissors"
      }
      
      cat(p_move,"vs. .....\n")
     
      
      #computer move
      c_num <- sample(1:3, size = 1)
      random_move <- function(){
        if (c_num == 1){
          return("Rock")
        }else if(c_num == 2){
          return("Paper")
        }else{
          return("Scissors")
        }
      }
      c_move <- random_move()
      
      
      #in the middle
      cat("Computer's Move is ...", c_move,"\n")
      
      #decision
      decision <- function(){
        if (c_move == p_move){
          outcome = "It's a tie!\n"
          ties <<- ties + 1
          
        } else if ((c_move == "Scissors" & p_move == "Paper") 
                   | (c_move == "Paper" & p_move == "Rock") 
                   | (c_move == "Rock" & p_move == "Scissors")){
          outcome = "You lost!\n"
          loses <<- loses + 1
          
         
        } else {
          outcome = "You win!\n"
          wins <<- wins + 1
          
        }
        
        cat(outcome)
        
      }
      
      decision()
      
      return(play_hand_game())
   }
   
   cat("Thank you for playing!\n")
   cat("*****GAME RESULTS*****\n")
   cat("Wins:", wins, ", Loses:", loses, ", Ties:", ties)
   
   
}

play_hand_game()
# ====================
# END
# ====================



