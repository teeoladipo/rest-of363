get_letter_indices <- function(letter, word) {
  return(which(strsplit(word, "")[[1]] == letter))
}

play_hangman <- function(max_guesses, word) {
  guessed_letters <- character()
  unique_letters <- unique(strsplit(word, "")[[1]])
  
  while (max_guesses > 0) {
    guess <- readline("Enter a letter: ")
    indices <- get_letter_indices(guess, word)
    
    if (length(indices) > 0) {
      cat("The letter '", guess, "' is at: ", paste(indices, collapse = " "), "\n", sep = "")
    } else {
      cat("The letter isn't in the word.\n")
    }
    
    guessed_letters <- c(guessed_letters, guess)
    
    remaining_unique_letters <- setdiff(unique_letters, guessed_letters)
    
    if (length(remaining_unique_letters) == 0) {
      cat("You won!\n")
      break
    }
    
    max_guesses <- max_guesses - 1  
    
    if (max_guesses == 0) {
      cat("You lost.\n")
      break
    }
  }
}

# to play it
play_hangman(6, "grain")

