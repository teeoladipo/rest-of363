word <- "grain"

get_letter_indices <- function(letter, word) {
  indices <- which(strsplit(word, "")[[1]] == letter)
  if (length(indices) > 0) {
    return(indices)
  } else {
    return(integer(0))
  }
}

play_hangman <- function(max_guesses) {
  guessed_letters <- character()
  unique_letters <- unique(strsplit(word, "")[[1]])
  
  while (max_guesses > 0) {
    guess <- readline("Enter a letter: ")
    indices <- get_letter_indices(guess, word)
    
    if (length(indices) > 0) {
      print(paste("The letter '", guess, "' IS at: ",paste(indices, collapse = " ")))
    } else {
      print(paste("The letter isn't in the word."))
      max_guesses <- max_guesses - 1
    }
    
    guessed_letters <- c(guessed_letters, guess)
    
    if (length(unique(guessed_letters)) == length(unique_letters)) {
      print("You won!")
      break
    } else if (max_guesses == 0) {
      print("You lost.")
      break
    }
  }
}

# to play it
play_hangman(6)
