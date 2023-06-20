# Hangman Game

# Function to read word list from file
readWordList <- function(file) {
  word_list <- readLines(file)
  return(word_list)
}

# Function to choose a random word from the list
chooseRandomWord <- function(word_list) {
  random_word <- sample(word_list, 1)
  return(random_word)
}

# Function to check if the character is a letter
isLetter <- function(char) {
  return(grepl("[A-Za-z]", char))
}

# Function to update the hidden word with correctly guessed letters
updateHiddenWord <- function(secret_word, hidden_word, letter) {
  for (i in 1:length(secret_word)) {
    if (tolower(secret_word[i]) == tolower(letter)) {
      hidden_word[i] <- secret_word[i]
    }
  }
  return(hidden_word)
}

# Function to display the hangman progress
displayProgress <- function(hidden_word) {
  progress <- paste(hidden_word, collapse = " ")
  cat(progress, "\n")
}

# Function to play the hangman game
playHangman <- function() {
  word_list <- readWordList("word_list.txt")
  secret_word <- chooseRandomWord(word_list)
  hidden_word <- strsplit(secret_word, "")[[1]]
  hidden_word[hidden_word != " "] <- "_"
  
  max_tries <- 6  # Maximum number of wrong guesses allowed
  wrong_guesses <- 0
  guessed_letters <- c()
  
  cat("Welcome to Hangman!\n")
  cat("The category is: Wild Cats.\n")
  cat("The secret word has", nchar(secret_word), "letters.\n")
  cat("You have", max_tries, "tries to guess the word.\n")
  
  while (wrong_guesses < max_tries) {
    cat("\n")
    displayProgress(hidden_word)
    cat("Guessed letters:", paste(guessed_letters, collapse = " "), "\n")
    letter <- tolower(readline("Enter a letter (or 'quit' to exit): "))
    
    if (letter == "quit") {
      cat("You have quit the game. The secret word was:", secret_word, "\n")
      return()
    }
    
    if (nchar(letter) != 1 || !isLetter(letter)) {
      cat("Please enter a single letter.\n")
      next
    }
    
    if (tolower(letter) %in% tolower(secret_word)) {
      hidden_word <- updateHiddenWord(secret_word, hidden_word, letter)
      if (all(hidden_word != "_")) {
        cat("Congratulations! You have guessed the word:", secret_word, "\n")
        return()
      }
    } else {
      wrong_guesses <- wrong_guesses + 1
      cat("Wrong guess! Tries remaining:", max_tries - wrong_guesses, "\n")
      if (wrong_guesses == max_tries) {
        cat("Game over! You have run out of tries. The secret word was:", secret_word, "\n")
        return()
      }
    }
    
    guessed_letters <- c(guessed_letters, letter)
  }
}

# Start the game
playHangman()
