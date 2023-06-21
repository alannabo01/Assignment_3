# Hangman Game

# Load required packages
library(stringr)

# Read the word list from the file
word_list <- readLines("word_list.txt")

# Check if the word list is empty
if (length(word_list) == 0) {
  stop("Word list is empty. Please populate the 'word_list.txt' file with words.")
}

# Choose a random word from the list
secret_word <- sample(word_list, 1)

# Count the number of characters in the secret word
word_length <- nchar(secret_word)

# Set the number of maximum wrong guesses
max_wrong_guesses <- 6

# Initialize variables
guessed_letters <- character(0)
wrong_guesses <- 0

# Display initial information to the user
cat("Welcome to Hangman!\n")
cat("The secret word has", word_length, "letters.\n")

# Game loop
while (TRUE) {
  # Ask for user input
  guess <- readline("Enter a letter or the full word: ")
  
  # Validate user input
  if (str_detect(guess, "[^a-zA-Z]")) {
    cat("Invalid input. Please enter a single letter or the full word.\n")
    next
  }
  
  # Convert the guess to lowercase
  guess <- tolower(guess)
  
  # Check if the guess is a single letter or the full word
  if (nchar(guess) == 1) {
    # Check if the letter has already been guessed
    if (guess %in% guessed_letters) {
      cat("You have already guessed that letter. Try again.\n")
      next
    }
    
    # Add the guessed letter to the list
    guessed_letters <- c(guessed_letters, guess)
    
    # Check if the guessed letter is in the secret word
    if (str_detect(secret_word, guess)) {
      cat("Correct guess!\n")
    } else {
      cat("Wrong guess!\n")
      wrong_guesses <- wrong_guesses + 1
    }
  } else {
    # Check if the full word guess is correct
    if (guess == secret_word) {
      cat("Congratulations! You guessed the word correctly.\n")
      break
    } else {
      cat("Wrong guess!\n")
      wrong_guesses <- wrong_guesses + 1
    }
  }
  
  # Display the current state of the secret word
  current_state <- str_replace_all(secret_word, paste0("[^", guessed_letters, "]"), "_")
  cat("Current state:", paste(current_state, collapse = " "), "\n")
  
  # Check if the user has won or lost
  if (all(str_split(current_state, "")[[1]] == str_split(secret_word, "")[[1]])) {
    cat("Congratulations! You guessed the word correctly.\n")
    break
  } else if (wrong_guesses == max_wrong_guesses) {
    cat("Sorry, you lost. The secret word was:", secret_word, "\n")
    break
  }
  
  # Display remaining tries
  remaining_tries <- max_wrong_guesses - wrong_guesses
  cat("Remaining tries:", remaining_tries, "\n")
}

# End of the game

