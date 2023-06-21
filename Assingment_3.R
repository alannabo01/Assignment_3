# Hangman Game

# Loading required packages
library(stringr)

# Reading the word list from the file
word_list <- readLines("word_list.txt")

# Checking if the word list is empty
if (length(word_list) == 0) {
  stop("Word list is empty. Please populate the 'word_list.txt' file with words.")
}

# Choosing a random word from the list
secret_word <- sample(word_list, 1)

# Counting the number of characters in the secret word
word_length <- nchar(secret_word)

# Setting the number of maximum wrong guesses
max_wrong_guesses <- 6

# Initializing variables
guessed_letters <- character(0)
wrong_guesses <- 0

# Displaying initial information to the user
cat("Welcome to Hangman!\n")
cat("The category is: Wild Cats\n")
cat("The secret word has", word_length, "letters.\n")

# Creating the game loop
while (TRUE) {
  # Ask for user input
  guess <- readline("Enter a letter or the full word: ")
  
  # Validating user input
  if (str_detect(guess, "[^a-zA-Z]") || nchar(guess) != 1) {
    cat("Invalid input. Please enter a single letter.\n")
    next
  }
  
  # Converting the guess to lowercase
  guess <- tolower(guess)
  
  # Checking if the guess is a single letter or the full word
  if (nchar(guess) == 1) {
    # Checking if the letter has already been guessed
    if (guess %in% guessed_letters) {
      cat("You have already guessed that letter. Try again.\n")
      next
    }
    
    # Adding the guessed letter to the list
    guessed_letters <- c(guessed_letters, guess)
    
    # Checking if the guessed letter is in the secret word
    if (str_detect(secret_word, guess)) {
      cat("Correct guess!\n")
    } else {
      cat("Wrong guess!\n")
      wrong_guesses <- wrong_guesses + 1
    }
  } else {
    # Checking if the full word guess is correct
    if (guess == secret_word) {
      cat("Congratulations! You guessed the word correctly.\n")
      break
    } else {
      cat("Wrong guess!\n")
      wrong_guesses <- wrong_guesses + 1
    }
  }
  
  # Displaying the current state of the secret word
  current_state <- str_replace_all(secret_word, paste0("[^", paste(guessed_letters, collapse = ""), " ]"), "_")
  cat("Current state:", current_state, "\n")
  
  # Checking if the user has won or lost
  if (all(str_split(current_state, "")[[1]] == str_split(secret_word, "")[[1]])) {
    cat("Congratulations! You guessed the word correctly.\n")
    break
  } else if (wrong_guesses == max_wrong_guesses) {
    cat("Sorry, you lost. The secret word was:", secret_word, "\n")
    break
  }
  
  # Displaying remaining tries
  remaining_tries <- max_wrong_guesses - wrong_guesses
  cat("Remaining tries:", remaining_tries, "\n")
}

# End of the game


