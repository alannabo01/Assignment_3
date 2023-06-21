# Function to read the word list from a file
readWordList <- function(file) {
  readLines(file)
}

# Function to choose a random word from the word list
chooseRandomWord <- function(wordList) {
  sample(wordList, 1)
}

# Function to check if the user input is a valid single character
isValidInput <- function(input) {
  nchar(input) == 1
}

# Function to update the secret word display with correctly guessed letters
updateSecretWord <- function(secretWord, guessedLetters) {
  displayWord <- secretWord
  
  for (letter in setdiff(unique(secretWord), guessedLetters)) {
    displayWord <- gsub(letter, "_", displayWord)
  }
  
  displayWord
}

# Function to check if the user has won the game
hasWonGame <- function(secretWord, guessedLetters) {
  all(setdiff(unique(secretWord), guessedLetters) == "_")
}

# Function to play the Hangman game
playHangman <- function(wordList, maxTries) {
  secretWord <- chooseRandomWord(wordList)
  guessedLetters <- character(0)
  remainingTries <- maxTries
  
  cat("Welcome to Hangman!\n")
  cat("The category is: Wild Cats\n")
  cat("The secret word has", nchar(secretWord), "letters.\n")
  
  while (remainingTries > 0) {
    cat("\n")
    cat("Secret word:", updateSecretWord(secretWord, guessedLetters), "\n")
    cat("Remaining tries:", remainingTries, "\n")
    
    userInput <- readline("Enter a letter (or type 'guess' to guess the whole word): ")
    userInput <- tolower(userInput)
    
    if (userInput == "guess") {
      guessedWord <- readline("Enter your guess for the whole word: ")
      guessedWord <- tolower(guessedWord)
      
      if (guessedWord == secretWord) {
        cat("Congratulations! You guessed the correct word:", secretWord, "\n")
        return()
      } else {
        cat("Sorry, your guess is incorrect.\n")
        remainingTries <- remainingTries - 1
      }
    } else if (isValidInput(userInput)) {
      if (userInput %in% guessedLetters) {
        cat("You've already guessed that letter. Try again.\n")
      } else if (userInput %in% secretWord) {
        cat("Good guess! The letter is in the secret word.\n")
        guessedLetters <- c(guessedLetters, userInput)
        
        if (hasWonGame(secretWord, guessedLetters)) {
          cat("Congratulations! You guessed the correct word:", secretWord, "\n")
          return()
        }
      } else {
        cat("Sorry, the letter is not in the secret word.\n")
        remainingTries <- remainingTries - 1
      }
    } else {
      cat("Invalid input. Please enter a single letter.\n")
    }
  }
  
  cat("Sorry, you've run out of tries. The secret word was:", secretWord, "\n")
}

# Main program

# Read the word list from a file (assuming words.txt is the file name)
wordList <- readWordList("word_list.txt")

# Set the maximum number of tries
maxTries <- 6

# Play the Hangman game
playHangman(wordList, maxTries)
