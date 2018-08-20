# Create a deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)

# Probability of drawing a king

kings <- paste("King", suits)
mean(deck %in% kings)

# Conditional probability of the second draw being a king if the first draw is a king as well
# Permutations lists all permutations if order matters
  library(gtools)
  #Compute all possible hands if the order matters
  hands <- permutations(52,2, v=deck)
  first_card <- hands[,1]
  second_card <- hands[,2]
  
  sum(first_card %in% kings)
  
  sum(first_card %in% kings & second_card %in% kings)/
    sum(first_card %in% kings)
  
  #combination function lists all combinations if order does not matter
  
  # What's the probabiliy of getting a blackjack?
  
  aces <- paste("Ace", suits)
  facecard <- c("King", "Queen", "Jack", "Ten")  
  facecard <- expand.grid(number = facecard, suit=suits)  
  facecard <- paste(facecard$number,facecard$suit)
  
  hands <- combinations(52,2,v=deck)  
  
  mean(hands[,1] %in% aces & hands[,2] %in% facecard)
  
  # we can use Monte Carlo
  B <- 1000000
  
  results <- replicate(B, {
    hand <- sample(deck,2)
    (hand[1] %in% aces & hand[2] %in% facecard) |
      (hand[2] %in% aces & hand[1] %in% facecard)
  })
  mean(results)
  