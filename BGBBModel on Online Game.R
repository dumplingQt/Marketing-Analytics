## It is a Very small practice on BGBB Model
## The reason why I choose online games is that it is a typical example of non-contractual relationship

transaction_data <- as.data.frame(
  read_excel("/game_transactions.xlsx", sheet=1))
colnames(transaction_data)

#Total number of purchases:
N_purchase = nrow(transaction_data)

#The total number of unique users:
N_UniUser = length(unique(transaction_data$uid))
