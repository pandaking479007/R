rm(list = ls())
library(ggplot2)
spin_the_wheel <- function(){
  step <- sample(x = 16,size = 1, pro =c(1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16, 1/16))
  return(step)
}

take_turn <- function(current_position){
  step <- spin_the_wheel()
  if(step <= 12){
      return(current_position + step)}
  else if(step == 13){
      current_position <- 115}
  else if(step == 14){
      current_position <- 81}
  else if(step == 15){
      current_position <- 51}
  else{
      current_position <- 35}
} 

simulate_game <- function(){
  current_position <- 0
  current_move <- 0
  while (current_position < 132){
    if(current_position == 9){
      current_position <- 45}
    else if(current_position == 20){
      current_position <- 34}
    else{
      current_position <- take_turn(current_position)}
    if (current_position == 70 | current_position == 106){
      current_position <- current_position
      current_move <- current_move + 1}
    current_move <- current_move + 1}
  return(current_move)}



simulate_games <- function(num_games, players){
  temp_df <- data.frame(matrix(data = replicate(num_games*players,simulate_game()), nrow = num_games, ncol = players))
  colnames(temp_df) <- gsub('X',replacement = "player", colnames(temp_df))
  temp_df$winner_moves <- apply(temp_df,1,min)
  
  cat('mean_winning', mean(temp_df$winner_moves),"\n\n")
  cat('max_winning',max(temp_df$winner_moves),"\n\n")
  cat('min_winning',min(temp_df$winner_moves),'\n\n')
 return(temp_df)
}

candyland_histogram <- function(dt) {
    ggplot(dt, aes(x = winner_moves)) +
    geom_histogram(binwidth = 5, na.rm = TRUE, color="black", fill="white")+
    xlim(c(0,150)) + ylim(c(0,200)) +  labs(title="Histogram for winner_moves") + 
    xlab('Winner_moves')+ylab ('Frequency of winner moves')
}

set.seed(1234)
sim1 <- simulate_games(1000,1)
sim2 <- simulate_games(1000,2)
sim3 <- simulate_games(1000,3)
sim4 <- simulate_games(1000,4)

candyland_histogram(sim1)
summary(sim1)
boxplot(sim1$winner_moves,xlab = "winner_moves",
        ylab = "Frequency of winner moves", main = "Boxplot for winner_moves",ylim=c(0,180))

