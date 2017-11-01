
four.in.a.row = function(player, v, debug=FALSE) {
  if (TRUE) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  if (length(v) < 4){
    return (FALSE)
  } else{
    count = 1
    i = 2
    while (i <= length(v)) {
      if (v[i] == v[i-1]){
        count = count +1
      }else count = 1
      if ((count >= 4) & (v[i] == player)){
        return(TRUE)
      }
      i = i + 1
    }
  }
  return(FALSE) # correct this return() statement
}

won = function(player, board, r, c, debug = FALSE) {
  if (TRUE) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  return(four.in.a.row(player, board[r,]) | four.in.a.row(player,board[,c]) | four.in.a.row(player, board[row(board) - col(board) == r - c]) | four.in.a.row(player, board[row(board) + col(board) == r + c]))
  return(FALSE) # correct this return() statement
}


largest.empty.row = function(board, col, debug = FALSE) {
  if (TRUE) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  v = board[,col]
  count = 0
  for (i in 1:6) {
    if (v[i] == "E")
      count = count + 1
  }
  return(count)
  return(6) # correct this return() statement
}

source("hw3test.R") # Run tests on the functions above.




nrow = 6
ncol = 7
x = rep(1:ncol, each = 6)
y = rep(1:nrow, times = 7)
plot(x, y, type="n", xlim=c(0, ncol + 1), ylim=c(nrow + 1, 0))
segments(x0=c(rep(0.5, nrow-1), seq(1.5, ncol-0.5, 1)),
         y0=c(seq(1.5, nrow-0.5, 1), rep(0.5, ncol-1)),
         x1=c(rep(ncol+0.5, nrow-1), seq(1.5, ncol-0.5, 1)),
         y1=c(seq(1.5, nrow-0.5, 1), rep(ncol+0.5, ncol-1)))
board = matrix(rep("E", 42), nrow = 6, ncol = 7)
player = "X"
for (j in 1:42) {
  if (player == "X") {
    repeat {
      index = identify(x, y, n = 1, plot = FALSE)
      col = x[index]
      row = y[largest.empty.row(board, col)]
      if (board[row, col] == "E") {
        break
      }
    }
  } else {
    board.vector = c(board)
    empty.indices = which(board.vector == "E")
    index = sample(x = empty.indices, size = 1)
    col = x[index]
    row = y[largest.empty.row(board, col)]
  }
  text(col, row, player)
  board[row, col] = player
  print(board)
  if (won(player, board, row, col)) {
    text(x=2, y=1/3, labels=paste(player, " won!"), col="red")
    break
  }
  player = ifelse(player == "X", "O", "X")
}



