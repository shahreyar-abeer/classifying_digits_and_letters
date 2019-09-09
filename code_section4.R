


# function that simulates the model of 4.4
sim = function(n, times){
  set.seed(100)
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('iteration', 'accuracy')
  for( i in 1:times){
    correct = 0
    for(j in 1:n){
      character = sample(c(1, 2), 1)
      prediction = sample(c(1, 2), 1)
      if (character == prediction)
        correct = correct + 1
    }
    out_data[i, 1] = i
    out_data[i, 2] = correct/n
  }
  return(out_data)
}


# function that scales the features
feature_scale = function(feature){
  scaled = (feature - mean(feature))/ sd(feature)
  return(scaled)
}






