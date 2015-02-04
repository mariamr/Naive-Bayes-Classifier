train <- function(cat) {
  n = nrow(cat)
  nfeature = ncol(cat)
  catp = matrix(0, 5, nfeature)
  
  for (i in 1:5) {
    for (j in 1:nfeature) {   
      catp[i, j] = sum(cat[,j]==i)/n
    }
  }  
  return(catp)
}

test <- function (pr, pl, Xtest, Ytest, RPM, LPM) {
  n = nrow(Xtest)
  error = 0
  
  for (i in 1:n) {
    sr = score(pr, RPM, Xtest[i,])
    sl = score(pl, LPM, Xtest[i,])
    
    if (sr > sl && Ytest[i] != "R") {
      error = error + 1
    }
    
    else if (sr < sl && Ytest[i] != "L") {
      error = error + 1
    }
  }
  
  return ((n - error)*100/n) 
}

score <- function (pcategory, trainset, sample) { 
  p = pcategory 
  for (i in 1:length(sample)) {
    p = p * trainset[sample[1,i], i]
  }  
  return (p)
}


naivebayes <- function (data, ntimes) {
  # Read the data
  balance.scale.data <- read.csv(data, header=F)
  X = balance.scale.data[,2:5]
  Y = balance.scale.data[,1]
  
  # Clean up the data
  X = X[Y!="B",]
  Y = Y[Y!="B"] 
  # Set the number of training and test data
  n <- nrow(X)
  ntrain = round(0.9*n)
  accuracy = 0
  
  for (i in 1:ntimes) {
    # Form the training and test sets
    train.rows = sample(seq(n), ntrain)
    Xtrain = X[train.rows,]
    Ytrain = Y[train.rows]
    Xtest = X[-train.rows,]
    Ytest = Y[-train.rows]
    
    L = Xtrain[Ytrain=="L",]
    R = Xtrain[Ytrain=="R",]
    pr = nrow(R)/nrow(Xtrain)
    pl = nrow(L)/nrow(Xtrain)
    
    # Do training
    LPM = train(L)
    RPM = train(R)
    
    accuracy = accuracy + test(pr, pl, Xtest, Ytest, RPM, LPM)
    
  }
  return (accuracy/ntimes)
}

naivebayes ("/PATH_TO_INPUT_FILE/balance-scale.data.txt", 5)

