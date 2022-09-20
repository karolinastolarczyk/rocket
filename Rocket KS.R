# Bibiloteki
# rm(list=ls()) 
library("geometry")
library("dplyr")
library("rJava")
library("RWeka")
library("stringr")
library("glmnet")
library("foreach")
library("doParallel")

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_331') 

#ustawiamy obliczenia r�wnoleg�e
myCluster <- makeCluster(7, # number of cores to use
                         type = "PSOCK")



#Wybieramy Dane
problem <- 'Epilepsy'
#problem <- 'AtrialFibrillation'
#path <- 'F:/ADwR/Rocket/Multivariate_arff/'
path <- 'D:/WZTUM/Multivariate_arff/'


#W folderze byla tabelka z informacjami o wymiarach danych
DataDimensions <- read.csv(str_c(path,'DataDimensions_.csv'),header = TRUE, sep = ,)
DataFeatures <- DataDimensions[DataDimensions$Problem == problem,]
NumDimensions <- DataFeatures$NumDimensions
TrainSize <- DataFeatures$TrainSize
TestSize <- DataFeatures$TestSize

#Pobieramy dane testowe i treningowe dla wszystkich wymiarow
Data_train <- list()
Data_test <- list()

for (i in 1: NumDimensions)
{
  #Test
  path_test <- str_c(path, problem, '/',problem, 'Dimension', i, '_TEST.arff')
  Data_test_ <-read.arff(path_test)
  if(i==1){
    for(j in 1:TestSize){
      names(Data_test_)[ncol(Data_test_)] <- 'target'
      columns_names_test <- colnames(Data_test_)
      Data_classes = distinct(Data_test_,target)[[1]]
      Data_test[[j]] <- rbind(Data_test_  %>% filter(row(Data_test_) == j))
    }
    
  } else {
    colnames(Data_test_) <- columns_names_test
    for(j in 1:TestSize){
      Data_test[[j]] <- rbind(Data_test[[j]],Data_test_  %>% filter(row(Data_test_) == j))
    }
  }
  
  #Train
  path_train <- str_c(path, problem, '/',problem, 'Dimension', i, '_TRAIN.arff')
  Data_train_ <-read.arff(path_train)
  if(i==1){
    for(j in 1:TrainSize){
      names(Data_train_)[ncol(Data_train_)] <- 'target'
      columns_names_train <- colnames(Data_train_)
      Data_train[[j]] <- rbind(Data_train_  %>% filter(row(Data_train_) == j))
    }
    
  } else {
    colnames(Data_train_) <- columns_names_train
    for(j in 1:TrainSize){
      Data_train[[j]] <- rbind(Data_train[[j]],Data_train_  %>% filter(row(Data_train_) == j))
    }
  }
}


cut_time_series <- function(lenght, number){
  ans <- integer(number)
  temp <- sample(number, number)
  for ( j in 1 : floor(number/3) ){
    ans[temp[j]]=max(lenght - floor(runif(1)*0.3*lenght),22)
  }
  for ( j in (floor(number/3)+1):floor(number*2/3) ){
    ans[temp[j]]=max(lenght - floor(runif(1)*0.3*lenght+lenght*30/100),22)
  }
  for ( j in (floor(number*2/3)+1):number ){
    ans[temp[j]]=max(lenght - floor(runif(1)*0.3*lenght+lenght*60/100),22)
  }
  return(ans)
}

cut_time_series3 <- function(dane, classes){
  for(i in 1:length(classes)){
    temp <- 0
    for(j in 1:length(dane)){
      if(dane[[j]]['target'][[1]][1]==classes[i]){
        temp <- temp + 1
      }
    }
    temp2 <- cut_time_series(lengths(dane)[1],temp)
    l <- 1
    for ( j in 1 : length(dane)){
      if(dane[[j]]['target'][[1]][1]==classes[i]){
        dane[[j]] <- dane[[j]] %>% select(1 : temp2[l],'target')
        l <- l+1
      }
    }
    
  }
  return(dane)
}

################### wyrownywanie szeregow ########################

series_features <- function(v_data, v_size){
  if((v_size %% 2) == 0) {
    size <- v_size-1
  } else {size <- v_size}
  
  test_l <- rep(0,size)
  for (i in 1:size) {
    test_l[i] <- length(v_data[[i]])
  }
  # Nasza dlugosc
  median <- median(sort(test_l))
  min <- min(sort(test_l))
  max <- max(sort(test_l))
  which_median <- which(test_l == median)[1]
  which_min <- which(test_l == min)[1]
  which_max <- which(test_l == max)[1]
  return(list(median,which_median, min, which_min, max, which_max))
}


series_alignment <- function(v_data, v_date_train, v_size, v_size_train, v_len, v_padding){
  
  series_f <- series_features(v_date_train, v_size_train)
  #teraz albo przycinamy szeregi, albo dokadamy obserwacji
  if(v_len == 'short'){ 
    #czyli tylko przycinamy
    series_len = series_f[[3]] #dlugosc minimalna dlugosc w zbiorze testowym
    sereis_witch_len = series_f[[4]] #ktory z testowych jest najkrotszy
  } else if (v_len == 'median'){
    series_len = series_f[[1]] #dlugosc to mediana dlugosci szeregow w zbiorze testowym 
    sereis_witch_len = series_f[[2]] #ktory ma dlugosc mediany
  } else if (v_len == 'long'){
    series_len = series_f[[5]] #dlugosc to maksymalna dlugosc
    sereis_witch_len = series_f[[6]] #ktory ma dlugosc najwieksza
  }
  
  
  final_series <- list(series_len) #ustalenie dlugosci finalnego szeregu
  means<- list(v_size)
  for (i in 1: v_size) {
    final_series[[i]] <- v_date_train[[sereis_witch_len]]
    if(length(v_data[[i]]) > (series_len-1)){
      #jesli dluzsze to przycinamy
      final_series[[i]] <- v_data[[i]][c(1:(series_len-1),length(v_data[[i]]))]
    } else {
      #jesli krotsze to dolaczamy 
      n <- length(v_data[[i]])-1
      n2 <- ceiling(0.15*n)
      target <-  v_data[[i]][n+1]
      for (j in 1:(series_len-1) ) {
        for (k in 1:NumDimensions) {
          if(j<n){
            final_series[[i]][k,][j] <- v_data[[i]][1:(n-1)][k,][j][[1]]
          } else {
            if(v_padding == 'copy' ){
              means[k] = as.numeric(mean(t(v_data[[i]][k,][c((n-n2):n)])))
              final_series[[i]][k,][j] <- means[k][[1]] + final_series[[i]][k,][c(j-n +1)] 
              #mean(t(final_series[[i]][k,][c((j-n +1):(j-n+min(10,n)))])) - można też wygladzic 
            }
            else if (v_padding == 'zero'){
              final_series[[i]][k,][j] <- 0
            }
            else if (v_padding == 'mean'){
              means[k] = as.numeric(mean(t(v_data[[i]][k,][c(1:n)])))
              final_series[[i]][k,][j] <- means[k][[1]]
            }
          }
        }
        
        final_series[[i]][,series_len] <- target
      }
      
    }
    
  }
  return(final_series)
}

################### ROCKET ########################

normalise_function <- function(x){
  y <- (x-mean(x))/(sd(x)+1e-8)
  return(y)
}

generate_kernels <- function(input_length, dimensions, num_kernels=10000){
  
  candidate_lengths <- c(7,9,11)
  lengths <- sample(candidate_lengths, num_kernels,replace = TRUE)
  
  num_channel_indices <- integer(num_kernels)
  
  for(i in 1:num_kernels){
    limit <- min(dimensions,lengths[i])
    num_channel_indices[i] <- as.integer(2 ** runif(1,0, log2(limit+1))) 
  }
  
  channel_indices <- integer(sum(num_channel_indices))
  
  weights <- integer(dot(lengths, num_channel_indices))
  biases <- integer(num_kernels)
  dilations <- integer(num_kernels)
  paddings <- integer(num_kernels)
  
  a1 <- 1
  a2 <- 1
  
  for (i in 1:num_kernels) {
    length_ <- lengths[i]
    num_channel_indices_ <- num_channel_indices[i]
    
    weights_ <- rnorm(length_*num_channel_indices_) 
    
    b1 <- a1 + (num_channel_indices_ * length_) -1
    b2 <- a2 + num_channel_indices_ -1
    a3 <- 1
    
    for(j in 1:num_channel_indices_){
      b3 <- a3 + length_ -1
      weights_[a3:b3] <- weights_[a3:b3] - mean(weights_[a3:b3])
      a3 <- b3
    }
    
    weights[a1:b2] <- weights_
    
    channel_indices[a2:b2] = sample(1:dimensions,num_channel_indices_,replace=FALSE)
    
    biases[i] <- runif(1,-1, 1)
    
    dilation <- 2 ** runif(1,0, log2((input_length - 1) / (length_ - 1)))

    dilations[i] <- floor(dilation)
    
    if(sample(1:2, size = 1,replace = TRUE) == 1){
      padding <- floor(((length_ - 1) * dilation)  %/% 2)
    }else padding <- 0
    
    paddings[i] <- padding
    
    a1 <- b1
    a2 <- b2
  }
  
  return(list(weights = weights, lengths = lengths, biases = biases, dilations = dilations, paddings = paddings, num_channel_indices = num_channel_indices, channel_indices = channel_indices))
  
}

apply_kernel <- function(X, weights, length, bias, dilation, padding, num_channel_indices, channel_indices){
  
  numDim <- dim(X)[1]
  X_data <- vector(mode = "list", length = numDim)
  
  for(i in 1:numDim){
    X_data[[i]] <- normalise_function(unlist(X[i,]))
  }
    
  print(X_data)
  input_length <- length(X)
  
  output_length <- (input_length + (2 * padding)) - ((length - 1) * dilation)
  
  ppv_ <- 0
  max_ <- -Inf
  
  end <- (input_length + padding) - ((length - 1) * dilation)
  
  for(i in (-padding:(end-1))){
    
    sum_ <- bias
    
    index <- i
    
    for(j in 1:length){
      
      if( index > -1 & index < input_length){
        for(k in 1:num_channel_indices){
          sum_ <- sum_ + weights[j+(k-1)*length] *X[[index+1]][[channel_indices[k]]]
          
        }
      }
      index <- index + dilation
    }
    if(sum_ > max_) max_ <- sum_
    if(sum_ > 0) ppv_ <- ppv_ + 1 
    
  }
  return(c(ppv_ / output_length, max_))
}

clusterExport(myCluster,c("apply_kernel","normalise_function"))
registerDoParallel(myCluster)

apply_kernels <- function(X, kernels){
  
  num_examples <- length(X)
  num_kernels <- length(kernels$lengths)
  
  
  X_ <- foreach( i = 1:num_examples, .packages = c("foreach","geometry", "dplyr", "stringr", "glmnet", "RWeka", "rJava")) %dopar% {
    temp <- list(numeric(num_kernels * 2))
    a1 <- 1 
    a2 <- 1 
    a3 <- 1
    for(j in 1:num_kernels){
      b1 <- a1 + kernels$num_channel_indices[j]*kernels$lengths[j]-1
      b2 <- a2 + kernels$num_channel_indices[j]-1
      b3 <- a3 + 2 -1
      
      pom <- apply_kernel(X[[i]][,1:length(X[[i]])-1], kernels$weights[a1:b1], kernels$lengths[j], kernels$biases[j], kernels$dilations[j], kernels$paddings[j], kernels$num_channel_indices[j], kernels$channel_indices[a2:b2])

      temp[[1]][a3] <- pom[1]
      temp[[1]][b3] <- pom[2]

      a1 <- b1
      a2 <- b2
      a3 <- b3+1
      
    }
    temp
  }
  
  return(X_)
}



################### TEST ########################
func_test_1 <- function(v_len, v_padding){
  
  train <- cut_time_series3(Data_train,Data_classes)
  test <- cut_time_series3(Data_test,Data_classes)
  
  final_test <- series_alignment(test, test, TestSize, TestSize, v_len, v_padding)
  final_train <- series_alignment(train, test, TrainSize, TestSize, v_len, v_padding)
  
  kernels <- generate_kernels(length(final_train[[1]])-1,NumDimensions,num_kernels=100)
  
  train_data_after_kernels <- apply_kernels(final_train,kernels)
  train_data_after_kernels <- as.data.frame(t(as.data.frame(train_data_after_kernels)))

  test_data_after_kernels <- apply_kernels(final_test,kernels)
  test_data_after_kernels <- as.data.frame(t(as.data.frame(test_data_after_kernels)))

  x<- matrix(unlist(train_data_after_kernels), ncol =200, nrow = TrainSize)
  x[is.na(x)]<-0
  max_x <- max(x[!is.infinite(x)])
  min_x <- min(x[!is.infinite(x)])
  x[is.infinite(x) & x > 0] <- max_x
  x[is.infinite(x) & x < 0] <- min_x

  cv_model <<- cv.glmnet(x, t(as.numeric(targets2)), alpha = 0, family='multinomial', nfolds=nrow(x))
  best_lambda <- cv_model$lambda.min
  predictions_test <- as.factor(predict(cv_model, s = best_lambda, newx = matrix(unlist(test_data_after_kernels), ncol =200, nrow =TestSize), type="class"))
  
  return(as.numeric(predictions_test))
}

#lambda=seq(0.02, 2, 0.02)

targets2 <- c(Data_train[[1]]$target[[1]])
for(i in 2:TrainSize){
  targets2 <- c(targets2,Data_train[[i]]$target[[1]])
}

targets <- c(Data_test[[1]]$target[[1]])
for(i in 2:TestSize){
  targets <- c(targets,Data_test[[i]]$target[[1]])
}


##### short #####length(targets2)
print(str_c('Dane ', problem, ' metoda - short, liczba szeregow w tescie: ', TestSize))
ans <- vector()
conf_matrix <- matrix(0, length(Data_classes), length(Data_classes))
for(j in 1:1){
  pom <- func_test_1('short', 'copy')
  ans <- c(ans,sum(as.numeric(targets)==pom))
  for (i in 1:TestSize) {
    if((! pom[i] %in% as.numeric(targets)) & (! pom[i] %in% c(NaN, NA, Inf, -Inf))) {if(pom[i] < min(as.numeric(targets))){pom[i]<-min(as.numeric(targets))} else {pom[i]<-max(as.numeric(targets))}}
    conf_matrix[targets[i],pom[i]] <- conf_matrix[targets[i],pom[i]]+1
  }
  accuracy <- round(sum(diag(conf_matrix))/(TestSize*length(ans)),2)
}



mean_of_results <- round(mean(ans[!is.na(ans)]),2)
#mean_of_results_perc <- round(mean(ans[!is.na(ans)])/TestSize,2)
print(str_c('Srednia otrzymanych wynikow to ', mean_of_results, ' .'))
print(str_c('Accuracy wynosi ', accuracy, ' .'))
print('Macierz pomyek :')
print(conf_matrix)
print('Wykres skrzynkowy :')
boxplot(ans)

##### median - copy #####
print(str_c('Dane ', problem, ' metoda - median-copy, liczba szeregow w tescie: ', TestSize))
ans <- vector()
conf_matrix <- matrix(0, length(Data_classes), length(Data_classes))
for(j in 1:10){
    pom <- func_test_1('median', 'copy')
  ans <- c(ans,sum(as.numeric(targets)==pom))
  for (i in 1:TestSize) {
      if((! pom[i] %in% as.numeric(targets)) & (! pom[i] %in% c(NaN, NA, Inf, -Inf))) {if(pom[i] < min(as.numeric(targets))){pom[i]<-min(as.numeric(targets))} else {pom[i]<-max(as.numeric(targets))}}
    conf_matrix[targets[i],pom[i]] <- conf_matrix[targets[i],pom[i]]+1
  }
  accuracy <- round(sum(diag(conf_matrix))/(TestSize*length(ans)),2)
}

mean_of_results <- round(mean(ans[!is.na(ans)]),2)
#mean_of_results_perc <- round(mean(ans[!is.na(ans)])/TestSize,2)
print(str_c('Srednia otrzymanych wynikow to ', mean_of_results, ' .'))
print(str_c('Accuracy wynosi ', accuracy, ' .'))
print('Macierz pomyek :')
print(conf_matrix)
print('Wykres skrzynkowy :')
boxplot(ans)

##### median - mean #####
print(str_c('Dane ', problem, ' metoda - median-mean, liczba szeregow w tescie: ', TestSize))
ans <- vector()
conf_matrix <- matrix(0, length(Data_classes), length(Data_classes))
for(j in 1:10){
  pom <- func_test_1('median', 'mean')
  ans <- c(ans,sum(as.numeric(targets)==pom))
  for (i in 1:TestSize) {
    if((! pom[i] %in% as.numeric(targets)) & (! pom[i] %in% c(NaN, NA, Inf, -Inf))) {if(pom[i] < min(as.numeric(targets))){pom[i]<-min(as.numeric(targets))} else {pom[i]<-max(as.numeric(targets))}}
    conf_matrix[targets[i],pom[i]] <- conf_matrix[targets[i],pom[i]]+1
  }
  accuracy <- round(sum(diag(conf_matrix))/(TestSize*length(ans)),2)
}

mean_of_results <- round(mean(ans[!is.na(ans)]),2)
#mean_of_results_perc <- round(mean(ans[!is.na(ans)])/TestSize,2)
print(str_c('Srednia otrzymanych wynikow to ', mean_of_results, ' .'))
print(str_c('Accuracy wynosi ', accuracy, ' .'))
print('Macierz pomyek :')
print(conf_matrix)
print('Wykres skrzynkowy :')
boxplot(ans)

##### long - copy #####
print(str_c('Dane ', problem, ' metoda - long-copy, liczba szeregow w tescie: ', TestSize))
ans <- vector()
conf_matrix <- matrix(0, length(Data_classes), length(Data_classes))
for(j in 1:10){
  pom <- func_test_1('long', 'copy')
  ans <- c(ans,sum(as.numeric(targets)==pom))
  for (i in 1:TestSize) {
    if((! pom[i] %in% as.numeric(targets)) & (! pom[i] %in% c(NaN, NA, Inf, -Inf))) {if(pom[i] < min(as.numeric(targets))){pom[i]<-min(as.numeric(targets))} else {pom[i]<-max(as.numeric(targets))}}
    conf_matrix[targets[i],pom[i]] <- conf_matrix[targets[i],pom[i]]+1
  }
  accuracy <- round(sum(diag(conf_matrix))/(TestSize*length(ans)),2)
}

mean_of_results <- round(mean(ans[!is.na(ans)]),2)
#mean_of_results_perc <- round(mean(ans[!is.na(ans)])/TestSize,2)
print(str_c('Srednia otrzymanych wynikow to ', mean_of_results, ' .'))
print(str_c('Accuracy wynosi ', accuracy, ' .'))
print('Macierz pomyek :')
print(conf_matrix)
print('Wykres skrzynkowy :')
boxplot(ans)


##### long - mean #####
print(str_c('Dane ', problem, ' metoda - long-mean, liczba szeregow w tescie: ', TestSize))
ans <- vector()
conf_matrix <- matrix(0, length(Data_classes), length(Data_classes))
for(j in 1:10){
  pom <- func_test_1('long', 'mean')
  ans <- c(ans,sum(as.numeric(targets)==pom))
  for (i in 1:TestSize) {
    if((! pom[i] %in% as.numeric(targets)) & (! pom[i] %in% c(NaN, NA, Inf, -Inf))) {if(pom[i] < min(as.numeric(targets))){pom[i]<-min(as.numeric(targets))} else {pom[i]<-max(as.numeric(targets))}}
    conf_matrix[targets[i],pom[i]] <- conf_matrix[targets[i],pom[i]]+1
  }
  accuracy <- round(sum(diag(conf_matrix))/(TestSize*length(ans)),2)
}

mean_of_results <- round(mean(ans[!is.na(ans)]),2)
#mean_of_results_perc <- round(mean(ans[!is.na(ans)])/TestSize,2)
print(str_c('Srednia otrzymanych wynikow to ', mean_of_results, ' .'))
print(str_c('Accuracy wynosi ', accuracy, ' .'))
print('Macierz pomyek :')
print(conf_matrix)
print('Wykres skrzynkowy :')
boxplot(ans)

stopCluster(myCluster)
