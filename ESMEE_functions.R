###### 
###### Function to reduce the digits on patent data
######
#       Input is one column, digits is the number of digits requested (3, 4, 7, or 9), sep is the separator of the data
reduce.digits.fast = function (data, digit, sep){
  # verifier le digit demandé
  if (digit == 3){
    y = function(x) {
      substr(x, 1, 3)
    }
    z = function(x) {
      tmp = x
      x = na.omit(x)
      x = unique(x)
      x = paste(t(x), collapse = sep)
    }
    data <- as.matrix(data)
    result = matrix(NA, nrow = dim(data)[1])
    data = splitstackshape::cSplit(data, 1,  sep = sep)
    data = as.matrix(data)
    data[] <- vapply(data, y, character(1))
    result[] = apply(data, 1, z)
    #print(class(result))
    return(result)
  }
  if (digit == 4){
    # creer une fonction qui extrait le 4 premiers caracteres
    y = function(x) {
      substr(x, 1, 4)
    }
    # creer une fonction qui enlève les na, les doublons, recoller 
    z = function(x) {
      tmp = x
      x = na.omit(x)
      x = unique(x)
      x = paste(t(x), collapse = sep)
    }
    # ici traitement des donnees
    data <- as.matrix(data) # assurer que format soit matrice
    result = matrix(NA, nrow = dim(data)[1]) # créer une matrice qui contiendra les resultats
    data = splitstackshape::cSplit(data, 1, sep =sep) # couper les données
    data = as.matrix(data) # remettre en matrice
    data[] <- vapply(data, y, character(1)) # appliquer la fonction y (soustraire) à toutes les lignes (vapply)
    result[] <- apply(data, 1, z) # Appliquer la fonction y sur toutes les
    return(result)
  }
  if (digit == 7){
    y = function(x) {
      sub("/.*", "", x)
    }
    z = function(x) {
      tmp = x
      x = na.omit(x)
      x = unique(x)
      x = paste(t(x), collapse = sep)
    }
    data <- as.matrix(data)
    result = matrix(NA, nrow = dim(data)[1])
    data = splitstackshape::cSplit(data, 1, sep = sep)
    data = as.matrix(data)
    data[] <- vapply(data, y, character(1))
    result[] = apply(data, 1, z)
    return(result)
  }
  if (digit == 9){return(data)}
}



###### 
###### Function to extract a network from a column of data
######

network_creation = function(data, dynamic, sep){
  res = c(NA,NA,NA)
  # we start by splitting the data
  nw = splitstackshape::cSplit(data, 1, sep = sep)
  nw = as.matrix(nw)
  # garde-fou : if the dimension of the dataframe after splitting is the same, then there is no network
  if(dim(nw)[2] == dim(data)[2]){
    print("You idiot, there is no network to be made here!")
  }else{
    # there is data, we need to make the list of links
    # we do this by looping over the columns and combining them
    for(i in 2:(dim(nw)[2]-1)){
      for(j in (i+1):dim(nw)[2]){
        tmp = cbind(nw[,1], nw[,i], nw[,j])
        tmp = na.omit(tmp)
        res = rbind(res, tmp)
      }
    }
    res = as.data.frame(na.omit(res))
    colnames(res) = c("Year", "Source", "Target")
    if(missing(dynamic) == FALSE){
      res[,c(2,3)] = alpha.order(res[,c(2,3)])
      res$linkid = paste(res[,2], res[,3], sep = ";")
      res = res %>% group_by(linkid) %>% summarize("weight" = n(), "first_year" = min(Year), "last_year" = max(Year))
      res = splitstackshape::cSplit(res, 1, sep = ";")
      colnames(res) = c("Weight", "First_year", "Last_year", "Source", "Target")
    }
    return(res)
  }
}


######
###### Put elements into alphabetical order 
######

alpha.order<-function(data){
  data<-as.matrix(data)
  for (i in 1:dim(data)[1]){
    data[i,1] = replace_non_ascii(data[i,1], remove.nonconverted = TRUE)
    data[i,2] = replace_non_ascii(data[i,2], remove.nonconverted = TRUE)
    if (data[i,1] > data[i,2]){
      s<-data[i,1]
      data[i,1]<-data[i,2]
      data[i,2]<-s
    }  
  }
  return(data)
}