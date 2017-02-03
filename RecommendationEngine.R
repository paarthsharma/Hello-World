setwd("C:\\Users\\parth.sharma\\Desktop\\Dataset")
getwd()
GirlsPref_DF <- read.csv("Girls Preference Data.csv")
## The above data tells us the how many times each of the 10 boy parameters
## occured in the profiles opened by the  girls

## We now calculate the ratio of number of occurences of a parameter and number 
## of profiles viewed for each girl
for(i in 1:nrow(GirlsPref_DF)){
  for(j in 4:ncol(GirlsPref_DF)){
    if(GirlsPref_DF[i,j]!=0){GirlsPref_DF[i,j]=GirlsPref_DF[i,j]/GirlsPref_DF[i,3]}
  }
}

## Importing data for Boys Parameters
BoysParameters_DF <- read.csv("Boy Parameters.csv")


## We now convert dataframes to matrices
matrix1 <- data.matrix(GirlsPref_DF, rownames.force = NA)
matrix1 <- matrix1[,-(1:3)]
colnames(matrix1) <- NULL

matrix2 <- data.matrix(BoysParameters_DF, rownames.force = NA)
matrix2 <- matrix2[,-(1:2)]
colnames(matrix2) <- NULL


## Preference Score between a pair is calculated by averaging the scores for 
## the parameters that are present in a boys profile
PreferenceScoreMatrix <- (matrix1%*%t(matrix2))/5
PreferenceScoreMatrix <- t(PreferenceScoreMatrix)


## Importing data for Girl Profile

GirlProfile_DF <- read.csv("Girl Profile Data.csv")


matrix3 <- as.matrix(GirlProfile_DF)
matrix3 <- matrix3[,-(1:2)]
colnames(matrix3) <- NULL

# Recoding the variables

for(i in 1:nrow(matrix3)){
  if(matrix3[i,1]=="26-30") {matrix3[i,1]=-1}
  else {matrix3[i,1]=1}
}

for(i in 1:nrow(matrix3)){
  if(matrix3[i,2]=="Average") {matrix3[i,2]=-1}
  else {matrix3[i,2]=1}
}

for(i in 1:nrow(matrix3)){
  if(matrix3[i,3]=="Wheatish") {matrix3[i,3]=-1}
  else {matrix3[i,3]=1}
}

for(i in 1:nrow(matrix3)){
  if(matrix3[i,4]=="Hindu") {matrix3[i,4]=-1}
  else {matrix3[i,4]=1}
}

for(i in 1:nrow(matrix3)){
  if(matrix3[i,5]=="Single") {matrix3[i,5]=-1}
  else {matrix3[i,5]=1}
}

matrix3 <- as.data.frame(matrix3)
matrix3 <- data.matrix(matrix3,rownames.force=NA)

for(i in 1:nrow(matrix3)){
  for(j in 1:ncol(matrix3)){
    if(matrix3[i,j]==2){matrix3[i,j]=-1}
  }
}


## Calculating the similarity index matrix

SimIndMatrix <- (matrix3%*%t(matrix3))/5

for(i in 1:nrow(SimIndMatrix)){
  for(j in 1:ncol(SimIndMatrix)){
    if(i==j){SimIndMatrix[i,j]=0}
  }
}


## Importing data for profiles opened by a girl

ProfilesOpened <- read.csv("Girls Data on Profiles Opened.csv")

ProfilesOpened <- data.matrix(ProfilesOpened,rownames.force=NA)

ProfilesOpened <- ProfilesOpened[,-1]

colnames(ProfilesOpened) <- NULL


## Calculating probability of a girl opening a boy's profileby taking into account
## the similarity index with all the girls who have opened the profile

ProbMatrix <- ProfilesOpened%*%SimIndMatrix

ProbMatrix <- round(ProbMatrix,2)

## Standardasing the probability values between 0 and 1

standardise0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
ProbMatrix <- standardise0to1(ProbMatrix)

## Calculating the recommendation score of all the girls for a boy by averaging over
## the preference match score and probability of a girl opening the boy's profile

RecScoreMatrix <- (PreferenceScoreMatrix+ProbMatrix)/2
RecScoreMatrix <- round(RecScoreMatrix,2)

rownames(RecScoreMatrix) <- BoysParameters_DF[,2]
colnames(RecScoreMatrix) <- t(GirlProfile_DF[,2])

## We can now recommend girls' profiles to each guy in descending order of their scores
