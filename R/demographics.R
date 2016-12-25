#' A function that gives the demographics of a dataset
#'
#' This function will give you a summary of the demographics of a dataset.
#' It requires the following columns: dataset$subject, dataset$gender, dataset$age
#' dataset$gender can have values 0,1,2,3 or na, male, female, other
#' @param dataset A dataframe with subject, gender, age as variables.
#' @keywords demographics
#' @export
#' @examples
#' demographics()

demographics <- function(dataset){
  X <- dataset
  
  # n
  n <- length(X$subject)
  n.text <- paste("total number of participants:",n)

  # gender
  gender.ratio <- data.frame(table(X$gender))
  if(gender.ratio[1,1] %in% c(0:3)){
    gender.ratio$gender[gender.ratio$Var1==0] <- "na"
    gender.ratio$gender[gender.ratio$Var1==1] <- "male"
    gender.ratio$gender[gender.ratio$Var1==2] <- "female"
    gender.ratio$gender[gender.ratio$Var1==3] <- "other"
    gender.ratio <- gender.ratio[c("gender","Freq","Var1")]
    gender.ratio <- subset(gender.ratio, select = - c(Var1))
    colnames(gender.ratio) <- c("gender","frequency")
  } else {
    colnames(gender.ratio) <- c("gender","frequency")
  }
  gender.ratio$proportion <- round(gender.ratio$frequency / n * 100,2)
  gender.ratio

  # age
  underage <- length(which(X$age < 18 & X$age!=0))
  noage    <- length(which(X$age==0))

  age <- X$age[X$age > 18]

  age.text0a <- paste("AGE: ")
  age.text0b <- paste("--------------------")
  Y1 <- 0
  Y2 <- 0
  if (underage > 0){
    underage.text <- paste("below 18:",underage)
    Y1 <- 2
  }
  if (noage > 0){
    noage.text <- paste("not recorded:",noage)
    Y2 <- 3
  }
  age.text1 <- paste("mean: ",round(mean(age),2),"; median: ",median(age),"; sd: ",round(sd(age),2),sep="")
  age.text2 <- paste("min: ",min(age),"; max: ",max(age),sep="")
  
  # produce output
  Y <- Y1 + Y2
  if (Y == 0){
    demo <- capture.output(cat(n.text,age.text0b,age.text0a,age.text1,age.text2,age.text0b,age.text0b,sep="\n"))
  }
  if (Y == 2){
    demo <- capture.output(cat(n.text,age.text0b,age.text0a,underage.text,age.text1,age.text2,age.text0b,age.text0b,sep="\n"))
  }
  if (Y == 3){
    demo <- capture.output(cat(n.text,age.text0b,age.text0a,noage.text,age.text1,age.text2,age.text0b,age.text0b,sep="\n"))
  }
  if (Y == 5){
    demo <- capture.output(cat(n.text,age.text0b,age.text0a,underage.text,noage.text,age.text1,age.text2,age.text0b,age.text0b,sep="\n"))
  }
  demo2<-cat(demo,sep="\n")
  return(print(gender.ratio,demo2))
}
