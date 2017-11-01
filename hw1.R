#step 1: output DTsubset.rdata
library(readr)
z <- as.data.frame(read_delim(file='data_complete.rdata', delim=' ', quote='\\\'',escape_double=F, trim_ws=T,guess_max=30000))
###guess_max = number greater than #records in data file
##exclude records with missing INTRDVX values
z <- z[z$INTRDVX_ %in% c("\"D\"", "\"T\""),]
##export data subset
write.table(z,'DTsubset.rdata', row.names=F,col.names=T,quote=F)



#step2: convert character to factor variables
for (i in 1:ncol(z)){
  if(is.character(z[,i])){
    z[,i]=factor(z[,i])
  }
}

#step 3: create DTsubsetdsc.txt
z.names <- names(z);role <- rep('n', ncol(z))
con.gp <- NULL #indices of variables with all NAs or are constant
for(j in 1:ncol(z)){
  if(sum(!is.na(z[,j])) == 0){ #all NAs
    con.gp <- c(con.gp,j)
  } else {
      if(class(z[,j]) == 'factor'){
        if(nlevels(z[,j]) <= 1){
          con.gp <- c(con.gp,j) #only one level
        } else {role[j] <- 'c'}
      } else { #exclude variables with constant values
          r <- range(z[,j], na.rm = T)
          if(r[2] <= r[1]) con.gp <- c(con.gp,j) #constant
        }
    }
}

role[con.gp] <- 'x' #delete variables with all NA
role[z.names %in% "INTRDVX"] <- "d" 
role[z.names %in% "FINLWT21"] <- "x"
role[z.names %in% "INTRDVBX"] <- "x"
role[z.names %in% "INTR_VBX"] <- "x"
role[z.names %in% "INTRDVB"] <- "x"
role[z.names %in% "INTRDVB_"] <- "x"
role[z.names %in% "INTRDVX1"] <- "x"
role[z.names %in% "INTRDVX2"] <- "x"
role[z.names %in% "INTRDVX3"] <- "x"
role[z.names %in% "INTRDVX4"] <- "x"
role[z.names %in% "INTRDVX5"] <- "x"
role[z.names %in% "INTRDVXI"] <- "x"
role[z.names %in% "INTRDVXM"] <- "x"
role[z.names %in% "INTRDVX_"] <- "x" 
write("DTsubset.rdata","DTsubsetdsc.txt")
write("NA",file="DTsubsetdsc.txt",append=TRUE)
write("2",file="DTsubsetdsc.txt",append=TRUE)
write.table(cbind(1:ncol(z),names(z), role),file="DTsubsetdsc.txt", append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)

#step 4: importance scoring

#step 5: create data set DTfinal.rdata

#step 6 :create DTfinaldsc.txt
library(readr)
z <- as.data.frame(read_delim(file = 'DTfinal.rdata', delim=' ', quote='\\\'',escape_double=F,trim_ws=T,guess_max=30000))
###convert character variables to factors
for (i in 1:ncol(z)){
  if(is.character(z[,i])) z[,i]=factor(z[,i])
}
role <- rep('n',ncol(z))
con.gp <- NULL # indices of variables with all NAs or are constant
for(j in 1:ncol(z)){
  if(class(z[,j])=='factor') role[j] <- 'c'
}
role[names(z) %in% 'INTRDVX'] <- 'd'
write('DTfinal.rdata', 'DTfinaldsc.txt')
write('NA', file='DTfinaldsc.txt', append=T)
write('2',file='DTfinaldsc.txt', append=T)
write.table(cbind(1:ncol(z),names(z),role), file='DTfinaldsc.txt', append=T, row.names=F,col.names=F,quote=F)

#guide imputation
 a <- "factor"; n <- "numeric"
 vartype <- c(n,a,n,a,n,n,n,n,n,n,n,a,a,a,n,a,n,a,n,n,a,n,a,n,a,n,a,n,a,n,n,a,n,n,a,n,n,n,a,n,a,n,a,a,a,a,a,a,a,a,a,n,n,n,a,n,a,n,a,a,a,a,n,n,n,n,a,a,a,a,n,a,a,n,a,n,a,n,a,a,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,a,a,a,a,a,a,a,a,a,n,a,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,a,n,n,a,a,a,a,n,n,a,n,a,n,a,a,a,n,n,a,n,a,n,a,n,a,n,n,a,a,a,a,a,a,a,a,n,a,a,n,a,a,a,n,n,n,a,n,a,a,n,a,n,a,n,a,a,a,a,n,n,n,n)
 z <- read.table("DTfinal.rdata",header=TRUE,colClasses=vartype)


dsc <- readLines("DTfinaldsc.txt")
roles <- NULL
for(j in 4:length(dsc)){ # store original roles of variables
  strng <- strsplit(dsc[j]," +")[[1]]
  roles <- c(roles,strng[3])
}
roles[roles == "d"] <- "x" # exclude INRDVX from imputation
n <- nrow(z); 
z.imputed <- z 
for(j in 1:ncol(z)){
  tmproles <- roles
  k <- sum(!is.na(z[,j]))
  if(k > 0 & k < n){ # skip variables without NAs
    tmproles[j] <- "d" # make this variable "D" 
    write(dsc[1],"tmpdsc.txt") 
    write("NA","tmpdsc.txt",append=TRUE) 
    write("2","tmpdsc.txt",append=TRUE) 
    write.table(cbind(1:ncol(z),names(z),tmproles),"tmpdsc.txt",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)

    if(roles[j] == "n"){ # regression 
      system("./guide < reg_in.txt > log.txt")
    } else if(roles[j] == "c"){ # classification 
      system("./guide < class_in.txt > log.txt")
    }
    fit.info <- read.table("fit.txt",header=TRUE) 
    gp <- fit.info$train == "n"
    z.imputed[gp,j] <- fit.info$predicted[gp]
  }
}
    write.table(z.imputed,"data_imputed.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)

#cross validation
mse<-0
ncv<-0
cvgroup<-sample.int(ncv,nrow(z),replace=T)
for(j in 1:ncv){
  z$wt <- rep(0,n) # create a weight variable
  z$wt[cvgroup != j] <- 1 
  write.table(z,"cv.data",col.names=FALSE,row.names=FALSE,quote=TRUE) 
  system("./guide < cvinput.txt > log.txt")
  ## change "system" to "shell" for Windows
  cvfit <- read.table("cvfit.txt",header=TRUE) 
  observed <- cvfit$observed[cvfit$train == "n"] 
  predicted <- cvfit$predicted[cvfit$train == "n"] 
  mse <- mse+sum((observed-predicted)^2)
}
mse <- mse/nrow(z) # mean-squared prediction error




















