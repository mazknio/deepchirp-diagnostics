args = commandArgs(trailingOnly= TRUE)
predicted = read.csv(args[1], header = T)
actual = read.csv(args[2], header = T)

sink("diagnostics", append=FALSE, split=TRUE)
library(ggplot2)


#helper function for fp percentage
fp = function(a,b,x,y) {
  if (a <= x && b >= y){
    return ((x - a + b - y)/(b - a))
  }
  if (a >= x && b <= y) {
    return (0) 
  }
  if (a < x && b < y)
    return ((x-a)/(b-a))
  if (a > x && b > y){
    return ((b-y)/(b-a))
  }
}

#helper function for fn percentage
fn = function(a,b,x,y) {
  if (a <= x && b >= y){
    return (0)
  }
  if (a >= x && b <= y) {
    return ((a-x+y-b)/(y-x)) 
  }
  if (a < x && b < y)
    return ((y-b)/(y-x))
  if (a > x && b > y){
    return ((a-x)/(y-x))
  }
}

#helper for "underlabling issue"
adder = function(a,b,y){
  if (b < y){
    return (b-a)
  }
  if (b > y){
    return (y-a)
  }
}

num_actual_words = length(subset(actual, name != "z")[,1])
col = vector(mode = "numeric", length = nrow(predicted))
predicted[,4:11] = col
predicted[,1] = factor(predicted[,1], levels=levels(actual[,1]))
colnames(predicted)[4:11] = c("fp_overlap","fn_overlap","mislabled","complete_fp","complete_fn", "overlable","noise_pickup","hit")
j = 1
for (i in 1:(nrow(predicted))){
  for(j in j:(nrow(actual))){
    #accounts for if an interval does not overlap at all
    if (predicted[i,]$stop <= actual[j,]$start){
      predicted[i,]$complete_fp = 1
      break
    }
    #accounts for overlapping intervals
    if ((predicted[i,]$start <= actual[j,]$stop) && (actual[j,]$start <= predicted[i,]$stop)){
      predicted[i,]$hit = 1
      #accounts for mismatches
      if (predicted[i,]$name != actual[j,]$name){
        if (actual[j,]$name == "z"){
          predicted[i,]$noise_pickup = 1
        }
        else {
          predicted[i,]$mislabled = 1
        }
        actual = actual[-j,]
        break
      }
      else {
        #accounts for interval errorrs
        predicted[i,]$fp_overlap = fp(predicted[i,]$start,predicted[i,]$stop,actual[j,]$start,actual[j,]$stop)
        predicted[i,]$fn_overlap = fn(predicted[i,]$start,predicted[i,]$stop,actual[j,]$start,actual[j,]$stop)
        z = 1
        while ((predicted[i,]$start < actual[j+z,]$stop) && (actual[j+z,]$start < predicted[i,]$stop)){
          predicted[i,]$fp_overlap = ((predicted[i,]$fp_overlap)*(predicted[i,]$stop - predicted[i,]$start) - adder(actual[j+z,]$start,actual[j+z,]$stop,predicted[i,]$stop)) / (predicted[i,]$stop - predicted[i,]$start)
          z = z+1
          predicted[i,]$overlable = 1
        }
        actual = actual[-(j:(j+(z-1))),]
        break
      }
    }
  }
}
complete_fn = subset(actual, name != "z")
fn_freq_table = as.data.frame(table(complete_fn$name))
fn_freq_table = subset(fn_freq_table, Freq != 0)
cat("FALSE NEGATIVE WORDS ANALYSIS\n")
cat(paste(length(complete_fn[,1])/num_actual_words, "of the total non 'z' words in our test-set were missed\n"))
cat(paste("Out of these", length(complete_fn[,1]), "words...\n"))
for (i in 1:nrow(fn_freq_table)){
  cat(paste0(fn_freq_table[i,2]/length(complete_fn[,1])*100,"%", " were ", fn_freq_table[i,1]),"\n")
}

complete_fp_set = subset(predicted, complete_fp != 0)
fp_freq_table = as.data.frame(table(complete_fp_set$name))
fp_freq_table = subset(fp_freq_table, Freq != 0)
cat("\nFALSE POSITIVE WORDS ANALYSIS\n")
cat(nrow(complete_fp_set)/nrow(predicted), "of the predicted words were total false positives\n")
cat("Out of these", nrow(complete_fp_set), "words... \n")
for (i in 1:nrow(fp_freq_table)){
  cat(paste0(round(fp_freq_table[i,2]/nrow(complete_fp_set)*100,digits = 3),"%", " were ", fp_freq_table[i,1]),"\n")
}


sum(predicted$hit)
cat("\nMISLABEL ANALYSIS\n")
cat((sum(predicted$mislabled)+sum(predicted$noise_pickup))/sum(predicted$hit), "of the words with overlapping intervals were incorrectly labled\n")
cat("of these", (sum(predicted$mislabled)+sum(predicted$noise_pickup)), "mislabled words,", sum(predicted$mislabled)/ (sum(predicted$mislabled)+sum(predicted$noise_pickup)), "of them are syllables mistaken for other sylables, while", sum(predicted$noise_pickup)/ (sum(predicted$mislabled)+sum(predicted$noise_pickup)), "were noise mistaken for sylables \n")
complete_mislabled_set = subset(predicted, mislabled != 0)
mislabled_freq_table = as.data.frame(table(complete_mislabled_set$name))
mislabled_freq_table = subset(mislabled_freq_table, Freq != 0)
cat("of the", sum(predicted$mislabled), "sylables that were incorrectly labled...\n")
for (i in 1:nrow(mislabled_freq_table)){
  cat(paste0(round(mislabled_freq_table[i,2]/sum(predicted$mislabled)*100,digits = 3),"%", " were ", mislabled_freq_table[i,1],"\n"))
}
cat("of the", sum(predicted$noise_pickup), "z's that were incorrectly labled as sylables...\n")
complete_noise_set = subset(predicted, noise_pickup != 0)
noise_freq_table = as.data.frame(table(complete_noise_set$name))
noise_freq_table = subset(noise_freq_table, Freq != 0)
for (i in 1:nrow(noise_freq_table)){
  cat(paste0(round(noise_freq_table[i,2]/sum(predicted$noise_pickup)*100,digits = 3),"%", " were ", noise_freq_table[i,1],"\n"))
}
cat("\nINTERVAL OVERLAP\n")
complete_overlap_set = subset(predicted, hit != 0)
complete_overlap_set = subset(complete_overlap_set, noise_pickup == 0 )
complete_overlap_set = subset(complete_overlap_set, mislabled == 0 )
overlap_freq_table = as.data.frame(table(complete_overlap_set$name))
overlap_freq_table = subset(overlap_freq_table, Freq != 0)
cat("The average false positive overlap is", mean(complete_overlap_set$fp_overlap), "\n")
for (i in 1:nrow(overlap_freq_table)){
cat(paste0("The average false positve overlap for letter ", overlap_freq_table[i,1], " is ", mean(complete_overlap_set[(complete_overlap_set$name == overlap_freq_table[i,1]),]$fp_overlap), "\n"))
}
cat("The average false negative overlap is", mean(complete_overlap_set$fn_overlap), "\n")
for (i in 1:nrow(overlap_freq_table)){
cat(paste0("The average false negative overlap for letter ", overlap_freq_table[i,1], " is ", mean(complete_overlap_set[(complete_overlap_set$name == overlap_freq_table[i,1]),]$fn_overlap), "\n"))
}

cat("\nOVERLABLING\n")
complete_overlable_set = subset(predicted, overlable != 0)
cat("There was", nrow(complete_overlable_set), "cases of overlabling\n")
overlable_freq_table_name = as.data.frame(table(complete_overlable_set$name))
overlable_freq_table_name = subset(overlable_freq_table_name, Freq != 0)
overlable_freq_table_size = as.data.frame(table(complete_overlable_set$overlable))
for (i in 1:nrow(overlable_freq_table_name)){
  cat(paste0("Out of all ", nrow(complete_overlable_set), " cases of overlabling ", round(overlable_freq_table_name[i,2]/nrow(complete_overlable_set)*100,digits = 3), "% were ", overlable_freq_table_name[i,1], "\n"))
}
for (i in 1:nrow(overlable_freq_table_name)){
  cat(paste0("Out of all ", nrow(complete_overlable_set), " cases of overlabling ", overlable_freq_table_size[i,2]/nrow(complete_overlable_set)*100, "% were of size ", overlable_freq_table_size[i,1], "\n"))
}


fp_plot = ggplot(complete_overlap_set, aes(x = fp_overlap, fill = name)) + geom_histogram(binwidth = .03)
fn_plot = ggplot(complete_overlap_set, aes(x = fn_overlap, fill = name)) + geom_histogram(binwidth = .03)
jpeg("./fp_overlap.jpg")
plot(fp_plot)
jpeg("./fn_overlap.jpg")
plot(fn_plot)
#THEN AVERAGE TOTAL, AVERAGE AMONG LETTERS
write.csv(predicted, file="raw_data.csv", row.names=FALSE)


