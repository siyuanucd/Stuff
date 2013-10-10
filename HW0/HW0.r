#### 03 
fpath1 = "G:/STA250/"
msg = "Hello, my name is Bob. I am a statistician. I like statistics very much."
cha = unlist(strsplit(msg,""))

for(i in 1:length(cha)){
  write(cha[i], file = paste(fpath1,sprintf("out_%02d.txt",i),sep=""))
}

d = lapply(list.files("G:/STA250/",full=TRUE), readLines)
write(paste(d,collapse=""),file=paste(fpath1,"Prob03b.txt",sep=""))


#### 04
