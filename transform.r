f = read.csv("c:/Users/Steve/Downloads/run_results5.csv")
df = data.frame(f)

#duplicate banknamen
length(df[,1]) / 6
i = rapply(list(0:(length(df[,1]) /6)), function(x) x * 6)
r = rapply(list(i), function(x) df[x + 1,3])
dr = data.frame(r)
drep = data.frame(rep(dr[,"r"], each=6))

df$ID <- seq.int(nrow(df))
drep$ID <- seq.int(nrow(drep))
total <- merge(df,drep,by="ID")

#parse banknaam
n = rapply(list(total[,5]), function(x) 
  gsub('<', '',
  substr(x, regexpr('asp">', x) + 5 , regexpr('</a>', x))
    )
)

n2 = data.frame(n)
n2$ID <- seq.int(nrow(total))
total2 = merge(total,n2,by="ID")

#detect percentage
f = data.frame(grepl('%|nbsp', total2[,4]))
f$ID = seq.int(nrow(f))
total3 = merge(total2,f,by="ID")

#select columns and name them
total4 = total3[,c(1,2,4,6,7)]
colnames(total4) = c("ID", "Jaar", "Rente", "Bank", "Filter")

#filter out rows with useless data
total5 = total4[total4$Filter==T,]

#add type of interest rate
st = data.frame(c("NHG", "60", "80", "100"))
s = data.frame(c("NHG", "60", "80", "100"))

for (i in 0:round(length(total5$ID) / 4)){
  s = rbind(s,st)
}

total5$ID2 = seq.int(nrow(total5))
s$ID2 = seq.int(nrow(s))

total6 = merge(total5,s,by="ID2")

#remove percentage and empty cell
r = data.frame(gsub('%|&nbsp;', '', total6$Rente))
r$ID2 = seq.int(nrow(r))
total7 = merge(total6,r,by="ID2")
final = total7[,c(3,5,7,8)]
colnames(final) = c("Jaar", "Bank","Type","Rente")

