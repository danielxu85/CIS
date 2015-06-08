#citation rank analysis to accompany paper:
#"A Survey of Chinese Interpreting Studies: Who influences who ... and why?"


library(xlsx)
library(cluster)
library(splines)
library(glmnet)
library(ggplot2)

#load and create data.frames ####

#need to do a test to see if an encoding difference is responsible for not matching AllScores to adjacency matrix
pagerank.csv = read.csv("../data/AllScores_UTF8.csv",as.is=TRUE)
adjacency.tab = read.table('../data/AllCitationsV1.0.tsv.graph',header=T,sep=",",stringsAsFactors=FALSE)

unique.names.adj = unique(c(adjacency.tab$Source,adjacency.tab$Target)) #8732 length
unique.names.pagerank = unique(pagerank.csv$Id) # 12815 length

rank.no.tab.csv = unique.names.pagerank[which(!(unique.names.pagerank %in% unique.names.adj))]

authornametranslation.dat = read.csv("../data/Author Name Translation SPLITTED (1).csv",as.is=TRUE)

translatename2 <- function(name,CtoE=TRUE)
{
  if(CtoE)
  {
    from = authornametranslation.dat$Chinese.Name
    to = authornametranslation.dat$English.Name
  } else
  {
    to = authornametranslation.dat$Chinese.Name
    from = authornametranslation.dat$English.Name
  }
  
  idx = which(from %in% name) 
  if(length(idx) > 0)
    return(strip_string(toupper(to[idx[1]]))) #take the first match
  
  return(strip_string(toupper(name)))
}

rank.no.tab.csv.translated = sapply(rank.no.tab.csv,translatename)

print(sum(pagerank.csv$Id %in% authornametranslation.dat$Chinese.Name))

english.names.translated2 = toupper(sapply(authornametranslation.dat$English.Name,strip_string))

#find how many of these are NOT in our documents database
for(i in 1:3)
  print(sum(!(all.profiles[[i]]$authors %in% english.names.translated2) ))

#down to 103, great!


#Has all information on papers in database in 3 data.frames, journal.dat , mathesis.dat , dissertations.dat
load("allpapers.Rdata")

#has information on author citation rankings (pagerank.xls), adjacency matrix (adjacency.tab), and authornametranslation (authornametranslation.xls)
load("pagerank.Rdata") 

#create keyword and meme author profile for articles, matheses and dissertations ####

strip_string <- function(x,remove.all.whitespace=TRUE)
{
  x = gsub("[[:punct:]]", "", x)
  if(remove.all.whitespace)
    x = gsub("\\s","", x)
  else
    x = gsub("^\\s+|\\s+$", "", x)
  return(x)
}

split_string <- function(x,perl.string,whitespace=TRUE)
{
  x = toupper(unlist(strsplit(x,split=perl.string,perl=TRUE)))
  x = strip_string(x,whitespace)
  return(x)
}

#find Chinese formatted split character version of ";"
alt.split.char = levels(articles.dat$Academic.Affiliation..Chinese.)[as.numeric(articles.dat[10,6])]
alt.split.char = strsplit(alt.split.char,NULL)[[1]][32]

perl.split.string = paste("(,|;|/|",alt.split.char,")",sep="")
perl.split.string2 = paste("(;|/|",alt.split.char,")",sep="")

# a comma for separate on both keywords and authors
profile.split.string = ","

createProfile <- function(dat,authorCol="Author",wordCol="Keywords")
{
  #create paper authors matrix
  
  all.authors = dat[,authorCol]
  all.authors = levels(all.authors)[as.numeric(all.authors)]
  
  authors.list = sapply(all.authors,split_string,perl.string=profile.split.string)
  
  all.authors = unique(unlist(authors.list))
  
  getauthorvec <- function(paper.idx)
  {
    1 * ( all.authors %in% authors.list[[paper.idx]] )
  }
  
  #this will be a authors x papers matrix
  authorship.mat = sapply(1:length(authors.list),getauthorvec)
  
  #number of papers each author wrote
  num.papers = rowSums(authorship.mat)
  
  #get keyword profile of each author
  all.words = dat[,wordCol]
  all.words = levels(all.words)[as.numeric(all.words)]
  
  words.list = sapply(all.words,split_string,perl.string=profile.split.string)
  
  unique.words = sort(unique(unlist(words.list)))
  unique.words = unique.words[which(unique.words != "")]
  
  getwordsofAuthor <- function(i)
  {
    words = unlist(words.list[which(authorship.mat[i,] == 1)])
    words = table(factor(words,levels=unique.words))
  }
  
  profile = t(sapply(seq(all.authors),getwordsofAuthor))
  
  rownames(profile) = all.authors
  colnames(profile) = unique.words
  
  return(list(authors=all.authors,words=unique.words,num.papers=num.papers,profile=profile))
}

profile.journal.keyword = createProfile(journal.dat)
profile.ma.keywords = createProfile(mathesis.dat,authorCol="Author.s.")
profile.dis.keywords = createProfile(dissertation.dat,authorCol="Author.s.")

#english-chinese name translation

translatename <- function(name,CtoE=TRUE)
{
  if(CtoE)
  {
    from = authornametranslation.xls$Chinese.Name
    to = authornametranslation.xls$English.Name
  } else
  {
    to = authornametranslation.xls$Chinese.Name
    from = authornametranslation.xls$English.Name
  }
  
  idx = which(from %in% name) 
  if(length(idx) > 0)
    return(strip_string(toupper(to[idx[1]]))) #take the first match
  
  return(strip_string(toupper(name)))
}

adjacency.tab.english = adjacency.tab
adjacency.tab.english$Source = sapply(adjacency.tab.english$Source,translatename)
adjacency.tab.english$Target = sapply(adjacency.tab.english$Target,translatename)

#also do the same for PageRank
node.data.english = pagerank.xls
node.data.english$Id = sapply(node.data.english$Id,translatename)

#add to this odds ratio and numpapers

all.profiles = list(profile.journal.keyword,profile.ma.keywords,profile.dis.keywords)

addnumpapers <- function(name)
{
  numpapers = 0
  for(i in seq(all.profiles))
  {
    idx = which(all.profiles[[i]]$authors %in% name)
    numpapers = numpapers + sum(all.profiles[[i]]$num.papers[idx])
  }
  return(numpapers)
}

node.data.english$num.papers = sapply(node.data.english$Id,addnumpapers)

node.data.english$degree.odds = node.data.english$In.Degree / (sum(node.data.english$In.Degree) - node.data.english$In.Degree)
node.data.english$degree.odds = node.data.english$degree.odds * (sum(node.data.english$Out.Degree) - node.data.english$Out.Degree) / node.data.english$Out.Degree

#get keywords groups and compute most frequent group ####

combined_keywords = read.csv("../data/combined_keywords.csv")

key_leftout.idx = which(!(keywords.all$Keywords %in% combined_keywords$Keywords))

sort(keywords.all$Keywords[key_leftout.idx])

combined_keywords[!(combined_keywords$Keywords %in% keywords.all$Keywords),]

keyword_combined_map <- sort(keywords.all$Keywords[key_leftout.idx])
keyword_combined_map = levels(keyword_combined_map)[as.numeric(keyword_combined_map)]
keyword_combined_map = cbind(keyword_combined_map,rep("",length(keyword_combined_map)))
colnames(keyword_combined_map) = c("Keyword removed from original list","Replacement (combined) keyword")

write.csv(keyword_combined_map,file="keywords_combined_map.csv",row.names=FALSE)

#ok, now that I have this, need to find the most used category by author

combined_keywords$Keywords.strip = sapply(combined_keywords$Keywords,strip_string)

idx.of.cats = c(1,4,6:9)
category.vec = levels(combined_keywords$Categories)[idx.of.cats]
indices.of.combined.cats = list(list(from=2,to=c(1,2)),list(from=3,to=c(1,5)),list(from=5,to=c(2,5)))

mostUsedCategory <- function(name)
{
  #print(name)
  cat.counts = rep(0,length(levels(combined_keywords$Categories)))
  for(i in seq(all.profiles))
  {
    idx = which(all.profiles[[i]]$authors %in% name)
    idx2 = which(all.profiles[[i]]$profile[idx,] > 0)
    freq.temp = all.profiles[[i]]$profile[idx,idx2]
    words.temp = all.profiles[[i]]$words[idx2]
    cc.temp = combined_keywords$Categories[match(words.temp,combined_keywords$Keywords.strip)]
    cc.temp = levels(combined_keywords$Categories)[as.numeric(cc.temp)]
    #cc.temp = c(unlist(mapply(rep,x=cc.temp,each=freq.temp,simplify=FALSE)))
    cc.temp = table(factor(c(unlist(mapply(rep,x=cc.temp,each=freq.temp))),levels=levels(combined_keywords$Categories)))
    #cc.temp = table(unlist(mapply(rep,x=cc.temp,each=freq.temp,simplify=FALSE)))
    if(length(cc.temp) > 0)
      cat.counts = cat.counts + cc.temp
  }
  
  sepbicounts <- function(x)
  {
    y = rep(0,length(category.vec)) #6 single category counts
    
    for(i in seq(indices.of.combined.cats))
    {
      y[indices.of.combined.cats[[i]]$to] = y[indices.of.combined.cats[[i]]$to] + x[indices.of.combined.cats[[i]]$from]
    }
    names(y) = category.vec
    y
  }
  
  cat.counts = cat.counts[idx.of.cats] + sepbicounts(cat.counts)
  
  maxs = which(cat.counts == max(cat.counts))
  
  if(length(maxs) > 1)
    maxs = sample(maxs,1)
  
  return(names(cat.counts)[maxs])
}


node.data.english$most.used.category = sapply(node.data.english$Id,mostUsedCategory)

mostUsedCategory(node.data.english$Id[275])
name = node.data.english$Id[275]
name = node.data.english$Id[11928]
mostUsedCategory(name)

# Doing regression analysis ####

#find final list of matches for regression (pagerank , authors)

all.words = c(all.profiles[[1]]$words,all.profiles[[2]]$words,all.profiles[[3]]$words)
all.words = unique(all.words)

combined.keywords = read.csv('../data/combined_keywords.csv',as.is=TRUE)

#pagerank.xls = read.xlsx('../data/AllScores.xlsx',sheetIndex=1,startRow=1) #this is 12,000 entries, may take a while!
more.combined.keywords = read.xlsx('../data/List of Coimbined Keywords.xlsx',sheetIndex=1,startRow=1)

pagerank.names.english = sapply(pagerank.csv$Id,translatename2)

#need to remove duplicate pageranks
dup.names = which(table(pagerank.names.english) > 1)
dup.names = table(pagerank.names.english)[dup.names]

pagerank.names.english.nodup = pagerank.names.english
for(i in 1:length(dup.names))
{
  temp = which(pagerank.names.english.nodup == names(dup.names)[i])
  temp = temp[-sample(length(temp),1)]
  pagerank.names.english.nodup = pagerank.names.english.nodup[-temp]
}
print(sum((table(pagerank.names.english.nodup) > 1))) #0

final.dat = NULL
profile.dat = NULL

for(i in 1:length(all.profiles))
{
  pg.idx = which(pagerank.names.english.nodup %in% all.profiles[[i]]$authors)
  doc.idx = which(all.profiles[[i]]$authors %in% pagerank.names.english.nodup)
  
  match.idx = match(pagerank.names.english.nodup[pg.idx],all.profiles[[i]]$authors[doc.idx])
  
  print(paste("We matched",length(pg.idx),"of",length(all.profiles[[i]]$authors),"authors."))
  
  temp = data.frame(author=pagerank.names.english.nodup[pg.idx])
  temp = cbind(temp,pagerank.csv[pg.idx,c(2:3,8,9,10,12,15)])
  temp$num.papers = all.profiles[[i]]$num.papers[doc.idx[match.idx]]
  final.dat = rbind(final.dat,temp)
  
  #now need to create expnaded profile
  profile.idx = match(all.profiles[[i]]$words,all.words)
  temp2 = apply(all.profiles[[i]]$profile[doc.idx[match.idx],],1,function(x){y = rep(0,length(all.words));y[profile.idx] = x;y})
  profile.dat = rbind(profile.dat,t(temp2))
}

colnames(profile.dat) = all.words

save(final.dat,profile.dat,file="citationrank0.Rdata")
#2277 + 1288 + 33 - 2144
#[1] "We matched 1023 of 2277 authors."
#[1] "We matched 1092 of 1288 authors."
#[1] "We matched 29 of 33 authors."

#how many keywords do I match?
combined.keywords.strip = toupper(sapply(combined.keywords$Keywords,strip_string))

more.combined.keywords[,1] = toupper(sapply(more.combined.keywords[,1],strip_string))
more.combined.keywords[,2] = toupper(sapply(more.combined.keywords[,2],strip_string))

length(all.words) - sum(all.words %in% combined.keywords.strip) #998 - 926 = 72 missing

#now add all the new words in 2

getCombinedWordCount <- function(word)
{
  #find idxs of matches in all words
  #output column of sum across idxs
  #output idx of matches in all words
  
  idx.temp = which(more.combined.keywords[,2] %in% word)
  idx.temp = c(which(all.words %in% words),which(all.words %in% more.combined.keywords[idx.temp,1]))
  idx.temp = unique(idx.temp)
  
  word.counts = profile.dat[,idx.temp]
  
  if(length(idx.temp) > 1)
    word.counts = rowSums(word.counts)
  
  
  return(list(counts=word.counts,idxs=idx.temp))
}

profile.dat2 = NULL
idx.remove = NULL
idx.remove2 = NULL

for(word in unique(more.combined.keywords[,2]))
{
  temp = getCombinedWordCount(word)
  idx.remove = c(idx.remove,temp$idxs)
  profile.dat2 = cbind(profile.dat2,temp$counts)
  print(c(word,dim(temp$counts)))
  if(length(temp$idxs) == 0)
    idx.remove2 = c(idx.remove2,which(unique(more.combined.keywords[,2]) %in% word))
}

#rowsums should not be less
author.sums = rowSums(profile.dat)

profile.dat = cbind(profile.dat[,-idx.remove],profile.dat2)

which(rowSums(profile.dat) < author.sums) #none are less!
which(rowSums(profile.dat) > author.sums) #8 are greater, not too bad

author.sums = rowSums(profile.dat)

colnames(profile.dat) = c(all.words[-idx.remove],unique(more.combined.keywords[-idx.remove2,2]))

all.words = colnames(profile.dat)
#see how many we are missing now
sum(all.words %in% combined.keywords.strip) #there are finally 978 - 960 = 18 missing, not bad!
                          
all.words.categories = combined.keywords$Categories[match(all.words,combined.keywords.strip)]

category.dat = apply(profile.dat,1,function(z){aggregate(z,by=list(all.words.categories),FUN=sum)$x})
category.dat = t(category.dat)
colnames(category.dat) = aggregate(profile.dat[1,],by=list(all.words.categories),FUN=sum)$Group.1
category.dat[,1] = rowSums(category.dat[,1:3])
category.dat[,4] = rowSums(category.dat[,c(2,4)])
category.dat[,8] = rowSums(category.dat[,c(8,3,5)])
category.dat = category.dat[,c(1,4,6:9)]

final.dat = cbind(final.dat,category.dat)

save(final.dat,profile.dat,file="final_ranks_reg1.Rdata")

final.dat$PageRank = as.numeric(final.dat$PageRank)
plot(final.dat$PageRank,final.dat$num.papers,pch=16)

final.dat$Eigenvector.Centrality = as.numeric(final.dat$Eigenvector.Centrality)

getDivisions3 <- function(x,pseq=seq(0,1,.05))
{
  getCutoffs <- function(x,p1,p2)
  {
    if(p1 == 0)
    {
      x1 = -Inf
    } else
      x1 = quantile(x,p1)
    
    if(p2 == 1)
    {
      x2 = Inf
    } else
      x2 = quantile(x,p2)
    
    return(c(x1,x2))
  }
  
  p.mat = NULL
  
  for(i in 1:(length(pseq)-1))
  {
    temp = (i+1):length(pseq)
    if(i == 1)
      temp = temp[-length(temp)]
    
    p.mat = rbind(p.mat,cbind(rep(i,length(temp)),temp))
  }
  
  p.mat[,1] = pseq[p.mat[,1]]
  p.mat[,2] = pseq[p.mat[,2]]
  
  divs = mapply(getCutoffs,p.mat[,1],p.mat[,2],MoreArgs=list(x=x))
  
  res = cbind(p.mat,t(divs))
  res = data.frame(res)
  colnames(res) = c("plow","phigh","LB","UB")
  return(res)
}

regfromDivs <- function(y,X,div1,div2,penalize=FALSE)
{
  divs = c(div1,div2)
  y.cat = 1 * (y <= divs[1]) + 2 * ( (y > divs[1]) & (y <= divs[2]) ) + 3 * (y > divs[2])
  y.cat = factor(y.cat)
  X = as.matrix(X)
  
  #fit glm.net on y.cat ~ X
  if(penalize)
  {
    #fit = glmnet(X,y.cat,family="multinomial")
    cvfit = cv.glmnet(X, y.cat, family="multinomial")
    fit = cvfit$glmnet.fit
    idx1 = which(cvfit$lambda == cvfit$lambda.1se)
    if(idx1 == 1)
      idx1 = max(idx1,which(cvfit$lambda == cvfit$lambda.1se))
  } else
  {
    fit = glmnet(X,y.cat,family="multinomial",lambda=0)
    cvfit = NULL
    idx1 = 1 #may need to change this
  }
  
  #get out model.fit 
  
  #and F stat and p-value for smallest lambda
  dev = fit$dev.ratio[idx1] * fit$nulldev[idx1]
  p.val = 1 - pchisq(dev,fit$df[idx1])
  
  #and variance estimates for all the betas
  #by normal theory, the variance of each category of coefs is (X^T W X)^(-1)
  #With W a diagonal matrix of weights p_i (1 - p_i)
  
  phat = predict(fit,X,type="response")
  
  se.hat = list()
  
  for(i in 1:dim(phat)[2])
  {
    se.hat[[i]] = try(solve(t(X) %*% diag( phat[,i,idx1] * (1 - phat[,i,idx1])) %*% X ),silent=TRUE)
    if(!is.null(attr(se.hat[[i]],"class")))
    {
      if(attr(se.hat[[i]],"class") == "try-error")
        se.hat[[i]] = rep(0,dim(X)[1])
    }
  }
  
  return(list(fit=fit,p.val=p.val,se.hat=se.hat,cvfit=cvfit))
}

#system.time(regfromDivs(y,X,divs)) roughly half a second

getAllDivRegs <- function(yname="PageRank",xcols =10:15, dat=final.dat)
{
  y = dat[,yname]
  X = dat[,xcols]
  
  all.divs = getDivisions3(y)
  
  #all.regs = apply(all.divs[1:2,3:4],MARGIN=1,FUN=regfromDivs,y=y,X=X)
  all.regs = mapply(regfromDivs,div1=all.divs[,3],div2=all.divs[,4],MoreArgs=list(y=y,X=X),SIMPLIFY=FALSE)
  
  #need to backout all pvalues to add to all divs, return all regression results seperately
  p.vals = NULL
  for(i in 1:length(all.regs))
    p.vals = c(p.vals,all.regs[[i]]$p.val)
  
  all.divs$pval = p.vals
  
  return(list(P = all.divs,fits=all.regs))
}

#and now want to plot p.vals in a simplex
#want y axis to be width of middle interval
#and x axis width of left interval

#detach("package:ggplot2", unload=TRUE)

makeplotfromP <- function(P,plot.name="test",type="P")
{
  P.temp = P
  P.temp$y = P.temp$phigh - P.temp$plow
  P.temp$x = (P.temp$phigh + P.temp$plow) / 2
  
  if(type == "P")
  {
  p <- ggplot(P.temp, aes(x,y,colour=pval)) + geom_point(shape=15,size=5)
  p <- p + scale_color_gradient("P value",low = "steelblue", high = "white")
  p + labs(x = "Midpoint of Center Interval", y= "Width of Center Interval" , title = "") + theme_bw()
  } else
  {
    P.temp$qval = p.adjust(P.temp$pval,"BH")
    fdr.labels = c("FDR > .2","FDR <= .2","FDR <= .1","FDR <= .05")
    fdp.palette = c("white",cbPalette[c(5,2,7)])
    P.temp$qvalf = factor((P.temp$qval <= .2) + (P.temp$qval <= .1) + (P.temp$qval <= .05),levels=c(0,1,2,3),labels=fdr.labels)
    p <- ggplot(P.temp, aes(x,y,colour=qvalf)) + geom_point(shape=15,size=5)
    fdp.palette = fdp.palette[sort(unique(P.temp$qvalf))]
    fdr.labels = fdr.labels[sort(unique(P.temp$qvalf))]
    p <- p + scale_color_manual("FDR Threshold", values = fdp.palette,labels=fdr.labels)
    p + labs(x = "Midpoint of Center Interval", y= "Width of Center Interval" , title = "") + theme_bw()
  }
  fname = paste(plot.name,".eps",sep="")
  Sys.sleep(5)
  ggsave(file=fname,width=8,height=8)
}

#find P fits matrix for each ranking

#first I want to create normalize meme profiles
final.dat.norm = final.dat
final.dat.norm = final.dat.norm[-which(rowSums(final.dat.norm[,10:15]) == 0),]
final.dat.norm[,10:15] = final.dat.norm[,10:15] / rowSums(final.dat.norm[,10:15])

#redo linear models with profiles and normalized numpapers (9:15)
zero.meme.idx = c(1081,1591)
final.dat = final.dat[-which(rowSums(final.dat[,10:15]) == 0),]
final.dat$ppm = final.dat$num.papers / rowSums(final.dat[,10:15])

profile.dat.nonzero = profile.dat[-zero.meme.idx,]

#I fit linear regressions to explain 4 different measures of author rank by citations by their meme profile:
# cognitive             
# language              
# miscellaneous         
# professional          
# socio-cultural       
# training 

pagerank.lm = lm(PageRank ~ .,dat=final.dat.norm[,c(7,9:14)])
in.degree.lm = lm(In.Degree ~ .,dat=final.dat.norm[,c(2,9:14)])
out.degree.lm = lm(Out.Degree ~ .,dat=final.dat.norm[,c(3,9:14)])
evec.central.lm = lm(Eigenvector.Centrality ~ .,dat=final.dat.norm[,c(8,9:14)])

get.fp <- function(lm.obj)
{
  x = summary(lm.obj)
  pf(x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L],lower.tail = FALSE)
}

naive.fpvals <- unlist(lapply(list(pagerank.lm,in.degree.lm,out.degree.lm,evec.central.lm),get.fp))
#pvalues for each regression are
#0.34775470 0.18303430 0.01032645 0.17642589 

#So the ONLY significant model comes from using out.degree (the only one of the 4 pvalues above which is < .05)
#the results show that papers with 1 more cognitive and language terms are expected to have about 1.5 and 1.3 more citations to other papers

#search over breakpoints analysis ####

#Gini coefficient analysis ####

getGini <- function(x)
{
  x = sort(x)
  2 * sum(x * 1:length(x)) / ( length(x) * sum(x) ) - (length(x) + 1)/length(x)
}

getGini(final.dat$PageRank) # .28
getGini(final.dat$In.Degree) # .83
getGini(final.dat$Out.Degree) # .81
getGini(final.dat$Eigenvector.Centrality) # .84

topxpercent = cumsum(sort(final.dat$PageRank,decreasing=T)) / sum(final.dat$PageRank)
plot(topxpercent)

#We find that pagerank has the most equitable distribution of ranks, roughly equal to the amount of wage inequality in Belgium.
#On the other hand, the other ranks are much less balanced, with larger inequality than the wage inequality in any country measure by the World Bank.

#A better method to judge impact of author's meme profiles on their rankings is to divide author rankings into groups such as
#"High ranking", "Middle ranking", "Low ranking"

#We will search over the space of all such 3 category divisions, and use statistical analysis to find which divisions are most influenced by memes.

PageRank.allregs <- getAllDivRegs("PageRank",9:14,final.dat.norm)
InDegree.allregs <- getAllDivRegs("In.Degree",9:14,final.dat.norm)
OutDegree.allregs <- getAllDivRegs("Out.Degree",9:14,final.dat.norm)
EvecCentral.allregs <- getAllDivRegs("Eigenvector.Centrality",9:14,final.dat.norm)

setwd("/Users/leopekelis/Desktop/14_ziyun/citationrankfigs")
makeplotfromP(PageRank.allregs$P,"PageRank_pvals")
makeplotfromP(InDegree.allregs$P,"InDegree_pvals")
makeplotfromP(OutDegree.allregs$P,"OutDegree_pvals")
makeplotfromP(EvecCentral.allregs$P,"EvecCentral_pvals")

#insted make plots on Q value thresholds
makeplotfromP(PageRank.allregs$P,"PageRank_qvals",type="Q")
makeplotfromP(InDegree.allregs$P,"InDegree_qvals",type="Q")
makeplotfromP(OutDegree.allregs$P,"OutDegree_qvals",type="Q")
makeplotfromP(EvecCentral.allregs$P,"EvecCentral_qvals",type="Q")
setwd("/Users/leopekelis/Desktop/14_ziyun/R_work")

#actually, use K-medioids with a special dissimilarity matrix

getdistfromP <- function(P,wts=rep(1,3),recompute=TRUE)
{
  P.temp = P
  if(recompute)
  {
    P.temp$y = P.temp$phigh - P.temp$plow
    P.temp$x = (P.temp$phigh + P.temp$plow) / 2
    P.temp$qval = p.adjust(P.temp$pval,"BH")
  }
  
  return(daisy(P.temp[,c("x","y","qval")],metric="gower",weights=wts))
}

#gap statistic function for k-medioids
getWk <- function(pam.obj,D)
{
  getWm <- function(i)
  {
    idx.c = which(pam.obj$clustering == i)
    sum(as.matrix(D)[pam.obj$medoids[i],idx.c])
  }
  
  Wk = sapply(1:length(pam.obj$medoids),getWm)
  sum(Wk)
}

getnullWk <- function(P,k,wts)
{
  P$qval = runif(dim(P)[1],min(P$qval),max(P$qval))
  D = getdistfromP(P,wts,recompute=FALSE)
  pam.fit = pam(D,k)
  return(getWk(pam.fit,D))
}

gapMedoids <- function(P,wts=c(1,1,1),max.k=10,null.rep=25,D=NULL)
{
  if(is.null(D))
  {
    P$y = P$phigh - P$plow
    P$x = (P$phigh + P$plow) / 2
    P$qval = p.adjust(P$pval,"BH")
  
    D = getdistfromP(P,wts,recompute=FALSE)
  }
  
  getkDat <- function(k)
  {
    pam.fit = pam(D,k)
    Wk = getWk(pam.fit,D)
    nullWk = replicate(null.rep,getnullWk(P,k,wts))
    return(list(fit=pam.fit,Wk=Wk,nullWk=nullWk))
  }
  
  kDat.list = sapply(1:max.k,getkDat,simplify=FALSE)
  null.mu = NULL
  null.sd = NULL
  lWk = NULL
  for(i in 1:length(kDat.list))
  {
    null.mu = c(null.mu,mean(log(kDat.list[[i]]$nullWk)))
    null.sd = c(null.sd,sd(log(kDat.list[[i]]$nullWk)))
    lWk = c(lWk,log(kDat.list[[i]]$Wk))
  }
  
  return(list(Kdat=kDat.list,null.mu=null.mu,null.sd=null.sd,lWk=lWk))
}

PageRank.gap = gapMedoids(PageRank.allregs$P)

getGapStat <- function(gap.obj,plot.name=NULL)
{
  num.K = length(gap.obj$lWk)
  dat = data.frame(x = 1:num.K,mu0=gap.obj$null.mu,Wk=gap.obj$lWk,sd0=gap.obj$null.sd)
  
  if(!is.null(plot.name))
  {
    p <- ggplot(dat, aes(x=x)) + geom_line(aes(y=mu0-Wk),linetype=2) + geom_errorbar(aes(x=x,ymin=mu0-Wk-sd0,ymax=mu0-Wk+sd0,y=mu0-Wk),alpha=1,width=.2)
    p + labs(x = "Number of Clusters", y= "log Wk0 - log Wk" , title = "") + theme_bw()
    fname = paste(plot.name,".eps",sep="")
    Sys.sleep(5)
    ggsave(file=fname,width=8,height=8)
  }
  
  Gp = dat$mu0 - dat$Wk
  Gp1 = Gp[-1] + dat$sd0[-1]
  
  k.min = min(which(Gp[-length(Gp)] > Gp1))
  
  if(k.min == Inf)
  {
    Gp1 = Gp[-1] 
    k.min = min(which(Gp[-length(Gp)] > Gp1))
  }
  
  if(k.min == Inf)
  {
    Gp1 = Gp[-1] - dat$sd0[-1]
    k.min = min(which(Gp[-length(Gp)] > Gp1))
  }
  
  return(k.min)
}

setwd("/Users/leopekelis/Desktop/14_ziyun/citationrankfigs")
PR.best.clustering = getGapStat(PageRank.gap,"pagerank.gap")

FDRclustering <- function(P,k.max,wts=c(1,1,1),threshold=NULL,plot.name=NULL,gap.stat=TRUE,verbose=FALSE,null.rep=25)
{
  #setup
  P$y = P$phigh - P$plow
  P$x = (P$phigh + P$plow) / 2
  P$qval = p.adjust(P$pval,"BH")
  
  qvals = P$qval
  
  if(!is.null(threshold))
    P$qval = as.numeric(P$qval < threshold)
  
  D = getdistfromP(P,wts,recompute=FALSE)
  
  #find optimal number of clusters using gap statistic
  if(gap.stat)
  {
    if(verbose)
      print("Running Gap Statistic ...")
    
  gap.obj = gapMedoids(P,wts=wts,max.k=k.max,D=D,null.rep=null.rep)
  
  gap.plot.name = NULL
  if(!is.null(plot.name))
    gap.plot.name = paste(plot.name,"gap",sep="")
  
   k = min(getGapStat(gap.obj,plot.name=gap.plot.name),k.max)
  } else
    k = k.max
  
  if(verbose)
    print("Starting FDR Analysis ...")
  
  Km.temp = pam(D,k)
  P$clust = Km.temp$clustering
  P$tnc = P$clust
  
  #find expected number of non-nulls in each cluster, and cluster medoid location
  Nnn = NULL
  C.ctrs = NULL
  C.size = NULL
  for(i in 1:length(Km.temp$medoids))
  {
    idx = which(Km.temp$clustering == i)
    C.size = c(C.size,length(idx))
    
    #if(!is.null(threshold))
    #{
    #  maxq = 1 - max(P$qval[idx])*(1 - threshold)
    #} else
      maxq = max(qvals[idx])
    
    Nnn = c(Nnn,(1 - maxq) * length(idx))
    P$tnc[idx] = (1 - maxq) * length(idx)
    idx = Km.temp$medoids[i]
    C.ctrs = rbind(C.ctrs,c(P$x[idx],P$y[idx]))
  }
  
  C.ctrs = data.frame(x=C.ctrs[,1],y=C.ctrs[,2],size=C.size)
  
  if(!is.null(plot.name))
  {
    plot.name = paste(plot.name,"FDRclust",sep="")
    p <- ggplot(P, aes(x,y,colour=tnc)) + geom_point(shape=15,size=5)
    p <- p +  scale_color_gradient("Expected non-nulls",low = cbPalette[3], high = cbPalette[2])
    p <- p + geom_point(data=C.ctrs,aes(x=x,y=y),shape=8,size=10,col=cbPalette[8])
    p + labs(x = "Midpoint of Center Interval", y= "Width of Center Interval" , title = "") + theme_bw()
    fname = paste(plot.name,".eps",sep="")
    Sys.sleep(5)
    ggsave(file=fname,width=8,height=8)
  }
  return(list(C.ctrs = C.ctrs, Nnn = Nnn))
}

setwd("/Users/leopekelis/Desktop/14_ziyun/citationrankfigs")
#PageRank.FDR = FDRclustering(PageRank.allregs$P,k.max=10,wts=c(1,1,1),threshold=.1,plot.name="PageRank10",verbose=TRUE,gap.stat=FALSE)

#set a max cluster-size of 10
PageRank.FDR.gap = FDRclustering(PageRank.allregs$P,k.max=10,wts=c(1,1,1),threshold=.1,plot.name="PageRankfinal",verbose=TRUE,gap.stat=TRUE,null.rep=100)
InDegree.FDR = FDRclustering(InDegree.allregs$P,k.max=10,wts=c(1,1,1),threshold=.1,plot.name="InDegree",verbose=TRUE,gap.stat=TRUE,null.rep=100)
OutDegree.FDR = FDRclustering(OutDegree.allregs$P,k.max=10,wts=c(1,1,1),threshold=.1,plot.name="OutDegree",verbose=TRUE,gap.stat=TRUE,null.rep=100)

PageRank.FDR.05 = FDRclustering(PageRank.allregs$P,k.max=10,wts=c(1,1,1),threshold=.05,plot.name="PageRankfinal05",verbose=TRUE,gap.stat=TRUE,null.rep=100)
InDegree.FDR.05 = FDRclustering(InDegree.allregs$P,k.max=10,wts=c(1,1,1),threshold=.05,plot.name="InDegree05",verbose=TRUE,gap.stat=TRUE,null.rep=100)
OutDegree.FDR.05 = FDRclustering(OutDegree.allregs$P,k.max=10,wts=c(1,1,1),threshold=.05,plot.name="OutDegree05",verbose=TRUE,gap.stat=TRUE,null.rep=100)
#save(PageRank.FDR.gap,InDegree.FDR,OutDegree.FDR,file="cutpointFDR.Rdata")
save(PageRank.FDR.05,InDegree.FDR.05,OutDegree.FDR.05,file="cutpointFDR05.Rdata")


#based on this, I find which cutpoints to use for my final multinomial regressions, report coefficients, SE and p-vals

getFinalReg <- function(FDR.gap.obj,allregs,idx=NULL,X)
{  
  if(is.null(idx))
    idx = which.max(FDR.gap.obj$Nnn)
  
  x = FDR.gap.obj$C.ctrs$x[idx]
  y = FDR.gap.obj$C.ctrs$y[idx]
  
  P = allregs$P
  P$y = P$phigh - P$plow
  P$x = (P$phigh + P$plow) / 2
  
  idx = which((P$x == x) & (P$y == y))
  
  fit = allregs$fits[[idx]]
  fit$cutoffs = P[idx,c(1,2)]
  
  #return matrix with estimate, se, z.val, p.val
  coef.names = c("Intercept",names(X))
  X = as.matrix(X)
  phat = predict(fit$fit,X,type="response")
  X = cbind(rep(1,dim(X)[1]),X)
  se.hat = list()
  out.mat = list()
  
  for(i in 1:dim(phat)[2])
  {
    se.temp = solve(t(X) %*% diag( phat[,i,1] * (1 - phat[,i,1])) %*% X )
    se.hat[[i]] = se.temp
    se.temp = sqrt(diag(se.temp))
    coef.temp = c(fit$fit$a0[i],as.matrix(fit$fit$beta[[i]]))
    z.vals = coef.temp / se.temp
    p.vals = sapply(pt(z.vals,df=c(dim(X) %*% c(1,-1))-1),function(p){2*min(p,1-p)})
    out.mat[[i]] = data.frame(coef.temp,se.temp,z.vals,p.vals)
    row.names(out.mat[[i]]) = coef.names
  }
  
  fit$coefs = out.mat
  
  return(fit)
}

#PageRank.reg.final = getFinalReg(PageRank.FDR.gap,PageRank.allregs,X=final.dat.norm[,9:14])
#InDegree.reg.final = getFinalReg(InDegree.FDR,InDegree.allregs,X=final.dat.norm[,9:14])
#OutDegree.reg.final = getFinalReg(OutDegree.FDR,OutDegree.allregs,idx=4,X=final.dat.norm[,9:14])

PageRank.reg.final = getFinalReg(PageRank.FDR.05,PageRank.allregs,X=final.dat.norm[,9:14])
InDegree.reg.final = getFinalReg(InDegree.FDR.05,InDegree.allregs,X=final.dat.norm[,9:14])
OutDegree.reg.final = getFinalReg(OutDegree.FDR.05,OutDegree.allregs,idx=2,X=final.dat.norm[,9:14])
OutDegree.reg.final$cutoffs[1] = .60

save(PageRank.reg.final,InDegree.reg.final,OutDegree.reg.final,file="reg.final.Rdata")

#assume symmetric response, e.g. having a higher one of the three gives you a higher chance of being within that group
#and I'll also assume factor values increase in size, so small to med to large

#for the optimal cutoff, fit a multinomial lasso to the full profile and use cv

divs = quantile(final.dat.norm$PageRank,unlist(PageRank.reg.final$cutoffs))
PageRank.reg.profile = regfromDivs(final.dat.norm$PageRank,profile.dat.nonzero,divs[1],divs[2],penalize=TRUE)

divs = quantile(final.dat.norm$In.Degree,unlist(InDegree.reg.final$cutoffs))
InDegree.reg.profile = regfromDivs(final.dat.norm$In.Degree,profile.dat.nonzero,divs[1],divs[2],penalize=TRUE)

divs = quantile(final.dat.norm$Out.Degree,unlist(OutDegree.reg.final$cutoffs))
OutDegree.reg.profile = regfromDivs(final.dat.norm$Out.Degree,profile.dat.nonzero,divs[1],divs[2],penalize=TRUE)

save(PageRank.reg.profile,InDegree.reg.profile,OutDegree.reg.profile,file="reg_profile.Rdata")

PageRank.idx = c(20,264,70,121,165,175,810,879)
PageRank.reg.profile$fit$beta[[1]][c(20,264),4]
PageRank.reg.profile$fit$beta[[2]][c(70,121,165,175,810),4]
PageRank.reg.profile$fit$beta[[3]][c(879),4]
temp.idx = match(combined.keywords.strip,all.words[c(20,264,70,121,165,175,810,879)])
Pagerank.words = rbind(all.words[PageRank.idx[sort(temp.idx[!is.na(temp.idx)])]],combined.keywords$Categories[!is.na(temp.idx)][sort(temp.idx[!is.na(temp.idx)],index.return=T)$ix])
combined.keywords$Categories[which(combined.keywords.strip %in% all.words[PageRank.idx])]

InDegree.idx = list(which(InDegree.reg.profile$fit$beta[[1]][,6] != 0),which(InDegree.reg.profile$fit$beta[[2]][,6] != 0),which(InDegree.reg.profile$fit$beta[[3]][,6] != 0))

OutDegree.idx = list(which(OutDegree.reg.profile$fit$beta[[1]][,3] != 0),which(OutDegree.reg.profile$fit$beta[[2]][,3] != 0),which(OutDegree.reg.profile$fit$beta[[3]][,3] != 0))

for(i in 1:length(InDegree.idx))
{
  print(InDegree.reg.profile$fit$beta[[i]][InDegree.idx[[i]],6])
  temp.idx = match(combined.keywords.strip,all.words[InDegree.idx[[i]]])
  temp.cat = rbind(all.words[InDegree.idx[[i]][sort(temp.idx[!is.na(temp.idx)])]],combined.keywords$Categories[!is.na(temp.idx)][sort(temp.idx[!is.na(temp.idx)],index.return=T)$ix])
}

#print a list with Low Medium High entries
#each entry is a matrix with (word, dec/inc, categories)

getwordmatfromlasso <- function(dat,idx)
{
  word.idx = list(which(dat$fit$beta[[1]][,idx] != 0),which(dat$fit$beta[[2]][,idx] != 0),which(dat$fit$beta[[3]][,idx] != 0))
  
  word.list = NULL
  
  for(i in 1:length(word.idx))
  {
    if(length(word.idx[[i]]) > 0)
    {
      temp.sign = sign(dat$fit$beta[[i]][word.idx[[i]],idx])
      temp.idx = match(combined.keywords.strip,all.words[word.idx[[i]]])
      temp.cat = t(rbind(all.words[word.idx[[i]][sort(temp.idx[!is.na(temp.idx)])]],combined.keywords$Categories[!is.na(temp.idx)][sort(temp.idx[!is.na(temp.idx)],index.return=T)$ix]))
      temp.cat = aggregate(temp.cat[,2],by=list(temp.cat[,1]),FUN=paste,collapse=", ")
      
      temp.idx = 1
      if(length(temp.sign) > 1)
      {
        temp.idx = match(temp.cat$Group.1,names(temp.sign))
      } else
        names(temp.sign) = temp.cat$Group.1
      
      temp.cat = rbind(temp.sign,temp.cat$x[temp.idx])
      temp.idx = sort(temp.sign,index.return=TRUE)$ix
      rownames(temp.cat) = c("sign","category")
      colnames(temp.cat) = names(temp.sign)
      if(length(temp.idx) > 1)
        temp.cat = temp.cat[,temp.idx]
    } else
    {
      temp.cat = "none significant"
    }
    word.list[[i]] = temp.cat
  }
  
  names(word.list) = c("Low","Medium","High")
  return(word.list)
}

getwordmatfromlasso(InDegree.reg.profile,6)

#output summary tables for each of the final regressions

cutoff.mat = data.frame(rbind(PageRank.reg.final$cutoffs,InDegree.reg.final$cutoffs,OutDegree.reg.final$cutoffs))
cutoff.mat = cbind(cutoff.mat,mapply(function(x,y){signif(quantile(x,y),3)},x=list(final.dat$PageRank,final.dat$In.Degree,final.dat$Out.Degree),y=cutoff.mat[,1]))
cutoff.mat = cbind(cutoff.mat,mapply(function(x,y){signif(quantile(x,y),3)},x=list(final.dat$PageRank,final.dat$In.Degree,final.dat$Out.Degree),y=cutoff.mat[,2]))
cutoff.mat = cutoff.mat[,c(1,3,2,4)]
cutoff.mat[,c(1,3)] = cutoff.mat[,c(1,3)]*100
colnames(cutoff.mat) = c("Low Cutoff - %ile","Low Cutoff - Value","High Cutoff - %ile","High Cutoff - Value")
rownames(cutoff.mat) = c("Page Rank","In Degree","Out Degree")
print(cutoff.mat)

#correlation of network measures

round(cor(final.dat[,c("In.Degree","Out.Degree","PageRank","Eigenvector.Centrality")]),3)
