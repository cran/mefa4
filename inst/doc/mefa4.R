### R code from vignette source 'mefa4.Rnw'

###################################################
### code chunk number 1: mefa4.Rnw:51-52
###################################################
options(prompt = "R> ", continue = "+   ", useFancyQuotes = FALSE, width = 76)


###################################################
### code chunk number 2: mefa4.Rnw:74-82
###################################################
library(mefa)
x <- data.frame(
    sample = paste("Sample", c(1,1,2,2,3,4), sep="."),
    species = c(paste("Species", c(1,1,1,2,3), sep="."),  "zero.pseudo"),
    count = c(1,2,10,3,4,0),
    segment = letters[c(6,13,6,13,6,6)])
s <- stcs(x)
attributes(s)


###################################################
### code chunk number 3: mefa4.Rnw:86-92
###################################################
samp <- data.frame(samples=levels(x$sample), var1=1:2)
taxa <- data.frame(specnames=levels(x$species), var2=c("b","a"))
rownames(samp) <- samp$samples
rownames(taxa) <- taxa$specnames
(m <- mefa(s, samp, taxa))
m$xtab


###################################################
### code chunk number 4: mefa4.Rnw:102-106
###################################################
library(mefa4)
x0  <- Xtab(~ sample + species, x)
x1  <- Xtab(count ~ sample + species, x)
x11 <- Xtab(count ~ sample + species + segment, x)


###################################################
### code chunk number 5: mefa4.Rnw:111-114
###################################################
x2  <- Xtab(count ~ sample + species, x, cdrop=FALSE, rdrop=TRUE)
x21 <- Xtab(count ~ sample + species, x, cdrop=TRUE, rdrop=FALSE)
(x22 <- Xtab(count ~ sample + species, x, cdrop="zero.pseudo"))


###################################################
### code chunk number 6: mefa4.Rnw:124-125
###################################################
(x3 <- Mefa(x1, samp, taxa))


###################################################
### code chunk number 7: mefa4.Rnw:145-146
###################################################
(x4 <- Mefa(as.matrix(x1), samp[1:2,]))


###################################################
### code chunk number 8: mefa4.Rnw:149-151
###################################################
(x5 <- Mefa(x2, samp, taxa, join="inner"))
(x51 <- Mefa(x2, samp[1:2,], taxa, join="inner"))


###################################################
### code chunk number 9: mefa4.Rnw:154-155
###################################################
(x6 <- Mefa(x1))


###################################################
### code chunk number 10: mefa4.Rnw:163-165
###################################################
Melt(x1)
Melt(x11)


###################################################
### code chunk number 11: mefa4.Rnw:183-199
###################################################
as.stcs(x1)
as.mefa(x1)
as.stcs(x3)
a <- as.mefa(x3)
xtab(a)
samp(a)
taxa(a)
segm(a)
segm(x3)
as.Mefa(a)
as.Xtab(a)
s <- melt(a)
as.Xtab(s)
as.Mefa(s)
melt(x1)
melt(x3)


###################################################
### code chunk number 12: mefa4.Rnw:208-212
###################################################
xtab(x3)
x1[3,1] <- 999
xtab(x3) <- x1
xtab(x3)


###################################################
### code chunk number 13: mefa4.Rnw:216-221
###################################################
samp(x3)
samp(x3) <- NULL
samp(x3)
samp(x3) <- samp[1:3,]
samp(x3)


###################################################
### code chunk number 14: mefa4.Rnw:223-228
###################################################
taxa(x3)
taxa(x3) <- NULL
taxa(x3)
taxa(x3) <- taxa[1:3,]
taxa(x3)


###################################################
### code chunk number 15: mefa4.Rnw:232-235
###################################################
samp(x3)[1,]
samp(x3)[1,2] <- 3
samp(x3)[1,]


###################################################
### code chunk number 16: mefa4.Rnw:240-243
###################################################
x3[3:2, 1:2]
x3[3:2, ]
x3[ ,1:2]


###################################################
### code chunk number 17: mefa4.Rnw:249-257
###################################################
dim(x5)
dimnames(x5)
dn <- list(paste("S", 1:dim(x5)[1], sep=""),
    paste("SPP", 1:dim(x5)[2], sep=""))
dimnames(x5) <- dn
dimnames(x5)[[1]] <- paste("S", 1:dim(x5)[1], sep="_")
dimnames(x5)[[2]] <- paste("SPP", 1:dim(x5)[2], sep="_")
t(x5)


###################################################
### code chunk number 18: mefa4.Rnw:269-275
###################################################
groupSums(as.matrix(x2), 1, c(1,1,2))
groupSums(as.matrix(x2), 2, c(1,1,2,2))
groupSums(x2, 1, c(1,1,2))
groupSums(x2, 2, c(1,1,2,2))
groupSums(x5, 1, c(1,1,2))
groupSums(x5, 2, c(1,1,2,2))


###################################################
### code chunk number 19: mefa4.Rnw:279-285
###################################################
groupMeans(as.matrix(x2), 1, c(1,1,2))
groupMeans(as.matrix(x2), 2, c(1,1,2,2))
groupMeans(x2, 1, c(1,1,2))
groupMeans(x2, 2, c(1,1,2,2))
groupMeans(x5, 1, c(1,1,2))
groupMeans(x5, 2, c(1,1,2,2))


###################################################
### code chunk number 20: mefa4.Rnw:296-305
###################################################
x=matrix(1:4,2,2)
rownames(x) <- c("a", "b")
colnames(x) <- c("A", "B")
y=matrix(11:14,2,2)
rownames(y) <- c("b", "c")
colnames(y) <- c("B", "C")
mbind(x, y)
mbind(x, y, fill=0)
mbind(as(x, "sparseMatrix"), as(y, "sparseMatrix"))


###################################################
### code chunk number 21: mefa4.Rnw:314-322
###################################################
sampx <- data.frame(x1=1:2, x2=2:1)
rownames(sampx) <- rownames(x)
sampy <- data.frame(x1=3:4, x3=10:11)
rownames(sampy) <- rownames(y)
taxay <- data.frame(x1=1:2, x2=2:1)
rownames(taxay) <- colnames(y)
taxax <- NULL
mbind(Mefa(x, sampx), Mefa(y, sampy, taxay))


###################################################
### code chunk number 22: mefa4.Rnw:330-331
###################################################
data(abmibirds)


###################################################
### code chunk number 23: mefa4.Rnw:336-359
###################################################
b3 <- abmibirds
b3 <- b3[!(b3$Scientific.Name %in% c("VNA", "DNC", "PNA")),]
levels(b3$Scientific.Name)[levels(b3$Scientific.Name) 
    %in% c("NONE", "SNI")] <- "zero.pseudo"
b3$Counts <- ifelse(b3$Scientific.Name == "zero.pseudo", 0, 1)
b3$Label <- with(b3, paste(ABMI.Site, Year, 
    Point.Count.Station, sep="_"))
x3 <- b3[!duplicated(b3$Label), c("Label", 
    "ABMI.Site", "Year", "Field.Date", 
    "Point.Count.Station", "Wind.Conditions", "Precipitation")]
rownames(x3) <- x3$Label
z3 <- b3[!duplicated(b3$Scientific.Name), c("Common.Name",
    "Scientific.Name", "Taxonomic.Resolution", 
    "Unique.Taxonomic.Identification.Number")]
rownames(z3) <- z3$Scientific.Name
z3 <- z3[z3$Scientific.Name != "zero.pseudo",]
t31 <- system.time(s3 <- suppressWarnings(stcs(b3[,
    c("Label","Scientific.Name","Counts")])))
t32 <- system.time(m30 <- mefa(s3))
t33 <- system.time(m31 <- mefa(s3, x3, z3))
y30 <- m30$xtab
t34 <- system.time(m32 <- mefa(y30, x3, z3))
m32


###################################################
### code chunk number 24: mefa4.Rnw:363-384
###################################################
b4 <- abmibirds
b4$Label <- with(b4, paste(ABMI.Site, Year, 
    Point.Count.Station, sep="_"))
x4 <- b4[!duplicated(b4$Label), c("Label", "ABMI.Site", 
    "Year", "Field.Date", "Point.Count.Station",
    "Wind.Conditions", "Precipitation")]
rownames(x4) <- x4$Label
z4 <- b4[!duplicated(b4$Scientific.Name), c("Common.Name",
    "Scientific.Name", "Taxonomic.Resolution", 
    "Unique.Taxonomic.Identification.Number")]
rownames(z4) <- z4$Scientific.Name
t41 <- system.time(s4 <- Xtab(~ Label + Scientific.Name, 
    b4, cdrop = c("NONE", "SNI"), 
    subset = !(b4$Scientific.Name %in% c("VNA", "DNC", "PNA")), 
    drop.unused.levels = TRUE))
t42 <- system.time(m40 <- Mefa(s4))
t43 <- system.time(m41 <- Mefa(s4, x4, z4))
y40 <- as.matrix(m40@xtab)
t44 <- system.time(m42 <- Mefa(y40, x4, z4))
m42
sum(m42@xtab)


###################################################
### code chunk number 25: mefa4.Rnw:389-414
###################################################
res <- cbind("SIZE, *=3"=c("b*"=object.size(b3),
    "s*"=object.size(s3),
    "y*0"=object.size(y30),
    "m*0"=object.size(m30),
    "m*1"=object.size(m31),
    "m*2"=object.size(m32)),
"SIZE, *=4"=c("b*"=object.size(b4),
    "s*"=object.size(s4),
    "y*0"=object.size(y40),
    "m*0"=object.size(m40),
    "m*1"=object.size(m41),
    "m*2"=object.size(m42)),
"TIME, *=3"=c("b*"=NA,
    "s*"=t31[3],
    "y*0"=NA,
    "m*0"=t32[3],
    "m*1"=t33[3],
    "m*2"=t34[3]),
"TIME, *=4"=c("b*"=NA,
    "s*"=t41[3],
    "y*0"=NA,
    "m*0"=t42[3],
    "m*1"=t43[3],
    "m*2"=t44[3]))
(res <- cbind(res, "SIZE"=res[,2]/res[,1], "TIME"=res[,4]/res[,3]))


###################################################
### code chunk number 26: mefa4.Rnw:422-427
###################################################
stopifnot(identical(dim(y30), dim(y40)))
stopifnot(identical(setdiff(rownames(y30), rownames(y40)), character(0)))
stopifnot(identical(setdiff(rownames(y40), rownames(y30)), character(0)))
stopifnot(identical(setdiff(colnames(y30), colnames(y40)), character(0)))
stopifnot(identical(setdiff(colnames(y40), colnames(y30)), character(0)))


###################################################
### code chunk number 27: mefa4.Rnw:431-433
###################################################
system.time(xx3 <- aggregate(m31, "ABMI.Site"))
system.time(xx4 <- groupSums(m41, 1, m41@samp$ABMI.Site))


###################################################
### code chunk number 28: mefa4.Rnw:444-445
###################################################
toLatex(sessionInfo(), locale=FALSE)


