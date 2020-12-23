library(readxl)
options(scipen=999)




MS.t0 <- read_excel("MS t0.xlsx")
MS.t1 <- read_excel("MS t1.xlsx")

MS.t0 <- MS.t0[,2:7]
MS.t1 <- MS.t1[,2:7]


s_hat.t0 <- MS.t0[6,(1:5)]
s_hat.t1 <- MS.t1[4,(1:5)]









x.0 <- read_excel("Characteristics.xlsx")
x.0 <- t(x.0)
colnames(x.0) <- x.0[1,]
x.0 <- x.0[2:5,]
MSt0<- NA
x.0 <- cbind(x.0,MSt0)
x.0 <- as.data.frame(x.0)
outside <- NA
x.0 <- rbind(x.0,outside)
row.names(x.0)[5] <- "Outside" 
colnames(x.0)[22] <- "s_hat"

for(i in 1:ncol(x.0)){
  
  x.0[,i] <- as.numeric(levels(x.0[,i]))[x.0[,i]]
  
}

s_hat.t0 <- as.data.frame(s_hat.t0)
x.0$s_hat <- t(s_hat.t0)
s_0.t0 <- x.0[5,22]
s_0.t0 <- as.numeric(s_0.t0)
x.0 <- x.0[1:4,]





x.1 <- read_excel("Characteristics.xlsx")
x.1 <- t(x.1)
colnames(x.1) <- x.1[1,]
x.1 <- x.1[2:5,]
MSt1<- NA
x.1 <- cbind(x.1,MSt1)
x.1 <- as.data.frame(x.1)

for(i in 1:ncol(x.1)){
  
  x.1[,i] <- as.numeric(levels(x.1[,i]))[x.1[,i]]
  
}

x.1[3,21] <- 1
x.1 <- rbind(x.1,outside)
row.names(x.1)[5] <- "Outside"
colnames(x.1)[22] <- "s_hat"

s_hat.t1 <- as.data.frame(s_hat.t1)
x.1$s_hat <- t(s_hat.t1)

s_1.t1 <- x.1[5,22]
s_0.t1 <- as.numeric(s_1.t1)
x.1 <- x.1[1:4,]



x <- rbind(x.0,x.1)
x$s_hat <- as.numeric(x$s_hat)










#Preparing for OLS

log_s0.t0 <- log(s_0.t0)
log_s0.t1 <- log(s_0.t1)

log <- NA
x <- cbind.data.frame(x,log)

##################t0
for(i in 1:4){
  
  x$log[i] <- log(x$s_hat[i])-log_s0.t0
  
}

x$log[1] <- log(x$s_hat)-log_s0.t0




##################t1
for(i in 5:8){
  
  x$log[i] <- log(x$s_hat[i])-log_s0.t1
  
}
x <- x[c(1,3,4,5,6,7,8),]

colnames(x)[15] <- "ThreeD"

lm(log ~ Vol + Alone + grams + TV + Resolution + Wireless + Portable + Motion + Touch + LiveTV + Voice + StreamService 
   + Bluray + DVD + ThreeD + Storage + External + Titles + PC + Price + Subscribe, data = newx)

lm(log ~ Vol  + grams + Storage + Titles + PC + Price + Subscribe, data = x)  

newx <- read_excel("new x.xlsx")

lm(log ~ Vol + Alone + grams + Titles + Storage + newx$`RAM (MB)` + newx$`CPU (MHz)` + Price + Subscribe, data = newx)

lm(log ~ Vol + Alone + grams + Titles + Storage  + Price + Subscribe, data = newx)

m1 <- lm(log ~ Vol + grams + Titles + Storage + newx$`RAM (MB)` + newx$`CPU (MHz)` + Price + Subscribe, data = newx)
m2 <- lm(log ~Titles + Storage + newx$`RAM (MB)` + newx$`CPU (MHz)` + Price + Subscribe, data = newx)

require(stargazer)

stargazer(m1, type="text")

colnames(newx)[16] <- "ThreeD"

require(plm)

p.newx <- pdata.frame(newx, index = c("id", "year"))

colnames(p.newx)[c(10,11,12)] <- c("RAM", "CPU", "GPU")

fe1 <- plm(log ~ Vol + grams + Titles + Storage + newx$`RAM (MB)` + newx$`CPU (MHz)` + Price + Subscribe, data = p.newx,
           model = "within",
           effect = "twoways")

fe2 <- plm(log ~ Vol + grams + Titles + Storage + RAM + CPU + GPU + Price + Subscribe + Exclusive, data = p.newx,
           model = "within",
           effect = "twoways")

fe3 <- plm(log ~ Titles + Storage + RAM + CPU + GPU + Subscribe + Price + Exclusive , data = p.newx,
           model = "within",
           effect = "twoways")

stargazer(fe2, type="text")
stargazer(fe3, type="text")



fe_no.RAM <- plm(log ~  CPU + GPU + Subscribe + Price, data = p.newx,
           model = "within",
           effect = "twoways")

stargazer(fe_no.RAM, type="text")


fe_no.CPU <- plm(log ~  RAM + GPU + Subscribe + Price, data = p.newx,
                 model = "within",
                 effect = "twoways")

fe_no.GPU <- plm(log ~  RAM + CPU + Subscribe + Price, data = p.newx,
                 model = "within",
                 effect = "twoways")

#RAM and/or CPU and Subscribe are highly multicollinear

stargazer(fe_no.RAM,fe_no.CPU, fe_no.GPU, type="text")

fe_no.CPU1 <- plm(log ~  Storage + Titles + GPU + Exclusive + Subscribe + Price, data = p.newx,
                 model = "within",
                 effect = "twoways")


stargazer(fe_no.CPU, fe_no.CPU1, type="text")



x2 <- read_excel("x2.xlsx")
p.x2 <- pdata.frame(x2, index = c("id", "year"))
colnames(p.x2)[c(10,11,12)] <- c("RAM", "CPU", "GPU")
colnames(x2)[c(10,11,12)] <- c("RAM", "CPU", "GPU")







fe.x2 <- plm(log ~  CPU + RAM + GPU + Titles + Exclusive + Subscribe + Price 2, data = p.x2,
                 model = "within",
                 effect = "twoways")

fe.x2 <- plm(log ~ Vol + grams  + CPU + RAM + GPU + Titles + Exclusive + Subscribe + Price2 , data = p.x2,
             model = "within",
             effect = "twoways")


stargazer(
  plm(log ~ Vol + grams  + CPU + RAM + GPU + Titles + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + RAM + GPU + Titles  + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + RAM + GPU + Exclusive  + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~ CPU + RAM + Core +  Titles + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways")
  
  , type="html",
  out = "FE.html")


stargazer(

  plm(log ~  CPU + RAM + GPU + Exclusive  + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + GPU  + Core + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + RAM + GPU + Titles  + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~ Vol + grams  + CPU + RAM + GPU + Titles + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  
  df=F,
  font.size = "normalsize"
  
  )

stargazer(
  
  plm(log ~  Vol + grams + CPU + RAM + GPU + Titles + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + RAM + GPU + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~  CPU + GPU + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  plm(log ~ Vol + grams  + CPU + RAM + GPU + Titles + Exclusive + Subscribe + Price2 , data = p.x2,
      model = "within",
      effect = "twoways"),
  
  
  df=F
  
)


#IV estimation
stage1 <- lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core, data = x2)
stage1 <- lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles  + Core, data = x2)



stargazer(lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe, data = x2),
          
          lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles  + Core + Subscribe, data = x2),
          
          lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Core + Subscribe, data = x2),
          
          lm(Price2 ~ CPUcost + DRAMcost + RAM + GPU + Core + Subscribe, data = x2),
          
          type = "text",
          df = F)

require(AER)



stargazer( 
  ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe, data = x2),
  
  ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe , data = x2),
  
  ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe |
          CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe, data = x2),
  
  
  
  ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe, data = x2),
  
  omit.stat = "rsq",
  df = F,
  type="text"
  
)

print(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))



#F = 9.2224
IV1 <- ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe , data = x2)

print(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))

stargazer(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))

#F = 8.7123
ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe , data = x2)

print(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))


res.IV1 <- as.data.frame(resid(IV1))


x3 <- cbind(x2[,(1:13)],x2[,15],x2[,17],x2[,19],x2[,21],x2[,27])

x3 <- cbind(x3, unlist(res.IV1))

colnames(x3) <- c(colnames(x3)[1:18],"res.IV1")

j.reg1 <- lm(res.IV1 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe , data = x3) 
stargazer(j.reg1)


#robust SE
install.packages("ivpack")
library(ivpack)

robust.se(IV1)



stargazer(
  robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe, data = x2)),
  robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Storage + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Storage + Subscribe , data = x2)),
  robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe |
                    CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe, data = x2)),
  robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe, data = x2)),
  type = "text"
)


stargazer(
  
  ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe, data = x2),
  ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Storage + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Storage + Subscribe , data = x2),
  ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe |
          CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe, data = x2),
  ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe, data = x2),
  
  se = list(rse1,rse2,rse3,rse4),
  
  df=F
)


#robust.se
rse1 <- robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe, data = x2))[,2]
rse2 <- robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Storage + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Storage + Subscribe , data = x2))[,2]
rse3 <- robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe |
                          CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe, data = x2))[,2]
rse4 <- robust.se(ivreg(log ~ Price2 + CPU + RAM + GPU + Titles + Subscribe | CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe, data = x2))[,2]


stargazer(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))


stargazer(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Storage + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))


stargazer(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Exclusive + Storage + Core + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))

stargazer(waldtest(
  
  lm(Price2 ~ CPUcost + DRAMcost + CPU + RAM + GPU + Titles + Subscribe , data = x2),
  
  .~. -(CPUcost + DRAMcost)))
