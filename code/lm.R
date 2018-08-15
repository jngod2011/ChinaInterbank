 age=18:29    #年龄从18到29岁

 height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5)

 plot(age,height,main = "身高与年龄散点图") 
 lm.reg <- lm(height~age)  #建立回归方程

 lm.reg

 abline(lm.reg)    #画出拟合的线性回归线

 for (i in 1:length(vecm.myl.gir)) {
   y <- vecm.myl.gir[[i]] %>% unlist %>% as.numeric
   x1 <- loan.network[[i]] %>% unlist %>% as.numeric
   x2 <- deposit.network[[i]] %>% unlist %>% as.numeric
   lm.reg <- lm(y~x1+x2)
   
   print(summary(lm.reg))
   print("##")
 }

 y <- vecm.myl.gir %>% unlist
 x1 <- loan.network.so %>% unlist %>% as.numeric
 x2 <- loan.network.je %>% unlist %>% as.numeric
 x3 <- loan.network.ot %>% unlist %>% as.numeric
 x4 <- x1+x2+x3
 lm.reg <- lm(y~x4)
 print(lm.reg)
summary(lm.reg)
