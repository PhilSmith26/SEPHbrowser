# Make_chrtM08() function - SEPH employment, AWE, AHE and AWH by industry, SA
# August 3, 2021; improved August 9, 2021

Make_chrtM08 <- function(NAICS1,Est1,Typ1,type,month1,month2,altTitl,interv) {
  #browser()
  q0 <- readRDS(paste0("rds/",TS[[8]]$STCno,".rds"))
  q0 <- filter(q0,EST==Est1)
  q0 <- filter(q0,TYP==Typ1)
  q0 <- select(q0,Date,NAICS,VALUE)
  q0 <- pivot_wider(q0,names_from=NAICS,values_from=VALUE)
  # Check for NA's at start of this series and change start date if necessary
  tmp1 <- mutate(q0,val=.data[[NAICS1]])
  tmp1 <- filter(tmp1,Date>=month1 & Date<=month2)
  n <- nrow(tmp1)
  i <- 1
  while (is.na(tmp1$val[i]) & i<n) {i <- i+1}
  month1 <- tmp1$Date[i]
  
  if (altTitl=="") {ChrtTitl <- NAICS1}
  if (altTitl!="") {ChrtTitl <- altTitl}
  Fmth <- format(month1,"%b %Y")
  Lmth <- format(month2,"%b %Y")
  if (type==1) {
    q1 <- mutate(q0,val=.data[[NAICS1]])
    q1 <- filter(q1,Date>=month1 & Date<=month2)
    seas3 <- TS[[8]]$Seas
    MYsubtitl=paste0(Est1,", ",Typ1,"\nMonthly, ",Fmth," to ",Lmth,"\n",seas3)
    c1 <- ggplot(q1,
      aes(x=Date,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(labels=scales::"comma",position="right")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==2) {
    q1 <- mutate(q0,val=.data[[NAICS1]])
    q1 <- filter(q1,Date>=month1 & Date<=month2)
    seas3 <- TS[[8]]$Seas
    MYsubtitl=paste0("Including trend line\n",Est1,", ",Typ1,"\nMonthly, ",
      Fmth," to ",Lmth,"\n",seas3)
    c1 <- ggplot(q1,
      aes(x=Date,y=val))+
      geom_line(colour="black",size=1.5)+
      geom_smooth(method="lm",se=FALSE,linetype="dashed",na.rm=TRUE)+
      scale_y_continuous(labels=scales::"comma",position="right")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==3) {
    q0 <- filter(q0,Date>=month1 & Date<=month2)
    q1 <- mutate(q0,val=.data[[NAICS1]])
    seas3 <- TS[[8]]$Seas
    q1 <- mutate(q1,val=IDX(val))
    MYsubtitl=paste0("Index with starting month = 100\n",Est1,", ",Typ1,
      "\nMonthly, ",Fmth," to ",Lmth,"\n",seas3)
    c1 <- ggplot(q1,
      aes(x=Date,y=val))+
      geom_line(colour="black",size=1.5)+
      scale_y_continuous(position="right")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  } else if (type==4) {
    q1 <- mutate(q0,val=.data[[NAICS1]])
    q1 <- filter(q1,Date>=month1 & Date<=month2)
    seas3 <- TS[[8]]$Seas
    q1 <- mutate(q1,val=PC(val))
    MYsubtitl=paste0("One-month percentage change\n",Est1,", ",Typ1,
      "\nMonthly, ",Fmth," to ",Lmth,"\n",seas3)
    c1 <- ggplot(q1,
      aes(x=Date,y=val/100))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent",position="right")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
 } else if (type==5) {
    q1 <- mutate(q0,val=.data[[NAICS1]])
    q1 <- filter(q1,Date>=month1 & Date<=month2)
    seas3 <- TS[[8]]$Seas
    q1 <- mutate(q1,val=PC12(val))
    MYsubtitl=paste0("Twelve-month percentage change\n",Est1,", ",Typ1,
      "\nMonthly, ",Fmth," to ",Lmth,"\n",seas3)
    c1 <- ggplot(q1,
      aes(x=Date,y=val/100))+
      geom_col(fill="gold",colour="black",size=0.2)+
      scale_y_continuous(labels=scales::"percent",position="right")
    if(posNeg(q1$val)) {
      c1 <- c1+ geom_hline(yintercept=0,size=0.4,colour="black",
        linetype="dashed")
    }
  }
  if (datDif(month1,month2)>20 & interv=="") {
    interv <- "36 months"
  } else if (datDif(month1,month2)>10 & interv=="") {
    interv <- "12 months"
  } else if (datDif(month1,month2)>5 & interv=="") {
    interv <- "3 months"
  } else if (datDif(month1,month2)>2 & interv=="") {
    interv <- "2 months"
  } else if (interv=="") {
    interv <- "1 month"
  }
  c1 <- c1 + scale_x_date(breaks=seq.Date(month1,month2,by=interv))+
    labs(title=ChrtTitl,subtitle=paste0(MYsubtitl),
      caption=TS[[8]]$Ftnt,x="",y="")+
    theme(axis.text.y = element_text(size=18))+
    theme_DB()
  c1
}

