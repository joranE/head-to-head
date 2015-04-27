library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

tech_colors <- brewer.pal(n = 3,name = "Set1")
tech_colors <- setNames(tech_colors,c("C","F","FC"))
tech_labels <- c("Classic","Freestyle","Pursuit")
won_loss_colors1 <- brewer.pal(11,"RdBu")[c(1,11)]
won_loss_colors2 <- brewer.pal(11,"RdBu")[c(3:1,11:9)]
MAJ_INT <- c("WC","WSC","OWG","OWG")

plot_fis_dst <- function(ath1,ath2,by_tech = FALSE){
  if (length(ath2) == 1) ath2 <- c(ath2,ath2)
  if (missing(ath1) || missing(ath2) || ath1 == "" || ath2 == "") return(NULL)
  ath1_dat <- filter(DATA,name == ath1 & type == 'Distance') %>%
    collect()
  ath2_dat <- filter(DATA,name %in% ath2 & type == 'Distance') %>%
    collect()
  
  dat <- inner_join(ath1_dat,
                    ath2_dat,
                    by = "raceid")
  if (nrow(dat) == 0) return(NULL)
  dat_fis <- dat %>%
    mutate(fispoints.diff = fispoints.x - fispoints.y,
           name.y.lab     = paste("vs.",name.y)) %>%
    select(raceid,date.x,season.x,location.x,
           gender.x,length.x,tech.x,type.x,start.x,
           cat1.x,cat2.x,name.x,name.y,name.y.lab,
           fispoints.diff)
  
  if (by_tech){
    grp <- lapply(c("season.x","name.y.lab","tech.x"),as.symbol)
  }else{
    grp <- lapply(c("season.x","name.y.lab"),as.symbol)
  }
  
  dat_fis_med <- dat_fis %>%
    group_by_(.dots = grp) %>%
    summarise(med.fispoints.diff = median(fispoints.diff,na.rm = TRUE) * 1.0,
              n.fispoints = sum(!is.na(fispoints.diff))) %>%
    ungroup() %>%
    mutate(date.x = paste0(substr(season.x,6,10),"-01-01"))
  
  p <- ggplot(data = dat_fis,aes(x = as.Date(date.x),y = -fispoints.diff)) + 
    facet_wrap(~name.y.lab,ncol = 1) + 
    geom_hline(yintercept = 0,color = "black") + 
    geom_point() + 
    scale_x_date(breaks = date_breaks(width = "1 year")) +
    labs(x = NULL,y = paste("FIS Points: Opponent -",ath1))
  
  if (by_tech){
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.fispoints.diff,color = tech.x)) + 
      scale_color_manual(name = "Technique",values = tech_colors,labels = tech_labels)
  }else{
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.fispoints.diff),color = "blue")
  }
  
  p <- p + theme(legend.position = "bottom",
                 legend.direction = "horizontal",
                 axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                            size=8,color = "black"))
  p
}
plot_fis_spr <- function(ath1,ath2,by_tech = FALSE){
  if (length(ath2) == 1) ath2 <- c(ath2,ath2)
  if (missing(ath1) || missing(ath2) || ath1 == "" || ath2 == "") return(NULL)
  ath1_dat <- filter(DATA,name == ath1 & type == 'Sprint') %>%
    collect()
  ath2_dat <- filter(DATA,name %in% ath2 & type == 'Sprint') %>%
    collect()
  
  dat <- inner_join(ath1_dat,
                    ath2_dat,
                    by = "raceid")
  if (nrow(dat) == 0) return(NULL)
  dat_fis <- dat %>%
    mutate(rank.diff = rank.x - rank.y,
           name.y.lab     = paste("vs.",name.y)) %>%
    select(raceid,date.x,season.x,location.x,
           gender.x,length.x,tech.x,type.x,start.x,
           cat1.x,cat2.x,name.x,name.y,name.y.lab,
           rank.diff)
  
  if (by_tech){
    grp <- lapply(c("season.x","name.y.lab","tech.x"),as.symbol)
  }else{
    grp <- lapply(c("season.x","name.y.lab"),as.symbol)
  }
  
  dat_fis_med <- dat_fis %>%
    group_by_(.dots = grp) %>%
    summarise(med.rank.diff = median(rank.diff,na.rm = TRUE) * 1.0,
              n.rank = sum(!is.na(rank.diff))) %>%
    ungroup() %>%
    mutate(date.x = paste0(substr(season.x,6,10),"-01-01"))
  
  p <- ggplot(data = dat_fis,aes(x = as.Date(date.x),y = -rank.diff)) + 
    facet_wrap(~name.y.lab,ncol = 1) + 
    geom_hline(yintercept = 0,color = "black") + 
    geom_point() + 
    scale_x_date(breaks = date_breaks(width = "1 year")) +
    labs(x = NULL,y = paste("Finishing Place: Opponent -",ath1))
  
  if (by_tech){
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.rank.diff,color = tech.x)) + 
      scale_color_manual(name = "Technique",values = tech_colors,labels = tech_labels)
  }else{
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.rank.diff),color = "blue")
  }
  
  p <- p + theme(legend.position = "bottom",
                 legend.direction = "horizontal",
                 axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                            size=8,color = "black"))
  p
}

plot_maj_dst <- function(ath1,ath2,by_tech = FALSE){
  if (length(ath2) == 1) ath2 <- c(ath2,ath2)
  if (missing(ath1) || missing(ath2) || ath1 == "" || ath2 == "") return(NULL)
  ath1_dat <- filter(DATA,name == ath1 & type == 'Distance' & cat1 %in% MAJ_INT) %>% collect()
  ath2_dat <- filter(DATA,name %in% ath2 & type == 'Distance' & cat1 %in% MAJ_INT) %>% collect()
  
  dat <- inner_join(ath1_dat,
                    ath2_dat,
                    by = "raceid")
  if (nrow(dat) == 0) return(NULL)
  dat_fis <- dat %>%
    mutate(mpb.diff = mpb.x - mpb.y,
           name.y.lab     = paste("vs.",name.y)) %>%
    select(raceid,date.x,season.x,location.x,
           gender.x,length.x,tech.x,type.x,start.x,
           cat1.x,cat2.x,name.x,name.y,name.y.lab,
           mpb.diff)
  
  if (by_tech){
    grp <- lapply(c("season.x","name.y.lab","tech.x"),as.symbol)
  }else{
    grp <- lapply(c("season.x","name.y.lab"),as.symbol)
  }
  
  dat_fis_med <- dat_fis %>%
    group_by_(.dots = grp) %>%
    summarise(med.mpb.diff = median(mpb.diff,na.rm = TRUE) * 1.0,
              n.mpb = sum(!is.na(mpb.diff))) %>%
    ungroup() %>%
    mutate(date.x = paste0(substr(season.x,6,10),"-01-01"))
  
  p <- ggplot(data = dat_fis,aes(x = as.Date(date.x),y = -mpb.diff)) + 
    facet_wrap(~name.y.lab,ncol = 1) + 
    geom_hline(yintercept = 0,color = "black") + 
    geom_point() + 
    scale_x_date(breaks = date_breaks(width = "1 year")) +
    labs(x = NULL,y = paste("sMPB: Opponent -",ath1))
  
  if (by_tech){
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.mpb.diff,color = tech.x)) + 
      scale_color_manual(name = "Technique",values = tech_colors,labels = tech_labels)
  }else{
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.mpb.diff),color = "blue")
  }
  
  p <- p + theme(legend.position = "bottom",
                 legend.direction = "horizontal",
                 axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                            size=8,color = "black"))
  p
}
plot_maj_spr <- function(ath1,ath2,by_tech = FALSE){
  if (length(ath2) == 1) ath2 <- c(ath2,ath2)
  if (missing(ath1) || missing(ath2) || ath1 == "" || ath2 == "") return(NULL)
  ath1_dat <- filter(DATA,name == ath1 & 
                       type == 'Sprint' & 
                       cat1 %in% MAJ_INT) %>%
    collect()
  ath2_dat <- filter(DATA,name %in% ath2 & 
                       type == 'Sprint' & 
                       cat1 %in% MAJ_INT) %>%
    collect()
  
  dat <- inner_join(ath1_dat,
                    ath2_dat,
                    by = "raceid")
  if (nrow(dat) == 0) return(NULL)
  dat_fis <- dat %>%
    mutate(rank.diff = rank.x - rank.y,
           name.y.lab     = paste("vs.",name.y)) %>%
    select(raceid,date.x,season.x,location.x,
           gender.x,length.x,tech.x,type.x,start.x,
           cat1.x,cat2.x,name.x,name.y,name.y.lab,
           rank.diff)
  
  if (by_tech){
    grp <- lapply(c("season.x","name.y.lab","tech.x"),as.symbol)
  }else{
    grp <- lapply(c("season.x","name.y.lab"),as.symbol)
  }
  
  dat_fis_med <- dat_fis %>%
    group_by_(.dots = grp) %>%
    summarise(med.rank.diff = median(rank.diff,na.rm = TRUE) * 1.0,
              n.rank = sum(!is.na(rank.diff))) %>%
    ungroup() %>%
    mutate(date.x = paste0(substr(season.x,6,10),"-01-01"))
  
  p <- ggplot(data = dat_fis,aes(x = as.Date(date.x),y = -rank.diff)) + 
    facet_wrap(~name.y.lab,ncol = 1) + 
    geom_hline(yintercept = 0,color = "black") + 
    geom_point() + 
    scale_x_date(breaks = date_breaks(width = "1 year")) +
    labs(x = NULL,y = paste("Finishing Place: Opponent -",ath1))
  
  if (by_tech){
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.rank.diff,color = tech.x)) + 
      scale_color_manual(name = "Technique",values = tech_colors,labels = tech_labels)
  }else{
    p <- p + 
      geom_line(data = dat_fis_med,aes(y = -med.rank.diff),color = "blue")
  }
  
  p <- p + theme(legend.position = "bottom",
                 legend.direction = "horizontal",
                 axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                            size=8,color = "black"))
  p
}

won_loss <- function(ath1,ath2,maj_int = FALSE){
  if (length(ath2) == 1) ath2 <- c(ath2,ath2)
  if (maj_int){
    ath1_dat <- filter(DATA,name == ath1 & cat1 %in% MAJ_INT) %>% collect()
    ath2_dat <- filter(DATA,name %in% ath2 & cat1 %in% MAJ_INT) %>% collect()
  }else{
    ath1_dat <- filter(DATA,name == ath1) %>% collect()
    ath2_dat <- filter(DATA,name %in% ath2) %>% collect()
  }
  
  dat <- inner_join(ath1_dat,
                    ath2_dat,
                    by = "raceid")
  dat_fis <- dat %>%
    mutate(won.loss = c("Loss","Tie","Win")[sign(rank.y - rank.x) + 2],
           name.y.lab     = paste("vs.",name.y)) %>%
    select(raceid,date.x,season.x,location.x,
           gender.x,length.x,tech.x,type.x,start.x,
           cat1.x,cat2.x,name.x,name.y,name.y.lab,
           won.loss)
  dat_fis
}

won_loss_plot <- function(ath1,ath2,by_tech = FALSE,maj_int = FALSE){
  if (missing(ath1) || missing(ath2) || ath1 == "" || ath2 == "") return(NULL)
  commapos <- function(x, ...) {
    format(abs(x), big.mark = ",", trim = TRUE,
           scientific = FALSE, ...)
  }
  if (by_tech){
    grp1 <- lapply(c("season.x","type.x","name.y.lab","tech.x","won.loss"),
                  as.symbol)
    grp2 <- lapply(c("tech.x","won.loss"),
                   as.symbol)
  }else{
    grp1 <- lapply(c("season.x","type.x","name.y.lab","won.loss"),
                  as.symbol)
    grp2 <- as.symbol("won.loss")
  }
  
  dat <- won_loss(ath1,ath2,maj_int = maj_int)
  if (nrow(dat) == 0) return(NULL)
  dat <- dat %>% 
    filter(type.x != 'Stage') %>%
    group_by_(.dots = grp1) %>% 
    summarise(n = n())
  
  rng <- dat %>% 
    group_by(season.x,won.loss) %>% 
    summarise(mx = sum(n)) %>%
    group_by(won.loss,add = FALSE) %>%
    summarize(mx = max(mx))
  b <- sort(unique(dat$season.x))
  
  if (by_tech){
    dat$tech.long <- setNames(tech_labels,c("C","F","FC"))[dat$tech.x]
    dat$won.loss.tech <- with(dat,paste(tech.long,won.loss))
    dat$won.loss.tech <- factor(dat$won.loss.tech,
                                levels = rev(c("Classic Win",
                                           "Freestyle Win",
                                           "Pursuit Win",
                                           "Pursuit Loss",
                                           "Freestyle Loss",
                                           "Classic Loss")))
    
    p <- ggplot(data = dat,aes(x = season.x)) + 
      facet_grid(name.y.lab ~ type.x) + 
      geom_bar(data = filter(dat,won.loss == 'Win'),
               aes(y = n,fill = won.loss.tech),
               stat = "identity",position = "stack") + 
      geom_bar(data = filter(dat,won.loss == 'Loss'),
               aes(y = -n,fill = won.loss.tech),
               stat = "identity",position = "stack") +
      geom_hline(yintercept = 0,color = "black") +
      labs(x = NULL,y = "Won-Loss Record",fill = "") +
      scale_x_discrete(breaks = b[seq(1,length(b),by = 2)]) +
      scale_y_continuous(breaks = seq(from = -rng$mx[1],to = rng$mx[2],by = 5),
                         labels = commapos) + 
      scale_fill_manual(values = setNames(won_loss_colors2,levels(dat$won.loss.tech)),
                        breaks = rev(c("Pursuit Loss",
                                   "Freestyle Loss",
                                   "Classic Loss",
                                   "Classic Win",
                                   "Freestyle Win",
                                   "Pursuit Win"))) +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                       size=7,color = "black")) +
      guides(fill = guide_legend(reverse = TRUE,nrow = 2,byrow = TRUE))
  }else{
    p <- ggplot(data = dat,aes(x = season.x)) + 
      facet_grid(name.y.lab ~ type.x) + 
      geom_bar(data = filter(dat,won.loss == 'Win'),
               aes(y = n,fill = won.loss),
               stat = "identity",position = "stack") + 
      geom_bar(data = filter(dat,won.loss == 'Loss'),
               aes(y = -n,fill = won.loss),
               stat = "identity",position = "stack") +
      geom_hline(yintercept = 0,color = "black") +
      labs(x = NULL,y = "Won-Loss Record",fill = "") +
      scale_x_discrete(breaks = b[seq(1,length(b),by = 2)]) +
      scale_y_continuous(breaks = seq(from = -rng$mx[1],to = rng$mx[2],by = 5),
                         labels = commapos) + 
      scale_fill_manual(values = won_loss_colors1) +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.text.x = element_text(hjust=0,vjust=1,angle=310,
                                       size=7,color = "black")) +
      guides(fill = guide_legend(reverse = TRUE)) 
  }
  p
}
  