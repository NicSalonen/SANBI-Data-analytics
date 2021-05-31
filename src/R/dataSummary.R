dataSummary <- function(data, species){

data_file <- read.csv(data, stringsAsFactors = FALSE)

date <- as.character(data_file$date)
for (d in 1:length(date)){
    if (nchar(date[d]) > 10){
        date[d] <- substr(date[d], 4, 13)
    } else {
            next
    }
}
tot_obs <- length(date)
dDate <- as.Date(date, format = "%d/%m/%Y") # output y m d
yrs <- as.numeric(format(dDate, format = '%Y'))
yrs_fac <- factor(format(dDate, format = '%Y'))
un_yrs <- as.data.frame(table(yrs))
un_date_count <- as.data.frame(table(dDate))
un_date_count$dDate <- as.Date(un_date_count$dDate)
un_date_count$Month <- as.Date(cut(un_date_count$dDate, breaks = "month"))
monthly_count <- aggregate(un_date_count["Freq"], by=un_date_count["Month"], sum)
months <- seq(ymd(monthly_count$Month[1]),
                    ymd(monthly_count$Month[nrow(monthly_count)]), by = "months")


ht <- data_file$ht
if (is.character(ht)){
    ht <- as.numeric(ht)
}
bd1 <- data_file$bd1
if (is.character(bd1)){
    bd1 <- as.numeric(bd1)
}

df <- data.frame(dDate, yrs, ht, bd1)

buds <- as.factor(data_file$buds)
flowers <- as.factor(data_file$flowers)
pods <- as.factor(data_file$pods)
seeds <- as.factor(data_file$seeds)
resprout <- as.factor(data_file$resprout)

yrly_buds <- as.data.frame(table(yrs, buds))
if (nrow(yrly_buds) > 0){
    if (length(levels(yrly_buds$buds)) < 2){
        yrly_buds <- rbind(yrly_buds, yrly_buds)
        yrly_buds$buds <- as.character(yrly_buds$buds)
        yrly_buds$buds[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- "Y"
        yrly_buds$buds <- as.factor(yrly_buds$buds)
        yrly_buds$Freq[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- 0
    }
    NA_TEST <- aggregate(yrly_buds["Freq"], by=yrly_buds["yrs"],sum)
    yrly_buds <- subset(yrly_buds, buds == "Y")
    if (NA_TEST$Freq == 0){
        yrly_buds$Freq[NA_TEST$Freq == 0] <- NA
    }
} else {
    yrly_buds[1:nrow(un_yrs),] <- NA
}

yrly_flowers <- as.data.frame(table(yrs, flowers))
if (nrow(yrly_flowers) > 0){
    if (length(levels(yrly_flowers$flowers)) < 2){
        yrly_flowers <- rbind(yrly_flowers, yrly_flowers)
        yrly_flowers$flowers <- as.character(yrly_flowers$flowers)
        yrly_flowers$flowers[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- "Y"
        yrly_flowers$flowers <- as.factor(yrly_flowers$flowers)
        yrly_flowers$Freq[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- 0
    }
    NA_TEST <- aggregate(yrly_flowers["Freq"], by=yrly_flowers["yrs"],sum)
    yrly_flowers <- subset(yrly_flowers, flowers == "Y")
    if (NA_TEST$Freq == 0){
        yrly_flowers$Freq[NA_TEST$Freq == 0] <- NA
    }
} else {
    yrly_flowers[1:nrow(un_yrs),] <- NA
}

yrly_seeds <- as.data.frame(table(yrs, seeds))
if (nrow(yrly_seeds) > 0){
    if (length(levels(yrly_seeds$seeds)) < 2){
        yrly_seeds <- rbind(yrly_seeds, yrly_seeds)
        yrly_seeds$seeds <- as.character(yrly_seeds$seeds)
        yrly_seeds$seeds[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- "Y"
        yrly_seeds$seeds <- as.factor(yrly_seeds$seeds)
        yrly_seeds$Freq[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- 0
    }
    NA_TEST <- aggregate(yrly_seeds["Freq"], by=yrly_seeds["yrs"],sum)
    yrly_seeds <- subset(yrly_seeds, seeds == "Y")
    if (NA_TEST$Freq == 0){
        yrly_seeds$Freq[NA_TEST$Freq == 0] <- NA
    }
} else {
    yrly_seeds[1:nrow(un_yrs),] <- NA
}

yrly_pods <- as.data.frame(table(yrs, pods))
if (nrow(yrly_pods) > 0){
    if (length(levels(yrly_pods$pods)) < 2){
        yrly_pods <- rbind(yrly_pods, yrly_pods)
        yrly_pods$pods <- as.character(yrly_pods$pods)
        yrly_pods$pods[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- "Y"
        yrly_pods$pods <- as.factor(yrly_pods$pods)
        yrly_pods$Freq[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- 0
    }

    NA_TEST <- aggregate(yrly_pods["Freq"], by=yrly_pods["yrs"],sum)
    yrly_pods <- subset(yrly_pods, pods == "Y")

    if (NA_TEST$Freq == 0){
        yrly_pods$Freq[NA_TEST$Freq == 0] <- NA
    }
} else {
    yrly_pods[1:nrow(un_yrs),] <- NA
}

yrly_resprouts <- as.data.frame(table(yrs, resprout))
if (nrow(yrly_resprouts) > 0){
    if (length(levels(yrly_resprouts$resprout)) < 2){
        yrly_resprouts <- rbind(yrly_resprouts, yrly_resprouts)
        yrly_resprouts$resprout <- as.character(yrly_resprouts$resprout)
        yrly_resprouts$resprout[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- "Y"
        yrly_resprouts$resprout <- as.factor(yrly_resprouts$resprout)
        yrly_resprouts$Freq[(nrow(un_yrs)+1):(nrow(un_yrs)*2)] <- 0
    }
    NA_TEST <- aggregate(yrly_resprouts["Freq"], by=yrly_resprouts["yrs"],sum)
    yrly_resprouts <- subset(yrly_resprouts, resprout == "Y")
    if (NA_TEST$Freq == 0){
        yrly_resprouts$Freq[NA_TEST$Freq == 0] <- NA
    }
} else {
    yrly_resprouts[1:nrow(un_yrs),] <- NA
}

##
# Specimen observations per survey month
surveys <- ggplot(data = monthly_count,
       aes(Month, Freq)) +
    stat_summary_bin(fun = sum, # adds up all observations for the month
                 geom = "bar", bins = length(months)) + # or "line"
    scale_x_date(
        labels = date_format("%Y"),
        breaks = "1 year")

print(surveys + scale_y_log10() + xlab('Year') +
          ggtitle(paste(species, 'Observations per Month')) +
          theme(plot.title = element_text(hjust=0.5)))


# Specimen heights per survey day
if (sum(is.na(ht)) < tot_obs){
    scatter <- ggplot(df, aes(x = dDate, y = ht))+
        geom_point(size = 2, color = 'darkgreen', shape = 3)
    print(scatter + xlab('Year') + ylab('Height [m]') +
              scale_x_date(
                  labels = date_format("%Y"), breaks = "1 year") +
              ggtitle(paste(species, 'Heights per Survey')) +
              theme(plot.title = element_text(hjust=0.5)))
}
## Density plots
if (sum(is.na(ht)) < tot_obs){
    yrly_dens <- ggplot(df, aes(x = ht, color = yrs_fac)) +
        geom_density(size = 1)+
        geom_rug()
    print(yrly_dens + xlab('Height [m]') +
              labs(color = 'Year') +
              ggtitle(paste(species, 'Density by Height per Year')) +
              theme(plot.title = element_text(hjust=0.5)))

    all_dens <- ggplot(df, aes(x=ht))+
        geom_density(size = 0.7, linetype='dashed', color = 'black')+
        geom_rug()
    print(all_dens + xlab('Height [m]') +
              ggtitle(paste(species, 'Density by Height')) +
              theme(plot.title = element_text(hjust=0.5)))
}
#Summary table

summary <- data.frame(un_yrs)
summary <- cbind(summary, yrly_buds$Freq, yrly_flowers$Freq, yrly_pods$Freq, yrly_seeds$Freq, yrly_resprouts$Freq)
names(summary) <- c('Year', 'Observations', 'Bud count', 'Flower count', 'Pod count', 'Seed count', 'Resprouts')
# box and whisker

if (sum(is.na(ht)) < tot_obs){
ht_buds <- subset(ht, buds == "Y")
ht_flowers <- subset(ht, flowers =="Y")
ht_pods <- subset(ht, pods =="Y")
ht_seeds <- subset(ht, seeds == "Y")
ht_rep <- list(ht_buds, ht_flowers, ht_pods, ht_seeds)
names(ht_rep) <- c('buds', 'flowers', 'pods', 'seeds')
rc <- names(ht_rep)

bpht <- boxplot(ht_rep, xlab = "Reproductive Structure",
              ylab = 'Height [m]',
              notch = FALSE,
              col = 'green',
              border = 'black',
              main = paste(species, ' Height vs Reproductive Structure', sep = ':'))
bpht
}
if (sum(is.na(bd1)) < tot_obs){
bd1_buds <- subset(bd1, buds == "Y")
bd1_flowers <- subset(bd1, flowers =="Y")
bd1_pods <- subset(bd1, pods =="Y")
bd1_seeds <- subset(bd1, seeds == "Y")
bd1_rep <- list(bd1_buds, bd1_flowers, bd1_pods, bd1_seeds)
names(bd1_rep) <- c('buds', 'flowers', 'pods', 'seeds')
rc <- names(bd1_rep)

bpbd1 <- boxplot(bd1_rep, xlab = "Reproductive Structure",
                ylab = 'Basal diameter [m]',
                notch = FALSE,
                col = 'brown',
                border = 'black',
                main = paste(species, ' Basal Diameter vs Reproductive Structure', sep = ':'))
bpbd1
}
return(summary)
}





