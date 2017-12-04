library(drought2015)
library(lubridate)

inf <- readRDS("../papers/hpt/europe/infile_europe.rds")
res <- readRDS("../papers/hpt/europe/result_europe.rds")
country <- extract_metadata(res, cols = "country")

droughts <- lapply(inf, find_droughts, threshold = "Q95")
pooled <- lapply(droughts, pool_sp)
#dd <- lapply(pooled, summary, drop_minor = F)

dd <- list()
for(i in seq_along(pooled)) {
  if(as.vector(pooled[[i]][1, "threshold"]) == 0) next
  dd[[i]] <- summary(pooled[[i]], drop_minor = FALSE)
}

mask <- which(!sapply(dd, is.null))
dd <- dd[mask]


jseq <- function(start, end)
{
  start <- as.numeric(format(start, "%j"))
  end <- as.numeric(format(end, "%j"))

  if(end >= start) y <- seq(start, end) else {
    y <- c(seq(1, end), seq(start, 365))
  }
  return(y)
}

covers <- function(x)
{
  n <- nrow(x)
  nyear <- year(x$end[n]) - year(x$start[1])
  dry <- list()
  for(i in seq_len(n)) {
    dry[[i]] <- jseq(x$start[i], x$end[i])
  }

  tbl <- table(unlist(dry)) / nyear

  return(tbl)
}

#fraction of yeas with day under drought ##
dry <- lapply(dd, covers)
# matrix of dry

dm <- matrix(0, ncol = length(dry), nrow = 366)
for(i in seq_along(dry)) {
  dm[as.numeric(names(dry[[i]])), i] <- round(dry[[i]], 3)
}

dryday <- reshape::melt(dm, varnames = c("day", "station"))
dryday$station <- mask[dryday$station]
dryday$country <- country[dryday$station, "country"]
nstation <- table(country[mask, ])
nstation[] <- paste0(names(nstation), " (n=", nstation, ")")
dryday$country <- nstation[dryday$country]

library(tidyr)
library(dplyr)
days <- group_by(dryday, country, day) %>% summarize(fraction = mean(value))

saveRDS(days, file = "../smires/dev/day_drought95.rds")
days <- readRDS("dev/day_drought.rds")

days <- readRDS("dev/day_drought95.rds")
days$time <- as.Date(strptime(days$day, "%j"))
days <- na.omit(days)


library(tidyr)
col <- group_by(days, country) %>% summarize(min = (time[which.min(fraction)]),
                                      max = (time[which.max(fraction)]))


col <- gather(col, key = char, value = time,  -country)
# col <- rbind(col,
#              data.frame(country = "no", char = "min", time = as.Date("2017-10-12")),
#              data.frame(country = "no", char = "max", time = as.Date("2017-08-17")),
#              data.frame(country = "ch", char = "min", time = as.Date("2017-12-25")),
#              data.frame(country = "ch", char = "max", time = as.Date("2017-02-13")))

rank <- col
x <- subset(rank, char == "max")
x <- x[order(x$time), ]
x <- x[!duplicated(x$country), ]
lvls <- x$country

rank$country <- factor(rank$country, levels = lvls)
library(ggplot2)
ggplot(rank, aes(time, country, col = char)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "") +
  theme_bw()

tmp <- merge(days, col, all.x = TRUE)
shift_time <- function(x) {
  pre <- seq_len(which(x$fraction == min(x$fraction))[1] -1)
  x$time[pre] <- x$time[pre] + years(1)

  if(x$country %in% c("at", "ch", "no", "nl")) x$time <- x$time - years(1)
  x
}
#tmp <- group_by(tmp, country) %>% do(shift_time(.))
tmp$country <- factor(tmp$country, levels = lvls)


dp <- 0.01
smooth_vals <- tmp %>% group_by(country) %>%
  do(smoothed = predict(loess(fraction~day, ., span = 0.3), .$day)) %>%
  ungroup() %>% unnest() %>% group_by(country) %>%
  mutate(lwr = min(smoothed) + dp,
         upr = max(smoothed) - dp,
         char = ifelse(smoothed < lwr, "min", ifelse(smoothed > upr, "max", "default")))

smooth_vals$time <- sort(unique(tmp$time))



library(ggplot2)
library(scales)
#tmp1 <- subset(tmp, country == "no")
ggplot(tmp, aes(time, fraction)) +
  geom_col(aes(col = char, fill = char)) +
  geom_point(data = col, mapping = aes(col = char), y = 0) +
  geom_line(data = smooth_vals, aes(time, smoothed, col = char, group = country),
            size = 0.5) +
  scale_color_manual(na.value = "lightgrey",
                       values = c("max" = "#F8766D", "min" = "#00BFC4", "default" = 1)) +
  scale_fill_manual(na.value = "lightgrey",
                     values = c("max" = "#F8766D", "min" = "#00BFC4", "default" = 1)) +
  scale_x_date(date_minor_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2)) +
  #geom_smooth(se = F, span = 0.3, col = 1, lwd = 0.3, method = "loess") +

  labs(y = "Percentage of stations with discharge < Q95",
       title = "When do droughts occur?", x = "",
       subtitle = "loess smoother, points indicate day of unsmoothed extreme")+
  facet_wrap(~country, ncol = 2, strip.position = "right") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("inst/demos/2017-06_lyon/fig/hyear_start_95.pdf",
       width = 307/72*1.2, height = 258/72, unit = "in", scale = 2)

sv <- smooth_vals
sv$char[sv$char == "default"] <- NA

ggplot(sv,
       aes(time, country, col = char, group = country)) +
  geom_line(size = 0.5) +
  scale_color_manual("Extreme", values = c("max" = "#F8766D", "min" = "#00BFC4")) +
  scale_x_date(date_minor_breaks = "1 month", date_labels = "%b") +
  labs(title = "Smoothed period covering min/max +/- 1%") +
  theme_bw() +
  theme(axis.title = element_blank())
ggsave("inst/demos/2017-06_lyon/fig/hyear_start_95_region.pdf",
       width = 307/72, height = 258/72/2, unit = "in", scale = 2)


ggplot(tmp, aes(time, fraction, group = country)) +
  #geom_col(aes(col = char, fill = char)) +
  scale_color_discrete(na.value = "lightgrey") +
  scale_x_date(date_minor_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent) +
  geom_smooth(se = F, span = 0.3, col = 1, lwd = 0.3, method = "loess") +
  labs(y = "Percentage of stations under drought",
       title = "When do droughts occur?", x = "")+
  #facet_wrap(~country, ncol = 2, strip.position = "right") +
  theme_bw() +
  theme(legend.position = "none")
