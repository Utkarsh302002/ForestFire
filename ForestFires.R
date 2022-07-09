f <- read_csv("C:/Users/legion/Downloads/forestfires.csv")
f

dim(f)
head(f)
tail(f)
ncol(f)
nrow(f)
names(f)
class(f)
str(f)
summary(f)
colnames(f)
is.na(f)
sd(f$FFMC)
sd(f$DMC)
sd(f$DC)
sd(f$ISI)
sd(f$temp)
sd(f$RH)
sd(f$area)
sd(f$rain)
sd(f$wind)
quantile(f$DMC)
quantile(f$FFMC)
quantile(f$DC)
quantile(f$ISI)
quantile(f$temp)
quantile(f$RH)
quantile(f$area)
quantile(f$rain)
quantile(f$wind)

aggregate(.~DMC, f, mean)
aggregate(.~FFMC, f, mean)
aggregate(.~DC,f, mean)
aggregate(.~ISI, f, mean)
aggregate(.~temp, f, mean)
aggregate(.~RH,f, mean)
aggregate(.~area, f, mean)
aggregate(.~rain,f, mean)
aggregate(.~wind, f, mean)


aggregate(.~DMC, f, sd)
aggregate(.~FFMC, f, sd)
aggregate(.~DC,f, sd)
aggregate(.~ISI, f, sd)
aggregate(.~temp, f, sd)
aggregate(.~RH,f, sd)
aggregate(.~area, f, sd)
aggregate(.~rain,f, sd)
aggregate(.~wind, f, sd)

#######Number of forest fires by month
fireinmonths <- f %>%
  group_by(month) %>%
  summarize(count = n())

ggplot(fireinmonths) +
  aes(x = month, y = count) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of fires occurred in each month", x = "Month", y = "Number fires") + 
  theme(panel.background = element_rect(fill = "white"))

##########Number of forest fires by a week-day
fireindays <- f %>%
  group_by(day) %>%
  summarize(count_weekday = n())

ggplot(fireindays) +
  aes(x = day, y = count_weekday) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of fires occurred by each day", x = "Day of the week", y = "Number of fires") + 
  theme(panel.background = element_rect(fill = "white"))


#####Re-ordering months for easier readability

f <- f %>%
  mutate(month = factor(month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) %>%
  mutate(day = factor(day, c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

###After Re-ordering Number of forest fires by month
fireinmonths <- f %>%
  group_by(month) %>%
  summarize(count = n())

ggplot(fireinmonths) +
  aes(x = month, y = count) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of fires occurred in each month", x = "Month", y = "Number fires") + 
  theme(panel.background = element_rect(fill = "white"))

##########After Re-ordering Number of forest fires by a week-day
fireindays <- f %>%
  group_by(day) %>%
  summarize(count_weekday = n())

ggplot(fireindays) +
  aes(x = day, y = count_weekday) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of fires occurred by each day", x = "Day of the week", y = "Number of s fires") + 
  theme(panel.background = element_rect(fill = "white"))

########Box plots of independent var. by month
create_box_by_month <- function(x, y) {
  ggplot(f) +
    aes_string(x, y) + 
    geom_boxplot()
}

x_var <- c("month")
y_var <- c("FFMC", "DMC","DC", "ISI", "temp", "RH", "wind", "rain")

map2(x_var, y_var, create_box_by_month)


####Box Plots of independent var. by week day

create_box_by_day <- function(x, y) {
  ggplot(f) +
    aes_string(x, y) + 
    geom_boxplot()
}

X_var <- c("day")
Y_var <- c("FFMC", "DMC","DC", "ISI", "temp", "RH", "wind", "rain")

map2(X_var, Y_var, create_box_by_day)


#########
create_scatter <- function(x,y){
  ggplot(data = f) +
    aes_string(x,y) +
    geom_point()
}

Y_scat <- c("area")
X_scat <- c("FFMC", "DMC","DC", "ISI", "temp", "RH", "wind", "rain")

map2(X_scat, Y_scat, create_scatter)

### plot a histogram quickly:
ggplot(f)+
  aes(area) +
  geom_histogram(bins = 20)

area_is_zero <- f %>%
  filter(area == 0)
area_without_outliers <- f %>%
  filter(area > 0 & area < 400)
area_0_and_50 <- f %>%
  filter(area > 0 & area <= 50)

########Plotting area burnt between 0 ha and 50 ha

create_scatter <- function(x,y){
  ggplot(data = area_0_and_50) +
    aes_string(x,y) +
    geom_point()
}

Y_scat <- c("area")
X_scat <- c("FFMC", "DMC","DC", "ISI", "temp", "RH", "wind", "rain")

map2(X_scat, Y_scat, create_scatter)

