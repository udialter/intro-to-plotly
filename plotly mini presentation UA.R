#' ---
#' title: "Plotly demonstration/mini presentation"
#' author: 'Udi Alter'
#' date: March 14 2022
#' output: html_document
#' ---


#+ echo=FALSE
library(tidyverse)
library(readxl)
library(viridis)
library(hrbrthemes)
library(htmlwidgets)
library(RColorBrewer)
library(readr)
library(extrafont)
library(plotly)

#install.packages("extrafontdb")  # reset fonttable

#devtools::install_github("hrbrmstr/hrbrthemes", force = TRUE)
extrafont::loadfonts()
hrbrthemes::import_roboto_condensed()


#' ## Import data from Github
df <- read.csv("https://raw.githubusercontent.com/udialter/intro-to-plotly/main/promotions.csv")
#' Data adapted from https://open.canada.ca/data/en/dataset/89b12d0b-e844-4470-8123-bd062d27be0b
head(df)
#' ## renaming variables for convenience 
df <- df %>%
  rename(Year = 'Fiscal.Year', Female_Promoted = 'Female.NCMs.Promoted', Male_Promoted = 'Male.NCMs.Promoted', Total_promotions = 'Total.NCM.Promotions')
str(df)
#' ## data organizing and restructuring
year <- df$Year
sexf <- rep('Females', nrow(df))
sexm <- rep('Males', nrow(df))
sext <- rep('Total', nrow(df))
fp <- data.frame(year,df$Female_Promoted, sexf) %>% rename(promoted = df.Female_Promoted, sex= sexf)
mp <- data.frame(year,df$Male_Promoted,sexm )%>% rename(promoted = df.Male_Promoted, sex= sexm)
tp <- data.frame(year,df$Total_promotions,sext )%>% rename(promoted = df.Total_promotions,sex =sext)

#' ## New data now called df1, year and sex are set as factors
df1 <- rbind(fp,mp,tp)
df1$year <- factor(df1$year)
df1$sex <- factor(df1$sex)

#' ## first, creating a ggplot2 object. If issues arise with the font roboto or and error message mentioning polygon (usually in macs), 
#' you can # or delete the  hrbrthemes::theme_ft_rc()+ line below (line 45)
p<-ggplot(df1,aes(x=year, y=promoted, group=sex)) +
  geom_line(aes(color=sex), size=1.5)+
  geom_point(aes(color=sex),size = 3)+
  #hrbrthemes::theme_ft_rc()+
  theme(axis.text.x = element_text(angle=45,vjust = 1, hjust=1))

p <- p + scale_color_brewer(palette="Set2")+
  ggtitle("Canadian Armed Forces Officer Promotions by Sex")+
  labs(x = "Year", y="Number of personnel promoted",size=3)

p
ggsave("CAF.png", plot = p, dpi = 700)

(intplt<- ggplotly(p, tooltip = c('x',"y","group")))

saveWidget(intplt, "ggplotlyeg.html", selfcontained = F, libdir = "lib/")


dens <- with(diamonds, tapply(price, INDEX = cut, density))
data <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x)))

fig <- plot_ly(data, x = ~x, y = ~y, z = ~cut, type = 'scatter3d', mode = "lines", color = ~cut, fillcolor= ~cut)

fig

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

diamonds

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))
fig
dd <- sample_n(diamonds, size = 100)

#colors = c('#BF382A', '#0C4B8E', '#6699ff', '#5a3634', '#330055')
figd <- plot_ly(dd, x = ~carat, y = ~price, z = ~depth, color = ~cut)
figd <- figd %>% add_markers()
figd <- figd %>% layout(scene = list(xaxis = list(title = 'Carat'),
                                   yaxis = list(title = 'Price'),
                                   zaxis = list(title = 'Depth')))

figd

count <- 3000

x <- c()
y <- c()
z <- c()
c <- c()

for (i in 1:count) {
  r <- i * (count - i)
  x <- c(x, r * cos(i / 30))
  y <- c(y, r * sin(i / 30))
  z <- c(z, i)
  c <- c(c, i)
}

data <- data.frame(x, y, z, c)

fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
               line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040'))))

fig
