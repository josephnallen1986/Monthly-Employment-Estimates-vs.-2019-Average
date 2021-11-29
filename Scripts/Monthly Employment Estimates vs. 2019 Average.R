library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(tidyverse)    # Collection of R packages designed for data science
library(lubridate)    # Makes it easier to work with dates and times.

ces_2019_avg <- read.socrata(
  "https://data.edd.ca.gov/resource/pwx8-ztk5.json?$where=seasonally_adjusted_y_n = 'N' AND year = '2019'") %>%
  filter(area_type != "County",
         (series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
            series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
            series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
            series_code == "80000000" | series_code == "90000000" | series_code == "0")) %>%
  select(area_name, industry_title, average_2019_employment = current_employment) %>%
  mutate(average_2019_employment = as.numeric(average_2019_employment)) 

ces <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year >= '2019'") %>%
  filter(area_type != "County",
         (series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
            series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
            series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
            series_code == "80000000" | series_code == "90000000" | series_code == "0")) %>%
  select(area_name, industry_title, month = date, current_employment) %>%
  mutate(estimated_employment = as.numeric(current_employment),
         month = as.Date(month))

ces <- ces %>% 
  inner_join(ces_2019_avg, by = c("area_name" = "area_name", "industry_title" = "industry_title")) %>%
  mutate(index = round(
    ((estimated_employment / average_2019_employment) * 100) - 100, 0))

ca_ces <- ces %>%
  filter(area_name == "California") %>%
  mutate(ca_index = round(
    ((estimated_employment / average_2019_employment) * 100) - 100, 0))  %>%
  select(
    area_name, 
    industry_title,
    month,
    ca_estimated_employment = estimated_employment, 
    ca_average_2019_employment = average_2019_employment,
    ca_index)

msa_ces <- ces %>%
  filter(area_name != "California") %>%
  mutate(cty_index = round(
    ((estimated_employment / average_2019_employment) * 100) - 100, 0))  %>%
  select(
    msa = area_name, 
    industry_title,
    month,
    cty_estimated_employment = estimated_employment, 
    cty_average_2019_employment = average_2019_employment,
    cty_index) %>%
  inner_join(ca_ces, by = c("month" = "month", "industry_title" = "industry_title")) %>%
  select(!area_name)

d <- paste(getwd(),"/Output/",format(max(ces$month), "%y-%m")," ",month.abb[month(max(ces$month))],sep="")
dir.create(d, showWarnings = FALSE)

industry_list <- unique(msa_ces$industry_title)

for (industry in industry_list) {
  
  df <- msa_ces %>%  
    filter(str_detect(msa, "Anaheim|Riverside|San Diego|Los Angeles"),
           industry_title == industry)
  
  index_graph <- df %>%
    ggplot(aes(x=month, y=cty_index,)) +
    geom_hline(yintercept = 0, color="gray", size = 1) +
    geom_line(size = 1.25, alpha = 0.8, aes(y=cty_index,color="MSA")) +
    geom_line(size = 1.25, alpha = 0.8, aes(y=ca_index,color="California")) +
    scale_color_manual(values = c("#cf7f00","#00597c")) +
    labs(colour="Area",x="Month",y="Index") +
    labs(
      title = industry,
      subtitle = "Industry Employment Growth Comparison\nPercentage Change Relative to 2019 Average") +
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          plot.title = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 18,
            face="bold"),
          plot.subtitle = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 16,
            face="bold"),
          axis.title=element_text(size=16,face="bold")) +
    facet_wrap(vars(msa), ncol=2) +
    theme(
      strip.text.x = element_text(
        size = 14, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(
      breaks = seq(min(df$cty_index), max(df$cty_index), by = 5),
      limits = c(min(df$cty_index)-5, max(df$cty_index))) +      
    scale_x_date(
      date_breaks  ="3 month",
      date_labels="%y %b")

  file_name <- paste(d,"/",industry,".png",sep="")  

  ggsave(index_graph, filename = file_name, dpi = 300, type = 'cairo',
         width = 11, height = 8.5, units = 'in')
  
  print(file_name)
}



