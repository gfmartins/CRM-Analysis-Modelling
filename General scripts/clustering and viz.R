library(tidyverse)
library(caret)
library(readxl)
library(fpc)
library(factoextra)
library(ggthemes)
library(gridExtra)
library(plotly)

######### Data Clustering ##########


# 1) Importar dataset
dataset_pre_clustering <- dataset_donations %>%
  select(
    donation.date,
    donation.year,
    donor.no,
    donation.amount
  ) %>%
  group_by(donation.year, donor.no) %>%
  mutate(
    number.donation.year = n(),
    value.donations.year = mean(donation.amount)
  ) %>%
  ungroup() %>%
  group_by(donor.no) %>%
  summarize(
    number.donations = mean(number.donation.year, na.rm = TRUE),
    value.donations = mean(value.donations.year, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  filter(value.donations < 20000, number.donations < 30)

dataset_clustering<- dataset_pre_clustering %>%
  select(number.donations, value.donations) 


# Scale the data if neccesary (by preProcess = )
# library(caret)
## Create an object with the pre processing parameters
ObjectPreprocessParams <- preProcess(dataset_clustering, method=c("scale"))
# summarize transform parameters
print(ObjectPreprocessParams)
## Transform the dataset using the parameters
dataset_clustering <- predict(ObjectPreprocessParams, dataset_clustering)
# Summarize the transformed dataset
summary(dataset_clustering)

## Select number of clusters
NumberClusters <- 3

### Applying k-means to the dataset
## Dataset, número de clusters que se ve mediante la observación del gráfico (donde se suaviza la pendiente), número de iteraciones (valor fijo 300), nstart = valor fijo en 10, iter.max = valor máximo que R buscará (es bueno ponerlo para ver cual es el mejor resultado pero no es necesario)
set.seed(29)
ObjectKM<- kmeans(dataset_clustering, NumberClusters, iter.max = 300, nstart = 10)
VectorClusters<- ObjectKM$cluster
## Create a new column with the clients and the cluster in which it was assigned
dataset_clustered_km <- mutate(dataset_clustering, cluster.assigned = VectorClusters)

# Visualize the clusters

## Option2
plotAllClusters <-
  ggplot(dataset_clustered_km,
         aes(
           x = number.donations,
           y = value.donations,
           color = factor(cluster.assigned)
         )) +
  geom_point(alpha = 0.5) +
  theme_economist() +
  scale_color_economist() +
  labs(x = "Number of Donations",
       y = "Average Value of Donations") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12),
    text = element_text(family = "Tahoma")
  ) +
  guides(color = guide_legend(title = "Clusters"))

# Add clusters to dataset_donations 
## Include clusters in dataset_pre_clustering
dataset_post_clustering <-  dataset_pre_clustering %>% 
  mutate(cluster.assigned = dataset_clustered_km$cluster.assigned)

## Include clusters to main dataset
dataset_donations <- dataset_donations %>% 
  left_join(dataset_post_clustering, by = "donor.no") %>% 
  replace_na(list(cluster.assigned = "Outlier")) 




######### Data Visualisation ##########

# Clusters visualisation
## % of number and amount of donations per cluster
dataset_donations %>% 
  mutate(
    total.amount.donations = sum(donation.amount),
    total.count.donations = n()
  ) %>%  
  group_by(cluster.assigned) %>% 
  mutate(
    perc.count.donations.total = (n() / total.count.donations)* 100,
    perc.sum.amount.donations.total = (sum(donation.amount) / total.amount.donations)*100
  )%>% 
  ungroup() %>% 
  distinct(cluster.assigned, .keep_all = TRUE) %>% 
  select(cluster.assigned, "% Number Donations" = perc.count.donations.total, 
         "% Amount of donations (GBP)" = perc.sum.amount.donations.total, 
         -c(total.amount.donations, total.count.donations)) %>%  
  mutate_at(vars(cluster.assigned), funs(as.factor)) %>% 
  gather(index, value, - cluster.assigned) %>% 
  ggplot(aes(cluster.assigned, value, fill = cluster.assigned)) +
  geom_col(alpha = 0.8) +
  facet_grid(. ~ index) +
  labs(title = "Number and Amount of Donations by Cluster", 
       x = "", 
       y = "Value (%)") +
  theme_economist() + 
  scale_fill_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        panel.background = element_rect(fill = "#cee0e6")) + 
  guides(fill=guide_legend(title= "Cluster Assigned"))




## % of donors per cluster
dataset_donations %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  mutate(
    total.count.donors = n()
  ) %>%  
  group_by(cluster.assigned) %>% 
  mutate(
    perc.count.donors.total = (n() / total.count.donors)* 100
  )%>% 
  ungroup() %>% 
  distinct(cluster.assigned, .keep_all = TRUE) %>% 
  select(cluster.assigned, perc.count.donors.total) %>% 
  mutate_at(vars(cluster.assigned), funs(as.factor)) %>% 
  gather(index, value, - cluster.assigned) %>% 
  filter(index == "perc.count.donors.total") %>% 
  ggplot(aes(cluster.assigned, value, fill = cluster.assigned)) +
  geom_col(alpha = 0.8) +
  # facet_grid(. ~ cluster.assigned) +
  labs(title = "Number of Donors by Cluster", subtitle = "Cluster 2 contains the majority of values", 
       x = "", 
       y = "Value (%)") +
  theme_economist() + 
  scale_fill_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=20), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma")) + 
  guides(fill=guide_legend(title= "Cluster Assigned"))


### Group by donor.category

dataset_viz_clustering <- dataset_donations %>% 
  mutate(
    total.amount.donations = sum(donation.amount),
    total.count.donations = n()
    ) %>% 
  group_by(donor.category) %>% 
  mutate(
    total.count.donations.group = n(),
    total.amount.donations.group = sum(donation.amount)
  ) %>% 
  ungroup() %>% 
  group_by(donor.category, cluster.assigned) %>% 
  mutate(
    count.donations.clusters = n(),
    perc.count.donations.group = (count.donations.clusters / total.count.donations.group) * 100,
    perc.count.donations.total = (count.donations.clusters / total.count.donations) * 100,
    sum.amount.donations.clusters = sum(donation.amount),
    perc.sum.amount.donations.group = (sum.amount.donations.clusters / total.amount.donations.group) * 100,
    perc.sum.amount.donations.total = (sum.amount.donations.clusters / total.amount.donations) * 100
  ) %>% 
  distinct(donor.category, .keep_all = TRUE) %T>% print() %>% 
  ungroup() %>% 
  select(
    donor.category, 
    cluster.assigned, 
    count.donations.clusters, 
    perc.count.donations.group,
    perc.count.donations.total,
    sum.amount.donations.clusters, 
    perc.sum.amount.donations.group,
    perc.sum.amount.donations.total
  ) %>% 
  gather(index, value, -donor.category, -cluster.assigned) 


dataset_viz_clustering_unique_donors <- dataset_donations %>% 
  distinct(donor.no, .keep_all = TRUE) %>% 
  mutate(
    total.count.donors = n()
  ) %>% 
  group_by(donor.category) %>% 
  mutate(
    total.count.donors.group = n()
  ) %>% 
  ungroup() %>% 
  group_by(donor.category, cluster.assigned) %>% 
  mutate(
    count.donors.clusters = n(),
    perc.count.donors.group = (count.donors.clusters / total.count.donors.group) * 100,
    perc.count.donors.total = (count.donors.clusters / total.count.donors) * 100
  ) %T>% print() %>%
  distinct(donor.category, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(
    donor.category, 
    cluster.assigned, 
    count.donors.clusters, 
    perc.count.donors.group,
    perc.count.donors.total
  ) %>% 
  gather(index, value, -donor.category, -cluster.assigned) 


# Number donations
## Percentage of donations  
plotPercDonationsClus1 <- dataset_viz_clustering %>% 
  filter(
    index == "perc.count.donations.group"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of of each category's number of donations are on every cluster?", 
    x = "", 
    y = "% Number of donations of category"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=13), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

grid.arrange(plotPercDonationsClus1, plotAllClusters, nrow=1, ncol=2)


## Percentage of total donations  
plotPercTotalDonationsClus1 <- dataset_viz_clustering %>% 
  filter(
    index == "perc.count.donations.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of global number of donations are on every cluster and category?", 
    x = "", 
    y = "% Number Donations"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=13), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

grid.arrange(plotPercTotalDonationsClus1, plotAllClusters, nrow=1, ncol=2)


# Number donors
## Percentage of donors  
plotPercDonorsClus1 <- dataset_viz_clustering_unique_donors %>% 
  filter(
    index == "perc.count.donors.group"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of of each category's number of donors are on every cluster?", 
    x = "", 
    y = "% Donors of category"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=13), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  

grid.arrange(plotPercDonorsClus1, plotAllClusters, nrow=1, ncol=2)

## Percentage of total donors  
plotPercTotalDonorsClus1 <- dataset_viz_clustering_unique_donors %>% 
  filter(
    index == "perc.count.donors.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of global number of donors are on every cluster and category?", 
    x = "", 
    y = "% Donors"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=13), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

grid.arrange(plotPercTotalDonorsClus1, plotAllClusters, nrow=1, ncol=2)


# Amount donations
## Percentage of group amount donations  
plotPercDonClus1 <- dataset_viz_clustering %>% 
  filter(
    index == "perc.sum.amount.donations.group"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of of each category's donations are on every cluster?", 
    x = "", 
    y = "% Donations of category"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))

grid.arrange(plotPercDonClus1, plotAllClusters, nrow=1, ncol=2)


## Percentage of total amount donations  
plotPercTotalDonClus1 <- dataset_viz_clustering %>% 
  filter(
    index == "perc.sum.amount.donations.total"
  ) %>% 
  ggplot(aes(donor.category, value)) +
  geom_col() +
  facet_grid(. ~ cluster.assigned) +
  labs(
    title = "What % of global donations are on every cluster and category?", 
    x = "", 
    y = "% Donations"
  ) +
  theme_economist() + 
  scale_color_economist() + 
  theme(legend.position = "right", 
        plot.title = element_text(size=15), 
        plot.subtitle = element_text(size = 12), 
        text = element_text(family = "Tahoma"),
        axis.text.x=element_text(angle = 60)) + 
  guides(color=guide_legend(title= "Titulo leyenda"))


grid.arrange(plotPercTotalDonClus1, plotAllClusters, nrow=1, ncol=2)


# Challenge1, increase donations of one time donors
# Challenge2, increase value donates of high number donations donors
# Challenge3, create a legacy strategy (40% of income)
# Conclusion: events is an income generator, but it accounts for
#   less than 10% of income, but in terms of donors (spread of message)
#   its value is almost 40% (it's a charitable activity)
