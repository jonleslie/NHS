library(tidyverse)

df_non_medical <- read_csv("data/NHS Workforce Statistics - August 2017, staff excluding medical.csv")
df_non_medical

names(df_non_medical) <- str_replace_all(names(df_non_medical), " ", "_")

df_non_medical %>% 
  select(Staff_Group_1) %>% 
  group_by(Staff_Group_1) %>% 
  count() 

df_non_medical %>% 
  select(Main_Staff_Group:Staff_Group_2) %>% 
  group_by(Main_Staff_Group) %>% 
  count()

# Look at nurses and health visitors --------------------------------------


with(df_non_medical, table(Main_Staff_Group, Staff_Group_1))
table(df_non_medical$Staff_Group_1)
nurses_hv <- df_non_medical %>% 
  filter(Staff_Group_1 == "005_Nurses & health visitors")

table(nurses_hv$Main_Staff_Group)
table(nurses_hv$Staff_Group_2)
View(nurses_hv)

nurses_hv %>% 
  group_by(HEE_Region_Name) %>% 
  ggplot(aes(HEE_Region_Name)) +
  geom_bar() +
  coord_flip() +
  ggtitle("Number of NHS nurses and \nHealth Visitors by health education region")

nurses_hv %>% 
  group_by(HEE_Region_Name) %>% 
  ggplot(aes(HEE_Region_Name, fill = Staff_Group_2)) +
  geom_bar() +
  coord_flip() +
  ggtitle("Number of NHS nurses and \nHealth Visitors by health education region")

nurses_hv %>% 
  group_by(HEE_Region_Name, Staff_Group_2) %>% 
  count()
  ggplot(aes(HEE_Region_Name)) +
  geom_bar() +
  coord_flip() +
  ggtitle("Number of NHS nurses and \nHealth Visitors by health education region")

test <- nurses_hv %>% 
  select(HEE_Region_Name, Staff_Group_2) %>% 
  group_by(HEE_Region_Name) %>% 
  mutate(count = n()) %>% 
  group_by(Staff_Group_2, add = TRUE) %>% 
  mutate(per = n()/count)
test <- distinct(test)
test %>% 
  group_by(HEE_Region_Name) %>% 
  ggplot(aes(HEE_Region_Name, per, fill = Staff_Group_2)) +
  geom_col() +
  coord_flip()

%>% 
  filter(distinct(.))

%>% 
  ggplot(aes(Staff_Group_2, per)) +
  geom_col() +
  coord_flip()

nurses_hv %>% 
  select(HEE_Region_Name, Staff_Group_2) %>% 
  group_by(HEE_Region_Name) %>% 
  mutate(count = n()) %>% 
  group_by(Staff_Group_2, add = TRUE) %>% 
  mutate(per = n()/count) %>% 
  filter(HEE_Region_Name == "Health Education East Midlands") %>% 
  ggplot(aes(Staff_Group_2, per)) +
  geom_col() +
  coord_flip()

nurses_hv %>% 
  group_by(HEE_Region_Name) %>% 
  mutate(count = n()) %>%
  group_by(Staff_Group_2) %>% 
  mutate(per = )
  
df %>% group_by(month) %>% 
    mutate(per=paste0(round(count/sum(count)*100, 2), "%")) %>% 
    ungroup

nurses_hv %>%
  group_by(HEE_Region_Name) %>%
  mutate(countT= sum(count)) %>%
  group_by(type, add=TRUE) %>%
  mutate(per=paste0(round(100*count/countT,2),'%'))