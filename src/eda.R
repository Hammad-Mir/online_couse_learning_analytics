library('ProjectTemplate')
load.project()

'
for (dataset in project.info$cache)
{
  message(paste(\'Showing top 5 rows of\', dataset))
  print(head(get(dataset)))
}
'
# Playing around with the data

typeof(project.info$cache)

project.info$data[1]

typeof(get(project.info$cache[1]))

head(get(project.info$cache[1]))

print(head(get(project.info$cache[1])))

head(get(project.info$cache[2]))

summary(cyber.security.1_enrolments)

table(cyber.security.1_enrolments$gender)
table(cyber.security.1_enrolments$age_range)
table(cyber.security.1_enrolments$highest_education_level)
table(cyber.security.1_enrolments$employment_status)
table(cyber.security.1_enrolments$employment_area)
table(cyber.security.1_enrolments$detected_country)

a = filter(cyber.security.1_enrolments, !(gender == "Unknown" | age_range == "Unknown" |
                                                   highest_education_level == "Unknown" |
                                                   employment_status == "Unknown"))
dim(a)

b = filter(cyber.security.1_enrolments, !(gender == "Unknown" & age_range == "Unknown" &
                                                   highest_education_level == "Unknown" &
                                                   employment_status == "Unknown"))
dim(b)

table(b$gender)
table(b$age_range)
table(b$highest_education_level)
table(b$employment_status)
table(b$detected_country)

table(a$gender)
table(a$age_range)
table(a$highest_education_level)
table(a$employment_status)
table(a$detected_country)

c = as_tibble(data.frame(table(a$country)))

names(c)[1] = "region"
names(c)[2] = "users"

# creating map showing users per country for sub sample of data set
# the sub sample was selected where value for variables 
c = filter(c, country != "Unknown")

world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region)

world_map <- left_join(world_map, c, by="region")
#mapdata <- world_map %>% filter(!is.na(world_map$users))
world_map <- world_map %>%
  mutate(users = coalesce(users, 0))

map1 <- ggplot(world_map, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = users), color = "black") +
  scale_fill_gradient(low = "yellow", high =  "red", na.value = "grey50")
map1

# loop to retrieve only enrollments data over all the runs
iter <- c()
users <- c()
for(i in project.info$cache){
  if(grepl("enrolments", i, fixed = TRUE)){
    #print(i)
    iter <- append(iter, paste("run", gsub(".*?([0-9]+).*", "\\1", i), sep = "_"))
    users <- append(users, nrow(get(i)))
  }
}

users_per_iter <- data.frame(iter, users)

# plot to see differences in enrollments over runs
ggplot(data=users_per_iter, aes(x=iter, y=users, group=1)) +
  geom_line(color="red", size = 1.3)+
  geom_point(size = 2)

# run 1 enrollment subset for demographic analysis and plot
run1_enrollment_subset <- filter(cyber.security.1_enrolments, !(gender == "Unknown" | age_range == "Unknown" |
                                                                  highest_education_level == "Unknown" |
                                                                  employment_status == "Unknown"))

# extracting number of users for each category in all of the variables/features
run1_gender <- data.frame(table(run1_enrollment_subset$gender))
colnames(run1_gender) <- c("gender", "users")

run1_age <- data.frame(table(run1_enrollment_subset$age_range))
colnames(run1_age) <- c("age_range", "users")

run1_edu <- data.frame(table(run1_enrollment_subset$highest_education_level))
colnames(run1_edu) <- c("edu_level", "users")

run1_emp_status <- data.frame(table(run1_enrollment_subset$employment_status))
colnames(run1_emp_status) <- c("emp_status", "users")

run1_emp_area <- data.frame(table(run1_enrollment_subset$employment_area))
colnames(run1_emp_area) <- c("emp_area", "users")
run1_emp_area <- filter(run1_emp_area, !(emp_area == "Unknown"))

# creating plots for each of the features
gender_plot <- ggplot(data=run1_gender, aes(x=gender, y=users)) +
  geom_bar(stat="identity")
gender_plot                 #plot check

age_plot <- ggplot(data=run1_age, aes(x=age_range, y=users)) +
  geom_bar(stat="identity")
age_plot                 #plot check

edu_plot <- ggplot(data=run1_edu, aes(x=edu_level, y=users)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
edu_plot                 #plot check

emp_sta_plot <- ggplot(data=run1_emp_status, aes(x=emp_status, y=users)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
emp_sta_plot                 #plot check

emp_area_plot <- ggplot(data=run1_emp_area, aes(x=emp_area, y=users)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
emp_area_plot                 #plot check
'
lay <- rbind(c(1, 2),
             c(1, 2),
             c(3, 4),
             c(3, 4),
             c(5, 5),
             c(5, 5),
             c(5, 5),
             c(5, 5))
'
#creating layout matrix for final plot
lay <- rbind(c(1, 2),
             c(3, 4),
             c(5, 5),
             c(5, 5))

# Generating a combined plot
grid.arrange(gender_plot, age_plot, edu_plot, emp_sta_plot, emp_area_plot, layout_matrix = lay)

# generating data frames for comparison over 7 runs of the course

all_run_gender <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_run_gender) <- c("gender", "users", "run")

all_run_age <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_run_age) <- c("age_range", "users", "run")

all_run_edu <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_run_edu) <- c("edu_level", "users", "run")

all_run_emp_status <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_run_emp_status) <- c("emp_status", "users", "run")

all_run_emp_area <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_run_emp_area) <- c("emp_area", "users", "run")


for(i in project.info$cache){
  if(grepl("enrolments", i, fixed = TRUE)){
    
    iter = gsub(".*?([0-9]+).*", "\\1", i)
    
    subset_data = filter(get(i), !(gender == "Unknown" | age_range == "Unknown" |
                                                          highest_education_level == "Unknown" |
                                                          employment_status == "Unknown"))
    
    gender <- data.frame(table(subset_data$gender))
    colnames(gender) <- c("gender", "users")
    #gender$gender <- as.logical(gender$gender)
    gender$run <- rep(iter, times = nrow(gender))
    #gender$run <- as.logical(gender$run)
    all_run_gender <- rbind(all_run_gender, gender)
    
    age <- data.frame(table(subset_data$age_range))
    colnames(age) <- c("age_range", "users")
    age$run <- rep(iter, times = nrow(age))
    all_run_age <- rbind(all_run_age, age)
    
    edu <- data.frame(table(subset_data$highest_education_level))
    colnames(edu) <- c("edu_level", "users")
    edu$run <- rep(iter, times = nrow(edu))
    all_run_edu <- rbind(all_run_edu, edu)
    
    emp_status <- data.frame(table(subset_data$employment_status))
    colnames(emp_status) <- c("emp_status", "users")
    emp_status$run <- rep(iter, times = nrow(emp_status))
    all_run_emp_status <- rbind(all_run_emp_status, emp_status)
    
    emp_area <- data.frame(table(subset_data$employment_area))
    colnames(emp_area) <- c("emp_area", "users")
    #emp_area <- filter(emp_area, !(emp_area == "Unknown"))
    emp_area$run <- rep(iter, times = nrow(emp_area))
    all_run_emp_area <- rbind(all_run_emp_area, emp_area)
  }
}

# creating comparison plots for each of the features over 7 runs
all_gender_plot <- ggplot(data=all_run_gender, aes(x=gender, y=users, fill = run)) +
  geom_bar(stat="identity", position=position_dodge())
all_gender_plot                 #plot check

all_age_plot <- ggplot(data=all_run_age, aes(x=age_range, y=users, fill = run)) +
  geom_bar(stat="identity", position=position_dodge())
all_age_plot                 #plot check

all_edu_plot <- ggplot(data=all_run_edu, aes(x=edu_level, y=users, fill = run)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
all_edu_plot                 #plot check

all_emp_sta_plot <- ggplot(data=all_run_emp_status, aes(x=emp_status, y=users, fill = run)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
all_emp_sta_plot                 #plot check

all_emp_area_plot <- ggplot(data=all_run_emp_area, aes(x=emp_area, y=users, fill = run)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
all_emp_area_plot

#creating layout matrix for final plot
lay <- rbind(c(1, 2),
             c(3, 4),
             c(5, 5),
             c(5, 5))

# Generating a combined plot
grid.arrange(all_gender_plot, all_age_plot, all_edu_plot, all_emp_sta_plot, all_emp_area_plot, layout_matrix = lay)
