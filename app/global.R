library(dplyr)



library(readr)

### The original 311 Service Requests data set is about 15 GB. Here I did some data filtering to remove unnecessary data for this project. 
### Also, I manually categorize 311 Service Complaints into 7 groups: General, Traffic, Environment, Noise, Neighborhood Condition, Infrastructure Condition and Virus (Covid-19).


#X311_Service_Requests_from_2010_to_Present <- read_csv("311_Service_Requests_from_2010_to_Present.csv",
#                                                       col_types = cols(Descriptor = col_skip(), Agency = col_skip(), `Closed Date` = col_skip(),
#                                                                          `Location Type` = col_skip(), `Incident Address` = col_skip(), 
#                                                                          `Street Name` = col_skip(), `Cross Street 1` = col_skip(), 
#                                                                          `Cross Street 2` = col_skip(), `Intersection Street 1` = col_skip(), 
#                                                                          `Intersection Street 2` = col_skip(), 
#                                                                          `Address Type` = col_skip(), City = col_skip(), 
#                                                                          Landmark = col_skip(), `Facility Type` = col_skip(), 
#                                                                          Status = col_skip(), `Due Date` = col_skip(), 
#                                                                          `Resolution Description` = col_skip(), 
#                                                                          `Resolution Action Updated Date` = col_skip(), 
#                                                                          `Community Board` = col_skip(), BBL = col_skip(), 
#                                                                          Borough = col_skip(), `X Coordinate (State Plane)` = col_skip(), 
#                                                                          `Y Coordinate (State Plane)` = col_skip(), 
#                                                                          `Open Data Channel Type` = col_skip(), 
#                                                                          `Park Facility Name` = col_skip(), 
#                                                                          `Park Borough` = col_skip(), `Vehicle Type` = col_skip(), 
#                                                                          `Taxi Company Borough` = col_skip(), 
#                                                                          `Taxi Pick Up Location` = col_skip(), 
#                                                                          `Bridge Highway Name` = col_skip(), 
#                                                                          `Bridge Highway Direction` = col_skip(), 
#                                                                          `Road Ramp` = col_skip(), `Bridge Highway Segment` = col_skip(), 
#                                                                          Location = col_skip()))

#X311_Service_Requests_from_2010_to_Present$`Created Date` <-as.Date(X311_Service_Requests_from_2010_to_Present$`Created Date`,"%m/%d/%Y %H:%M:%S")


#X311_Service_Requests_from_2010_to_Present = X311_Service_Requests_from_2010_to_Present%>%filter(`Created Date` >="2020-03-15")

# data$`Complaint Type` <- ifelse(grepl("Noise", data$`Complaint Type`), "Noise", 
#                                 ifelse(grepl("Traffic", data$`Complaint Type`) 
#                                        | grepl("Street", data$`Complaint Type`)
#                                        | grepl("Parking", data$`Complaint Type`)
#                                        | grepl("Vehicles", data$`Complaint Type`)
#                                        | grepl("Vehicle", data$`Complaint Type`)
#                                        | grepl("Taxi", data$`Complaint Type`)
#                                        | grepl("Bike", data$`Complaint Type`)
#                                        | grepl("Tunnel", data$`Complaint Type`)
#                                        | grepl("Bicycle", data$`Complaint Type`)
#                                        | grepl("Bus", data$`Complaint Type`)
#                                        | grepl("Scooter", data$`Complaint Type`)
#                                        | grepl("Driveway", data$`Complaint Type`)
#                                        | grepl("Sidewalk", data$`Complaint Type`)
#                                        | grepl("Ferry", data$`Complaint Type`)
#                                        | grepl("Highway", data$`Complaint Type`)
#                                        | grepl("Lot", data$`Complaint Type`), "Traffic", 
#                                        ifelse(grepl("WATER",data$`Complaint Type`)
#                                               |grepl("Water",data$`Complaint Type`)
#                                               |grepl("Boilers",data$`Complaint Type`)
#                                               |grepl("Electrical",data$`Complaint Type`)
#                                               |grepl("ELEVATOR",data$`Complaint Type`)
#                                               |grepl("Elevator",data$`Complaint Type`)
#                                               |grepl("Residential",data$`Complaint Type`)
#                                               |grepl("Food",data$`Complaint Type`)
#                                               |grepl("Building",data$`Complaint Type`)
#                                               |grepl("ELECTRIC",data$`Complaint Type`)
#                                               |grepl("WINDOW",data$`Complaint Type`)
#                                               |grepl("Cooling",data$`Complaint Type`)
#                                               |grepl("PLASTER",data$`Complaint Type`)
#                                               |grepl("Sewage",data$`Complaint Type`),"Infrastructure Condition",  !!!
#                                               ifelse(grepl("Dirty",data$`Complaint Type`)
#                                                      |grepl("sani",data$`Complaint Type`)
#                                                      |grepl("Sani",data$`Complaint Type`)
#                                                      |grepl("SANI",data$`Complaint Type`)
#                                                      |grepl("Plumbing",data$`Complaint Type`)
#                                                      |grepl("PLUMBING",data$`Complaint Type`)
#                                                      |grepl("Quality",data$`Complaint Type`)
#                                                      |grepl("Sewer",data$`Complaint Type`)
#                                                      |grepl("Tree",data$`Complaint Type`)
#                                                      |grepl("Storm",data$`Complaint Type`)
#                                                      |grepl("Litter",data$`Complaint Type`)
#                                                      |grepl("Waste",data$`Complaint Type`)
#                                                      |grepl("Plant",data$`Complaint Type`)
#                                                      |grepl("Graffiti",data$`Complaint Type`)
#                                                      |grepl("Mold",data$`Complaint Type`)
#                                                      |grepl("Snow",data$`Complaint Type`), "Environment", 
#                                                      ifelse(grepl("Drug",data$`Complaint Type`)
#                                                             |grepl("Homeless",data$`Complaint Type`)
#                                                             |grepl("Safety",data$`Complaint Type`)
#                                                             |grepl("SAFETY",data$`Complaint Type`)
#                                                             |grepl("Disorderly",data$`Complaint Type`)
#                                                             |grepl("Drinking",data$`Complaint Type`)
#                                                             |grepl("Polic",data$`Complaint Type`)
#                                                             |grepl("Panhandling",data$`Complaint Type`)
#                                                             |grepl("Hazardous",data$`Complaint Type`)
#                                                             |grepl("Posion",data$`Complaint Type`),"Neighborhood Condition", !!!
#                                                             ifelse(grepl("COVID",data$`Complaint Type`)
#                                                                    |grepl("Vaccine",data$`Complaint Type`)
#                                                                    |grepl("Reopen",data$`Complaint Type`),"Covid-related","General"))))))

#write.csv(X311_Service_Requests_from_2010_to_Present,"temp.csv", row.names = FALSE)


# ## Import data
# data = read_csv("data/311_filtered_preprocessed_data.csv")
# print("Preprocessing Data......")
# ## Filter the dataset to include data after 2019-08-12
# data = data %>%filter(`Created Date` >="2019-08-12")
# ## Format latitude and longitude
# data$Latitude = as.numeric(formatC(data$Latitude,digits=2,format="f"))
# data$Longitude = as.numeric(formatC(data$Longitude,digits=2,format="f"))
# ## Complaint Type Preprocessing
# data$`Complaint Type` = ifelse(grepl("Covid",data$`Complaint Type`),"Virus (Covid-19)",data$`Complaint Type`)
# data$`Complaint Type` = ifelse(grepl("Safety",data$`Complaint Type`),"Neighborhood Condition",data$`Complaint Type`)
# data$`Complaint Type` = ifelse(grepl("Living Condition",data$`Complaint Type`),"Infrastructure Condition",data$`Complaint Type`)
# ## PreCovid: 2019-08-12 - 2020-03-12
# data_preCovid = data %>%filter(`Created Date` <="2020-03-12")
# ## DuringCovid: 2020-08-12 - 2021-03-11
# data_duringCovid = data %>%filter(`Created Date` >="2020-08-12" & `Created Date` <"2021-03-12")
# ## PostCovid: 2021-03-12 - 2021-10-12
# data_postCovid = data %>%filter(`Created Date` >="2021-03-12")

## Groupby Complaint Type, Latitude, Longitude and count, comment out because the file size is too large for deployment
# data_preCovid_count = data_preCovid %>% 
#   group_by(`Complaint Type`, Latitude,Longitude) %>%
#   tally()
data_preCovid_count = read_csv("data/311_preCovid.csv")
## Add jitter to avoid latitude and longitude overlap
data_preCovid_count$Latitude <- jitter(data_preCovid_count$Latitude)
data_preCovid_count$Longitude <- jitter(data_preCovid_count$Longitude)
## Comment out because the file size is too large for deployment
# data_duringCovid_count = data_duringCovid %>% 
#   group_by(`Complaint Type`, Latitude,Longitude) %>%
#   tally()
data_duringCovid_count = read_csv("data/311_duringCovid.csv")
data_duringCovid_count$Latitude <- jitter(data_duringCovid_count$Latitude)
data_duringCovid_count$Longitude <- jitter(data_duringCovid_count$Longitude)
## Comment out because the file size is too large for deployment
# data_postCovid_count = data_postCovid %>% 
#   group_by(`Complaint Type`, Latitude,Longitude) %>%
#   tally()
data_postCovid_count = read_csv("data/311_postCovid.csv")
data_postCovid_count$Latitude <- jitter(data_postCovid_count$Latitude)
data_postCovid_count$Longitude <- jitter(data_postCovid_count$Longitude)
print("Data Preprocessed......")