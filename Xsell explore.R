# To do -------------------------------------------------------------------

# look at customer.att -> what does it do?
# result and customer analyses? what is left to do?
# find solution to calculating uplift


# 1.  Administration -------------------------------------------------------------------
library('tidyverse')
library('scales')
library('lubridate')
library('dataMeta') # to build data dictionary
library('xlsx') 

setwd("H:/R Statistics/Xsell") # This is my local dir where I put my work for the time being
theme_set(theme_light())

# 2.  Get data ----------------------------------------------------------------

# Query database (HAMSTER) for data
source("~/queryHAMSTER v02.R")
Xsell <- queryHAMSTER(str_replace_all(readLines("Xsell dump results v2.sql"), "[\r\n]" , ""))

Xsell <- Xsell %>% 
  rename(id = CASE_ID_T, # Rename columns for ease & consistency
         customer = KLANTNR,
         segs = SEGMENT,
         batch = BATCH_TYPE,
         customer.att = CUSTOMER_ATT,
         sub = CURRENT_SUB,
         sub.start.date = SUB_START_DATE,
         date.of.birth = DATE_OF_BIRTH,
         placed.time = CALLPLACEDTIME,
         answered.time = CALLANSWEREDTIME,
         length = LENGTH,
         is.contact = ISCONTACT,
         is.sale = IS_SALE,
         result = FINISHCODE
  ) %>% 
  mutate(sub.start.date = ymd(sub.start.date), # add correct date formatting 
         placed.time = ymd_hms(placed.time), 
         answered.time = ymd_hms(answered.time),
         # date.of.birth = dmy(date.of.birth), # too many parsing errors, due to missing date of birth
         is.contact = as.integer(is.contact), # convert datatypes to make proper use of booleans
         is.sale = as.integer(is.sale),
         placed.date = date(placed.time), # add additional date dimensions to timestamp
         placed.week = week(placed.time),
         placed.month = month(placed.time), 
         placed.moment = format(ymd_hms(placed.time), "%H:%M:%S"), # add the time of placement in hours, minutes, second
         placed.hour = hour(placed.time), # add hour in which attempt was placed
         dialer.tweak = if_else(placed.date < "2018-03-12", "no tweak", "tweak", "error"),  # add 'dialer.tweak' to signify the period in 
                                                                                            # which the dialer was correctly being used: after 13 march. 
         pilot.cat = if_else(segs == "Z", "control", "trial", "error"), # add grouping of segments into control and trial group 
                                                                        # (only segement Z is control group)
         result2 = case_when(# Think about renaming result2 to result, so you only have one result column...
           result == "Unassigned Number (ISDN Cause Code 01)" ~ "Unassigned Number", 
           result == "No Route To Destination (ISDN Cause Code 03)" ~ "No Route To Destination", 
           result == "User Alerting No Answer (ISDN Cause Code 19)" ~ "User Alerting No Answer",  
           result == "Incoming Calls Barred Within CUG (ISDN Cause Code 55)" ~ "Incoming Calls Barred Within CUG",  
           result == "No User Responding (ISDN Cause Code 18)" ~ "No User Responding", 
           result == "Call Rejected (ISDN Cause Code 21)" ~ "Call Rejected", 
           result == "Invalid Number Format (ISDN Cause Code 28)" ~ "Invalid Number Format", 
           result == "Normal, Specified (ISDN Cause Code 31)" ~ "Normal, Specified",
           result == "Recovery On Timer Expiry (ISDN Cause Code 102)" ~  "Recovery On Timer Expiry",
           result == "Number Changed (ISDN Cause Code 22)" ~ "Number Changed",
           TRUE ~ result),  # to retain al other strings
         result.cat = case_when( # to add a column which aggregates finishcodes (result) into broader categories
           result2 == "NoAnswer" ~ "No answer",
           result2 == "Voicemail" ~ "Voicemail",
           result2 == "Afgehandeld" ~ "Effective",
           result2 == "Terugbellen" ~ "Call back",
           result2 == "No Circuit" ~ "Technical",
           result2 == "System Hang Up" ~ "Technical",
           result2 == "GeenGehoor" ~ "No answer",
           result2 == "Bel Later Terug" ~ "Call back",
           result2 == "Unassigned Number" ~ "Bad number",
           result2 == "No User Responding" ~ "No answer",
           result2 == "Busy" ~ "Busy",
           result2 == "InGesprek" ~ "Busy",
           result2 == "Bad Number" ~ "Bad number",
           result2 == "No Route To Destination" ~ "Technical",
           result2 == "Reorder" ~ "Technical",
           result2 == "Call Rejected" ~ "Technical",
           result2 == "NotReached" ~ "No answer",
           result2 == "Policy Scheduled" ~ "Call back",
           result2 == "Invalid Number Format" ~ "Bad number",
           result2 == "Normal, Specified" ~ "Technical",
           result2 == "Ineffective Other" ~ "Technical",
           result2 == "Remote Hang Up" ~ "Technical",
           result2 == "NoLines" ~ "Technical",
           result2 == "Agent not available for callback" ~ "Technical",
           result2 == "Recovery On Timer Expiry" ~ "Technical",
           result2 == "Vacant Code" ~ "Technical",
           result2 == "User Alerting No Answer" ~ "Technical",
           result2 == "Number Changed" ~ "Bad number",
           result2 == "Failed to route call to agent." ~ "Technical",
           result2 == "Incoming Calls Barred Within CUG" ~ "Technical",
           result2 == "Agent Logout" ~ "Technical"
         )
  ) %>% 
  group_by(id)  %>% # add column to count the number of attempts within a case
  mutate(attempt.no = row_number(placed.time)) %>%  
  arrange(id, placed.time) 


# 2a. Old query from Bram, with an inner join.---------------------------------------------------
 # Xsellv1 <- queryHAMSTER(str_replace_all(readLines("Xsell dump results v1.sql"), "[\r\n]" , ""))
 # Xsellv1 <- Xsellv1 %>% 
 #   rename(id = CASE_ID_T, # Rename columns for ease & consistency
 #          customer = KLANTNR,
 #          segs = SEGMENT,
 #          batch = BATCH_TYPE,
 #          customer.att = CUSTOMER_ATT,
 #          sub = CURRENT_SUB,
 #          sub.start.date = SUB_START_DATE,
 #          date.of.birth = DATE_OF_BIRTH,
 #          placed.time = CALLPLACEDTIME,
 #          answered.time = CALLANSWEREDTIME,
 #          is.contact = ISCONTACT,
 #          is.sale = IS_SALE,
 #          result = FINISHCODE
 #   ) %>% 
 #   mutate(sub.start.date = ymd(sub.start.date), # add correct date formatting 
 #          placed.time = ymd_hms(placed.time), 
 #          answered.time = ymd_hms(answered.time),
 #          # date.of.birth = dmy(date.of.birth),
 #          is.contact = as.integer(is.contact), # convert datatypes to make proper use of booleans
 #          is.sale = as.integer(is.sale),
 #          placed.date = date(placed.time), # add additional date dimensions to timestamp
 #          placed.week = week(placed.time),
 #          placed.month = month(placed.time), 
 #          dialer.tweak = if_else(placed.date < "2018-03-12", "no tweak", "tweak", "error"),  # add columns 'dialer.tweak' and 'pilot.cat'
 #          pilot.cat = if_else(segs == "Z", "control", "trial", "error")
 #   )

# 2b. Build data dictionary (under construction) ---------------------------------------------------
# Isn't working yet, so can be ignored just now
# see: https://cran.r-project.org/web/packages/dataMeta/vignettes/dataMeta_Vignette.html


# var_desc <- c("Unique identifier of a record", 
#               "Unique identifier of a subscriber", 
#               "Attributed segment through model", 
#               "Batch",
#               "Proposition allowed to be offered to subscriber",
#               "Current subscription",
#               "Start date of current subscription",
#               "Date of birth of subscriber",
#               "Timestamp of dial attempt",
#               "Timestamp of answering of dial attempt",
#               "Boolean signaling whether contact was made",
#               "Boolean signaling wheter a sale was made",
#               "Code which signals what the conclusion of the dial attempt is",
#               "Signals whether record is in period where the dialer was tweaked to call attempts",
#               "Signals wheter record is in the trial or control group"
# )
# var_type <- c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1)
# linker <- build_linker(Xsell, variable_description = var_desc, variable_type = var_type)
# dict <- build_dict(my.data = Xsell, linker = linker, option_description = NULL, 
#                    prompt_varopts = FALSE)



# 3.  Tables ------------------------------------------------------------------

# show number of extreme cases
# extremes <- Xsell %>% 
#   group_by(segs, id) %>% 
#   summarise(attempts = n()) %>% 
#   arrange(segs,
#           desc(attempts)) %>% 
#   filter(attempts > 15)
#write.xlsx2(as.data.frame(extremes), "extremes.xlsx", sheetName = "extremes", col.names = TRUE, row.names = TRUE, showNA = TRUE)


# benefit track tables, split on segments
benefit.track.segs <- Xsell %>% 
  group_by(segs) %>% 
  summarise(records = n_distinct(id),
            attempts = n(), 
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  )

# show metrics mutations per week, change table to long format, to show difference between query between q v1 & v2

benefit.track.segs.weeks <- Xsell %>% 
  filter(placed.week > 5, # get relevant data, weeks < 5 contain little data. 
         segs %in% c("A", "C", "E", "I", "Z")) %>% # choice to only show a few segments. 
  group_by(segs, placed.week) %>% 
  summarise(records = n_distinct(id),
            attempts = n(), 
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  ) %>% 
  select(segs, 
         placed.week, 
         conversion, 
         sales.to.dials, 
         sales.to.connects, 
         avg.dials
  ) %>% 
  gather(key, value = value, 3:6
  ) %>% 
  spread(placed.week, value = value, fill = NA)
#write.xlsx2(as.data.frame(benefit.track.segs.weeks), "new_q.xlsx", sheetName = "metrics", col.names = TRUE, row.names = TRUE, showNA = TRUE)


benefit.track.segs.weeksv1 <- Xsellv1 %>% 
  filter(placed.week > 5,
         segs %in% c("A", "C", "E", "I", "Z")) %>% 
  group_by(segs, placed.week) %>% 
  summarise(records = n_distinct(id),
            attempts = n(), 
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  ) %>% 
  select(segs, 
         placed.week, 
         conversion, 
         sales.to.dials, 
         sales.to.connects, 
         avg.dials
  ) %>% 
  gather(key, value = value, 3:6
  ) %>% 
  spread(placed.week, value = value, fill = NA)
#write.xlsx2(as.data.frame(benefit.track.segs.weeksv1), "old_q.xlsx", sheetName = "metrics", col.names = TRUE, row.names = TRUE, showNA = TRUE)


# benefit tracking tables, per pilot category (control or trial) and total

benefit.track.cat <- Xsell %>% 
  group_by(pilot.cat) %>% 
  summarise(records = n_distinct(id),
            attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  )

benefit.track.tot <- Xsell %>% 
  summarise(records = n_distinct(id),
            attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  )  

#uplift.conversion <- make seperate objects and then do the calculation??


# benefit track tables, split on segments, pilot categories and total, per week

benefit.track.segs.week <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% # post 13 march period filter
  group_by(segs, 
           placed.week, 
           dialer.tweak) %>% 
  summarise(records = n_distinct(id),
            attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  ) %>% 
  gather(key = metric, value = value, 8:11) %>% 
  ggplot(aes(placed.week, value)) +
  geom_bar(aes(fill = dialer.tweak), stat = 'identity') +
  facet_grid(metric ~ segs, scales = 'free') +
  labs(x = "Week numbers",
       y = "Percentage",
       title = "Benefit tracking metrics per week and segment",
       subtitle = "Results post 13 march period"
       
  )
benefit.track.segs.week
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/benefit.segs.week.png", width = 9, height = 5)

benefit.track.cat.week <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% # post 13 march period filter
  group_by(pilot.cat, 
           placed.week
  )%>% 
  summarise(records = n_distinct(id),
            attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  ) %>% 
  gather(key = metric, value = value, 7:10) %>% 
  ggplot(aes(placed.week, value)) +
  geom_bar(aes(fill = metric), stat = 'identity') +
  facet_grid(metric ~ pilot.cat) + 
  labs(x = "Week numbers",
       y = "Percentage",
       title = "Benefit tracking metrics per week and trial & control group",
       subtitle = "Results post 13 march period"
       
  )
benefit.track.cat.week
ggsave(file="H:/R Statistics/Sanoma Data Science/plots/benefit.cats.week.png", width = 9, height = 5)

benefit.track.tot.week <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% # post 13 march period filter
  group_by(placed.week) %>% 
  summarise(records = n_distinct(id),
            attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale)
  ) %>% 
  mutate(conversion = round(sales / records * 100, 2),
         sales.to.dials = round(sales / attempts * 100, 2), 
         sales.to.connects = round(sales / contacts * 100, 2),
         avg.dials = round(attempts / records, 2)
  ) %>% 
  gather(key = metric, value = value, 7:9) %>% 
  ggplot(aes(placed.week, value)) +
  geom_bar(stat = 'identity') +
  facet_grid(metric ~ .)
benefit.track.tot.week
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/benefit.tot.week.png", width = 8, height = 3)

# 4.  Visualise -----------------------------------------------------------------

attempts <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% 
  group_by(placed.date) %>% 
  summarise(attempts = n()) %>% 
  ggplot(aes(placed.date, attempts)) +
  geom_bar(stat = 'identity') +
  labs(x = NULL,
       y = "# of Attempts",
       title = "Distribution of dials in the trial period"
  )
attempts  
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/dials.png", width = 8, height = 3)

segments <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% 
  group_by(placed.date,
           segs
  ) %>% 
  summarise(attempts = n()) %>% 
  ggplot(aes(placed.date, attempts)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'stack') +
  labs(x = NULL,
       y = "# of Attempts",
       title = "Segmentation in the dial attempts"
      )
segments
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/segments.png", width = 8, height = 3)
 
contacts <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% 
  group_by(placed.date,
           segs,
           is.contact
  ) %>% 
  summarise(attempts = n()) %>% 
  ggplot(aes(placed.date, attempts)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'stack' ) +
  facet_grid(is.contact ~ .) +
  labs(x = NULL,
       y = "# of Dials",
       title = "Dial attempts which succeeded or not",
       subtitle = "0 = unsuccessful, 1 = successful"
  )
contacts
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/contacts.png", width = 8, height = 3)

sales <- Xsell %>% 
  filter(is.contact == 1, 
         dialer.tweak == "tweak") %>% 
  group_by(placed.date,
           segs,
           is.sale,
           pilot.cat
  ) %>% 
  summarise(attempts = n()) %>% 
  ggplot(aes(placed.date, attempts)) +
  geom_bar(aes(fill = pilot.cat), stat = 'identity', position = 'stack' ) +
  facet_grid(is.sale ~ ., scales = 'free') +
  labs(x = NULL,
       y = "# of Contacts",
       title = "Contacts which converted into a sale or not",
       subtitle = "0 = unsuccessful, 1 = successful"
    
  )
sales
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/sales.png", width = 8, height = 3)

sales.tweak <- Xsell %>% 
  filter(is.sale == 1) %>% 
  group_by(placed.date,
           segs,
           pilot.cat,
           dialer.tweak
  ) %>% 
  summarise(sales = n()) %>% 
  ggplot(aes(placed.date, sales)) +
  geom_bar(aes(fill = dialer.tweak), stat = 'identity', position = 'stack') +
  facet_grid(. ~ pilot.cat) +
  labs(x = NULL,
       y = "# of Sales",
       title = "Sales in control versus trial group"
       
  )
sales.tweak
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/sales.tweak.png", width = 8, height = 3)
  
hour.attempt <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% 
  group_by(placed.hour) %>% 
  summarise(Attempts = n(),
            Contacts  = sum(is.contact),
            Sales = sum(is.sale)
            ) %>% 
  mutate(Attempt.perc = Attempts / sum(Attempts),
         Contact.perc = Contacts / Attempts,
         Sales.perc = Sales / Contacts
  ) %>% 
  gather(key = key, value = value, 5:7) %>% 
  ggplot(aes(placed.hour, value)) +
  geom_bar(stat = "identity", position = "stack", fill = "dodgerblue") +
  facet_grid(key ~ ., scales = "free") +
  scale_y_continuous(labels = percent) +
  labs(x = "Hour of the day",
       y = NULL,
       title = "% of Attempts, Contacts and Sales per hour",
       subtitle = "Post 13 march data"
       
  )
hour.attempt
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/hour.attempts.png", width = 6, height = 6)

hour.attempt.tab <- Xsell %>% 
  filter(dialer.tweak == "tweak") %>% 
  group_by(placed.hour) %>% 
  summarise(Attempts = n(),
            Contacts  = sum(is.contact),
            Sales = sum(is.sale)
  ) %>% 
  mutate(Attempt.perc = Attempts / sum(Attempts),
         Contact.perc = Contacts / Attempts,
         Sales.perc = Sales / Contacts
         )
#write.xlsx2(as.data.frame(hour.attempt.tab), "hour.attempt.tabular.xlsx", sheetName = "distribution", col.names = TRUE, row.names = FALSE, showNA = TRUE)

# 5.  Individual cases --------------------------------------------------------

case1 <- Xsell %>%
  filter(id == 2501992) %>% 
  arrange(placed.time) %>% 
  mutate(attempt.no = 1:n())

case2 <- Xsell %>%
  filter(id == 2220557) %>% 
  arrange(placed.time) %>% 
  mutate(attempt.no = 1:n()) # count the number of attempts within a case, this doesn't work
  
# proof of concept: adding dial attempt numbers in a column, results in adding 'attempt number' column
case3 <- Xsell %>%
  filter(id %in% c(2268684, 2220557, 2501992)) %>% 
  group_by(id)  %>% 
  mutate(attempt.no = row_number(placed.time)) %>% # count the number of attempts within a case
  arrange(id, placed.time) 


# 6.  Call attempt exploration ------------------------------------------------

# Distribution of id's over the attempts
attempt.dist <- Xsell %>% 
  filter(attempt.no < 16,
         dialer.tweak == "tweak") %>% 
  group_by(attempt.no,
           segs
           
  ) %>% 
  summarise(number = n_distinct(id)
    
  ) %>% 
  ggplot(aes(attempt.no, number)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'stack') +
  facet_grid(. ~ segs) +
  labs(x = "Number of attempts",
       y = "Number of cases",
       title = "Distribution of cases over attempts",
       subtitle = "Post 13 march period, excluded more than 15 dial attempts"
    
  ) +
  theme(legend.position="none") # remove legend

attempt.dist
ggsave(file="H:/R Statistics/Sanoma Data Science/plots/attempt.distribution.png", width = 9, height = 5)

# visualise attempts vs. contact and sales rates per segement
# filtered the data for period after 13th march and attempts less than 16
attempt.viz.perc <- Xsell %>% 
  filter(attempt.no < 16,
         dialer.tweak == "tweak") %>%
  group_by(attempt.no, 
           segs
  ) %>%
  summarise(attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale),
            contact.perc = round(contacts / attempts, 2),
            sales.perc = round(sales / contacts, 2)
            
  ) %>% 
  gather(key = key, value = value, 6:7) %>% 
  ggplot(aes(attempt.no, value)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'dodge') +
  facet_grid(key ~ segs, scales = 'free') + 
  scale_y_continuous(labels = percent) +
  labs(x = "Number of attempts",
       y = NULL,
       title = "Percentage of contacts and sales, per number of dial attempts per segment",
       subtitle = "Post 13 march period, excluded more than 15 dial attempts"
  ) + 
  theme(legend.position="none") # remove legend

attempt.viz.perc
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/attempt.segs.relative.png", width = 9, height = 5)


# visualise attempts vs. absolute numbers of contact and sales per segement
# filtered the data for period after 13th march and attempts less than 16
attempt.viz.abs <- Xsell %>% 
  filter(attempt.no < 16,
         dialer.tweak == "tweak") %>%
  group_by(attempt.no, 
           segs 
  ) %>%
  summarise(attempts = n(),
            contacts = sum(is.contact),
            sales = sum(is.sale),
            contact.perc = round(contacts / attempts * 100, 2),
            sales.perc = round(sales / contacts * 100, 2)
            
  ) %>% 
  gather(key = key, value = value, 4:5) %>% 
  ggplot(aes(attempt.no, value)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'dodge') +
  facet_grid(key ~ segs, scales = 'free') +
  labs(x = "# of attempts",
       y = "# of contacts/sales",
       title = "Absolute contacts and sales, per number of dial attempts per segment",
       subtitle = "Post 13 march period, excluded more than 15 dial attempts"
       
  ) + 
  theme(legend.position="none") # remove legend

attempt.viz.abs
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/attempt.segs.absolute.png", width = 9, height = 5)


# 7.  Result analyses (Finishcodes) -------------------------------------------

# Distribution of results in attempts, where is.contact == 0 
result.dist <- Xsell %>% 
  filter(dialer.tweak == "tweak", # check only for the post 13 march period
         is.contact == 0) %>% 
  group_by(segs, 
           result.cat
  ) %>% 
  summarise(rows = n()
  ) %>% 
  mutate(perc = round(rows / sum(rows), 2)
  ) %>% 
  arrange(segs, desc(rows)) %>%
  #filter(perc > 0.01) %>% 
  ggplot(aes(reorder(result.cat, perc), perc)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'stack') +
  facet_grid(. ~ segs) +
  # coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(y = NULL,
       x = NULL,
       title = "Percentage of finishcodes per segment in non-contacts",
       subtitle = "Post 13 march data"
  ) + 
  theme(legend.position = "none") + # remove legend
  theme(axis.text.x = element_text(angle = 90))
result.dist
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/attempt.dist.contact0.png", width = 10, height = 4)

# Distribution of results where is.contact == 1
result.dist.is.contact <- Xsell %>% 
  filter(dialer.tweak == "tweak", # check only for the post 13 march period
         is.contact == 1) %>% 
  group_by(segs, 
           result.cat
  ) %>% 
  summarise(rows = n()
  ) %>% 
  mutate(perc = round(rows / sum(rows), 2)
  ) %>% 
  arrange(segs, desc(rows)) %>%
  filter(perc > 0.05) %>%  # filter for realy small categories
  ggplot(aes(reorder(result.cat, perc), perc)) +
  geom_bar(aes(fill = segs), stat = 'identity', position = 'dodge') +
  facet_grid(. ~ segs) +
  scale_y_continuous(labels = percent) +
  # coord_flip() +
  labs(y = NULL,
       x = NULL,
       title = "Percentage of finishcodes per segment in Contacts",
       subtitle = "Post 13 march data"
  ) + 
  theme(legend.position = "none") + # remove legend
  theme(axis.text.x = element_text(angle = 90))
result.dist.is.contact
#ggsave(file="H:/R Statistics/Sanoma Data Science/plots/attempt.dist.contact1.png", width = 10, height = 4)

# find finish codes and their frequency in the data set
finish.codes <- Xsell %>%
  group_by(result2) %>% 
  summarise(rows = n()) %>% 
  arrange(desc(rows))
#write.xlsx2(as.data.frame(finish.codes), "finish.codes.xlsx", sheetName = "codes", col.names = TRUE, row.names = FALSE, showNA = TRUE)
  
# Build an attempt matrix to find the frequency of finish codes in call attempts
attempt.matrix <- Xsell %>% 
  filter(attempt.no < 11,
         segs == "Z"
  ) %>% 
  select(segs, id, result.cat, attempt.no) %>%
  spread(attempt.no, value = result.cat, fill = NA) %>% 
   rename(first   = "1",
          second  = "2", 
          third   = "3",
          fourth  = "4",
          fifth   = "5",
          sixth   = "6",
          seventh = "7",
          eighth   = "8",
          ninth   = "9",
          tenth   = "10"
     
  ) %>% 
  mutate(sequence = paste(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth, sep = ', ')) %>% 
  group_by(segs, sequence) %>% 
  summarise(freq = n()) %>% 
  mutate(freq.perc = round(freq / sum(freq), 2)) %>% 
  filter(freq.perc >= 0.01) %>%
  arrange(segs, desc(freq))
# write.xlsx2(as.data.frame(attempt.matrix), "finish.sequences.xlsx", sheetName = "sequences", col.names = TRUE, row.names = FALSE, showNA = TRUE)

# 8.  Customer.att analyses -------------------------------------------------------

att.contact <- Xsell %>% 
  group_by(customer.att,
           is.contact
           
  ) %>% 
  summarise(rows = n()) %>% 
  mutate(contact.perc = round(rows / sum(rows), 3))


att.sale <- Xsell %>% 
  group_by(customer.att,
           is.sale
    
  ) %>% 
  summarise(rows = n()) %>% 
  mutate(sale.perc = round(rows / sum(rows), 3))



# 9.  Sales per hour table and visualisation ---------------------------------------------

# Sales data
sales.data <- Xsell %>% 
  mutate(placed.week = as.character(placed.week)) %>% # to have all weeks in the graph
  filter(dialer.tweak == "tweak") %>% 
  group_by(segs, placed.week) %>% 
  summarise(hours = round(sum(length) / 3600, 2),
            sales = sum(is.sale)
  ) %>% 
  mutate(sales.hour = round(sales / hours, 2)) 

# turning data into a simple table 

## (1): sales per hour
sales.hour.table <- sales.data %>% 
  select(-hours, -sales) %>% 
  spread(key = placed.week, value = sales.hour)
#write.xlsx2(as.data.frame(sales.hour.table), "sales.hour.table.xlsx", sheetName = "sales.hour", col.names = TRUE, row.names = FALSE, showNA = TRUE)

## (2): sales
sales.table <- sales.data %>% 
  select(-hours, -sales.hour) %>% 
  spread(key = placed.week, value = sales)
#write.xlsx2(as.data.frame(sales.table), "sales.table.xlsx", sheetName = "sales", col.names = TRUE, row.names = FALSE, showNA = TRUE)

## (3): hour
hour.table <- sales.data %>% 
  select(-sales.hour, -sales) %>% 
  spread(key = placed.week, value = hours)
#write.xlsx2(as.data.frame(hour.table), "hour.table.xlsx", sheetName = "hours", col.names = TRUE, row.names = FALSE, showNA = TRUE)

# turning data into graph (1): bar chart for sales and hours, per segment
sales.hour.viz <- sales.data %>% 
  select(-sales.hour) %>% 
  gather(key = key, value = value, 3:4) %>% # combine hours and sales
  ggplot(aes(x = placed.week)) +
  geom_bar(aes(y = value, fill = key), stat = "identity", position = "dodge") +
  facet_wrap(~ segs, nrow = 3, ncol = 5, scales = "fixed") +
  labs(x = "Week",
       y = "Amount",
       title = "Post 13th march talk time in hours and sales",
       subtitle = "Per segment"
  )
sales.hour.viz

# turning data into graph (2): line chart for sales per hour
sales.per.hour.viz <- sales.data %>% 
  select(-sales, -hours) %>% 
  ggplot(aes(x = placed.week)) +
  geom_line(aes(y = sales.hour), group = 1, size = 1.5, alpha = 0.8, color = "dodgerblue") +
  facet_wrap(~ segs, nrow = 3, ncol = 5, scales = "fixed") +
  labs(x = "Week",
       y = "Sales per hour",
       title = "Post 13th march sales per hour",
       subtitle = "Per segment"
  )
sales.per.hour.viz

# Sales data aggregate on hour of placement

## underlying data
sales.data.hour <- Xsell %>% 
  mutate(placed.hour = as.character(placed.hour)) %>% # to have all weeks in the graph
  filter(dialer.tweak == "tweak") %>% 
  group_by(segs, placed.hour) %>% 
  summarise(hours = round(sum(length) / 3600, 2),
            sales = sum(is.sale)
  ) %>% 
  mutate(sales.hour = round(sales / hours, 2)) 

## table

# (1): sales per hour
sales.per.hour.table.v2 <- sales.data.hour %>% 
  select(-hours, -sales) %>% 
  spread(key = placed.hour, value = sales.hour)
#write.xlsx2(as.data.frame(sales.per.hour.table.v2), "sales.hour.table.v2.xlsx", sheetName = "sales", col.names = TRUE, row.names = FALSE, showNA = TRUE)

## (2): sales
sales.table.v2 <- sales.data.hour %>% 
  select(-hours, -sales.hour) %>% 
  spread(key = placed.hour, value = sales)
#write.xlsx2(as.data.frame(sales.table.v2), "sales.table.v2.xlsx", sheetName = "sales", col.names = TRUE, row.names = FALSE, showNA = TRUE)

## (3): hour
hour.table.v2 <- sales.data.hour %>% 
  select(-sales.hour, -sales) %>% 
  spread(key = placed.hour, value = hours)
#write.xlsx2(as.data.frame(hour.table.v2), "hour.table.v2.xlsx", sheetName = "sales", col.names = TRUE, row.names = FALSE, showNA = TRUE)

# turning data into graph: line chart for sales per hour
sales.per.hour.viz2 <- sales.data.hour %>% 
  select(-sales, -hours) %>% 
  filter(placed.hour != "8",
         placed.hour != "9",
         placed.hour != "21" # these intervals screw up the graph due to some extremes. so excluded these. 
  ) %>% 
  ggplot(aes(x = placed.hour)) +
  geom_line(aes(y = sales.hour), group = 1, size = 1.5, alpha = 0.8, color = "dodgerblue") +
  facet_wrap(~ segs, nrow = 3, ncol = 5, scales = "fixed") +
  labs(x = "Hour of the day",
       y = "Sales per hour",
       title = "Post 13th march sales per hour over the hours of the day"
  )
sales.per.hour.viz2

# Z.  Trash can ---------------------------------------------------------------

birth <- Xsell %>% 
  mutate(no.birth = if_else(date.of.birth == "--", 1, 0, 0)) %>% 
  group_by(segs, no.birth) %>% 
  summarise(rows = n()) %>% 
  mutate(no.birt.perc = round(rows / sum(rows), 2))


birth2 <- Xsell.attempts %>% 
  filter(attempt.no == 1) %>% 
  mutate(no.birth = if_else(date.of.birth == "--", 1, 0, 0)) %>% 
  group_by(segs, no.birth) %>% 
  summarise(rows = n()) %>% 
  mutate(no.birt.perc = round(rows / sum(rows), 2))


nana <- Xsell %>% 
  filter(is.na(result.cat))

segs.length <- Xsell %>%
  mutate(placed.week = as.character(placed.week)) %>% 
  filter(dialer.tweak == "tweak",
         placed.week < 20) %>% 
  group_by(segs, result.cat, placed.week) %>% 
  summarise(length = round(sum(length) / 3600, 2),
            sales = sum(is.sale)
  ) %>% 
  mutate(sales.hour = round(sales / length, 2))


segs.length.table <- segs.length %>% 
  spread(key = placed.week, value = length)
#write.xlsx2(as.data.frame(segs.length), "segs.length.xlsx", sheetName = "length", col.names = TRUE, row.names = FALSE, showNA = TRUE)


segs.length.perc <- segs.length %>%
  group_by(segs, placed.week) %>% 
  mutate(length.perc = round(length / sum(length) * 100, 2)) %>% 
  select(-length) %>% 
  spread(key = placed.week, value = length.perc)
#write.xlsx2(as.data.frame(segs.length.perc), "segs.length.perc.xlsx", sheetName = "length.perc", col.names = TRUE, row.names = FALSE, showNA = TRUE)

segs.length.plot <- segs.length %>%
  filter(!is.na(result.cat)) %>% 
  group_by(segs, placed.week) %>% 
  mutate(length.perc = round(length / sum(length), 2)) %>% 
  select(-length, -sales.hour) %>% 
  ggplot(aes(placed.week, length.perc)) +
  geom_bar(aes(fill = result.cat), position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent) +
  facet_grid(segs ~ .) +
  scale_fill_brewer(palette = "Blues")
segs.length.plot

# Comparison to ISP Glas project

isp.compare <- Xsell %>% 
  filter(placed.month %in% c(2,3,4)) %>% 
  group_by(placed.month) %>% 
  summarise(RECORDS = n_distinct(customer),
            ATTEMPTS = n(),
            EFFECTIEVEN = sum(is.contact),
            POSITIEVEN = sum(is.sale)
            )
#write.xlsx2(as.data.frame(isp.compare), "isp.compare.xlsx", sheetName = "stats", col.names = TRUE, row.names = FALSE, showNA = TRUE)

