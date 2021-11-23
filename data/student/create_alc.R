library(dplyr)

setwd("~/Desktop/opiskelumateriaali/iods-2021/IODS-project/data/student")
por <- read.table("student-por.csv", sep = ";", header=TRUE)
math <- read.table("student-mat.csv", sep = ";", header=TRUE)

# Add id
por_id <- por %>% mutate(id = 1000 + row_number()) 
math_id <- math %>% mutate(id = 2000 + row_number())

# Cols varying in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# Cols that are common identifiers used for joining the datasets. 
join_cols <- setdiff(colnames(por_id), free_cols)

# select only free cols
pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

# NOTE! There are NO 382 but 370 students that belong to both datasets

# two possibly different answers (math/por) to the same questions for each student.
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  group_by(.dots = join_cols) %>%  
    summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     
    paid=first(paid),                   
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  
  filter(n==2, id.m-id.p>650) %>%   # (id:s differ more than max within one dataset (649 here))
  
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>% # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

group_by(.dots = join_cols) %>%  
  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),   
    paid=first(paid),                   
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  )


setwd("~/Desktop/opiskelumateriaali/iods-2021/IODS-project/data/student")

write.csv(pormath, file = "pormath.csv", row.names = FALSE)
testdata <- read.csv("pormath.csv", stringsAsFactors = TRUE)
str(testdata)
dim(testdata) # 370 obs?









### Example ###

pormath_step1 <- bind_rows(por_id, math_id)

pormath_step2 <- group_by(pormath_step1, .dots = join_cols)

pormath_step3 <- pormath_step2 %>% 
                  summarise(
                  n=n(),
                  id.p=min(id),
                  id.m=max(id),
                  failures=round(mean(failures)),     #  Rounded mean for numerical
                  paid=first(paid),                   #    and first for chars
                  absences=round(mean(absences)),
                  G1=round(mean(G1)),
                  G2=round(mean(G2)),
                  G3=round(mean(G3))   
                 )

pormath_step4 <- filter(pormath_step3, n == 2, id.m-id.p > 650)

pormath_step5 <- inner_join(pormath_step4, pormath_free, by=c("id.p"="id"), suffix=c("",".p"))
  
pormath_step6 <- inner_join(pormath_step5, pormath_free,by=c("id.m"="id"),suffix=c("",".m"))

pormath_step7 <- ungroup(pormath_step6) %>% mutate(
                        alc_use = (Dalc + Walc) / 2,
                        high_use = alc_use > 2,
                        cid = 3000+row_number()
                        )


# Group_by

data_example <- data.frame(name = c("Elina","Elina","Elina","Anna","Jim"), x2 = letters[1:5])
data_example %>% group_by(name)

# Bind_rows

data1 <- data.frame(x1 = 1:5, x2 = letters[1:5])
data2 <- data.frame(x1 = 0, x3 = 5:9)
bind_rows(data1, data2) 
data1 %>% bind_rows(data2)

