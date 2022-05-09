library(shapes)
library(tidyverse)

####Generalized Procrustes Analysis####

#Need a version of the landmark file with just the landmarks
goali_landmarks <- read_csv("data/goali_landmarks.csv")

#two vines are mislabelled according to scans
goali_landmarks$accession <- gsub("11618070", "16118070",goali_landmarks$accession)
goali_landmarks$accession <- gsub("16120921", "16129021",goali_landmarks$accession)

goali_landmarks_info <- goali_landmarks %>% select(accession, year)
goali_landmarks <- goali_landmarks %>% select(x1:y21)

n_landmarks <- 21
n_leaves <- dim(goali_landmarks)[1]

write.table(as.matrix(goali_landmarks), col.names=F, row.names=F, file='data/goali_landmarks_raw.txt')

morpho_reformat_gpa <- read.in('data/goali_landmarks_raw.txt', n_landmarks, 2)

dim(morpho_reformat_gpa)

# fit the GPA
GPA <- procGPA(morpho_reformat_gpa, reflect=TRUE)

#
stdscores <- as.matrix(GPA$stdscores)
pca_results <- cbind(goali_landmarks_info, stdscores)
write.csv(pca_results, file="data/goali_PC_scores_info.csv", quote=FALSE, row.names = F)
write.csv(as.matrix(GPA$stdscores), file="data/goali_PC_scores.csv", quote=FALSE)
write.csv(as.matrix(GPA$percent), file="data/goali_PC_percents.csv", quote=FALSE)
write.csv(as.matrix(GPA$rotated), file="data/goali_GPA_rotated.csv", quote=FALSE)

#restructuring of matrix

morpho_GPA_rotated_flat <- matrix(nrow=n_leaves, ncol=(n_landmarks*2))

morpho_GPA_rotated <- as.matrix(GPA$rotated)
for(j in c(1:n_leaves)) {
  # extract all coordinates from the original table as blocks of 42 (n_landmarks*2) rows,
  # each representing the x coordinates of a leaf, one by one, as calculated from j.
  sub.data <- as.matrix(morpho_GPA_rotated[ (1+42*(j-1)):((1+42*(j-1))+41), 1])
  sub.data.x <- as.matrix(sub.data[1:n_landmarks,])
  sub.data.y <- as.matrix(sub.data[(n_landmarks+1):(n_landmarks*2),])
  
  # dissect out each x and y coordinate of the landmark data and put it into every other 
  # column of a single row (for a single leaf) in the overall table
  
  for (i in 1:n_landmarks){
    morpho_GPA_rotated_flat[j,(i*2-1)] <- sub.data.x[i, 1]
    morpho_GPA_rotated_flat[j, (i*2)] <- sub.data.y[i, 1]
  }
}

colnames(morpho_GPA_rotated_flat) <- c("x1_adjusted", "y1_adjusted", "x2_adjusted", "y2_adjusted", "x3_adjusted", "y3_adjusted", "x4_adjusted", "y4_adjusted", "x5_adjusted", "y5_adjusted", "x6_adjusted", "y6_adjusted", "x7_adjusted", "y7_adjusted", "x8_adjusted", "y8_adjusted", "x9_adjusted", "y9_adjusted", "x10_adjusted", "y10_adjusted", "x11_adjusted", "y11_adjusted", "x12_adjusted", "y12_adjusted", "x13_adjusted", "y13_adjusted", "x14_adjusted", "y14_adjusted", "x15_adjusted", "y15_adjusted", "x16_adjusted", "y16_adjusted", "x17_adjusted", "y17_adjusted","x18_adjusted", "y18_adjusted","x19_adjusted", "y19_adjusted","x20_adjusted", "y20_adjusted","x21_adjusted", "y21_adjusted")

head(morpho_GPA_rotated_flat)

shapepca(GPA, pcno=c(1:3), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4))

shapepca(GPA, pcno=c(1:3), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4), type="s")

shapepca(GPA,pcno=c(1), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4))

#Add info for Procrustes adjusted landmarks

goali_landmarks_adjusted <- cbind(goali_landmarks_info,goali_landmarks,morpho_GPA_rotated_flat)
write.table(goali_landmarks_adjusted, "data/goali_landmarks_adjusted.csv", sep=",", row.names = F, col.names = T)

#ADD PCs

goali_landmarks_all_info <- cbind(goali_landmarks_info,stdscores, goali_landmarks,morpho_GPA_rotated_flat)

write.table(goali_landmarks_all_info, "data/goali_pcs_with_landmarks.csv", sep=",", row.names = F, col.names = T)

###Area calculations (using raw values)

# Read in dataset, summarize, and list names

data <- read_csv("data/goali_landmarks_adjusted_image_size.csv")

summary(data)

names(data)

# Calculate the overall area of a leaf using the shoestring algorithm
# Convert to cm2 using the conversion factor
# Attach and detach data, because of the large number of landmarks to keep track of

detach(data)

attach(data)

data$all_area <- (0.5*abs(
  
  (x4*y3 + x3*y2 + x2*y1 + x1*y6 + x6*y14 + x14*y15 + x15*y16 + x16*y17 + x17*y18 + x18*y19 + x19*y20 + x20*y21 + x21*y13 + x13*y4) - 
    
    (y4*x3 + y3*x2 + y2*x1 + y1*x6 + y6*x14 + y14*x15 + y15*x16 + y16*x17 + y17*x18 + y18*x19 + y19*x20 + y20*x21 + y21*x13 + y13*x4) 
  
))/(px2_cm2)

detach(data)

# Calculate the convex area of a leaf using the shoestring algorithm (remove the sinuses compared to overall area)
# Convert to cm2 using the conversion factor
# Attach and detach data, because of the large number of landmarks to keep track of

attach(data)

data$convex_area <- (0.5*abs(
  
  (x4*y3 + x3*y2 + x2*y1 + x1*y6 + x6*y14 + x14*y15 + x15*y18 + x18*y21 + x21*y13 + x13*y4) - 
    
    (y4*x3 + y3*x2 + y2*x1 + y1*x6 + y6*x14 + y14*x15 + y15*x18 + y18*x21 + y21*x13 + y13*x4) 
  
))/(px2_cm2)

detach(data)


names(data)
head(data)
attach(data)

#Vein areas.

data$a_mid <- (0.5*abs(
  
  (x4*y3 + x3*y12 + x12*y11 + x11*y21 + x21*y13+ x13*y4) - 
    
    (y4*x3 + y3*x12 + y12*x11 + y11*x21 + y21*x13+ y13*x4)
))/
  (px2_cm2)


data$a_distal <- (0.5*abs(
  
  (x3*y2 + x2*y9 + x9*y8 + x8*y18 + x18*y10 + x10*y3) - 
    
    (y3*x2 + y2*x9 + y9*x8 + y8*x18 + y18*x10 + y10*x3) 
  
))/(px2_cm2)


data$a_proximal <- (0.5*abs(
  
  (x2*y1 + x1*y6 + x6*y5 + x5*y15 + x15*y7 + x7*y2) - 
    
    (y2*x1 + y1*x6 + y6*x5 + y5*x15 + y15*x7 + y7*x2) 
  
))/(px2_cm2)

data$a_mid_branch <- (0.5*abs(
  
  (x11*y12 + x12*y20 + x20*y11) - 
    
    (y11*x12 + y12*x20 + y20*x11) 
  
))/(px2_cm2)


data$a_distal_branch <- (0.5*abs(
  
  (x8*y9 + x9*y17 + x17*y8) - 
    
    (y8*x9 + y9*x17 + y17*x8) 
  
))/(px2_cm2)


data$a_petiolar <- (0.5*abs(
  
  (x5*y6 + x6*y14 + x14*y5) - 
    
    (y5*x6 + y6*x14 + y14*x5) 
  
))/(px2_cm2)

detach(data)

attach(data)

data$veins <- a_mid + a_distal + a_proximal + a_mid_branch + a_distal_branch + a_petiolar

detach(data)

attach(data)

#Areas of blade regions. These regions are defined by the borders of the veins and drawing lines from the sinuses to the petiole of the leaf. They will be illustrated in the figure
data$r1 <- (0.5*abs( (x5*y14 + x14*y15 + x15*y5) - (y5*x14 + y14*x15 + y15*x5) ) ) /(px2_cm2)

data$r2<- (0.5*abs(
  
  (x2*y7 + x7*y15 + x15*y16 + x16*y2) - 
    
    (y2*x7 + y7*x15 + y15*x16 + y16*x2)
  
))/(px2_cm2)

data$r3<- (0.5*abs(
  
  (x2*y16 + x16*y17 + x17*y9 + x9*y2) - 
    
    (y2*x16 + y16*x17 + y17*x9 + y9*x2)
  
))/(px2_cm2)

data$r4 <- (0.5*abs( (x8*y17 + x17*y18 + x18*y8) - (y8*x17 + y17*x18 + y18*x8) ) ) /(px2_cm2)

data$r5<- (0.5*abs(
  
  (x3*y10 + x10*y18 + x18*y19 + x19*y3) - 
    
    (y3*x10 + y10*x18 + y18*x19 + y19*x3)
  
))/(px2_cm2)

data$r6<- (0.5*abs(
  
  (x3*y19 + x19*y20 + x20*y12 + x12*y3) - 
    
    (y3*x19 + y19*x20 + y20*x12 + y12*x3)
  
))/(px2_cm2)

data$r7 <- (0.5*abs( (x11*y20 + x20*y21 + x21*y11) - (y11*x20 + y20*x21 + y21*x11) ) ) /(px2_cm2)
detach(data)

attach(data)

# Calculate blade area as the overall area of the leaf minus vein area

data$blade <- r1 + r2 + r3 + r4 + r5 + r6 + r7 

# Calculate vein-to-blade ratio 

data$veins_to_blade <- data$veins / data$blade

# Calculate convex to actual ratio 
data$solidity <- data$all_area / data$convex_area
detach(data)

attach(data)

#Calculate area of the sinuses

data$sinus_dist <- (0.5*abs( (x19*y18 + x18*y21 + x21*y20 + x20*y19) - (y19*x18 + y18*x21 + y21*x20 + y20*x19) ) ) /(px2_cm2)

data$sinus_prox <- (0.5*abs( (x16*y15 + x15*y18 + x18*y17 + x17*y16) - (y16*x15 + y15*x18 + y18*x17 + y17*x16) ) ) /(px2_cm2)

detach(data)

#Calculate lobing

data <- data %>% mutate(distal_lobing=sqrt( (x19-x3)^2 + (y19-y3)^2 ) / sqrt( (x18-x3)^2 + (y18-y3)^2 ), proximal_lobing=sqrt( (x16-x2)^2 + (y16-y2)^2 ) / sqrt( (x15-x2)^2 + (y15-y2)^2 ))

#Calculate midvein length

data <- data %>% mutate(px_cm=sqrt(px2_cm2))
data <- data %>% mutate(x3_4=(x3+x4)/2) %>% mutate(y3_4=(y3+y4)/2)
data <- data %>% mutate(midvein_length=sqrt(((x3_4-x21)^2) +((y3_4-y21)^2))) %>% mutate(midvein_length=midvein_length/px_cm)
hist(data$midvein_length)

#Calculate distal vein length

data <- data %>% mutate(x2_3=(x2+x3)/2) %>% mutate(y2_3=(y2+y3)/2)
data <- data %>% mutate(distal_length=sqrt(((x2_3-x18)^2) +((y2_3-y18)^2))) %>% mutate(distal_length=distal_length/px_cm)
hist(data$distal_length)

#Calculate proximal vein length 

data <- data %>% mutate(x1_2=(x1+x2)/2) %>% mutate(y1_2=(y1+y2)/2)
data <- data %>% mutate(proximal_length=sqrt(((x1_2-x15)^2) +((y1_2-y15)^2))) %>% mutate(proximal_length=proximal_length/px_cm)
hist(data$proximal_length)

#Calculate branching vein lengths 
data <- data %>% mutate(x5_6=(x5+x6)/2) %>% mutate(y5_6=(y5+y6)/2)
data <- data %>% mutate(petiolar_length=sqrt(((x5_6-x14)^2) +((y5_6-y14)^2))) %>% mutate(petiolar_length=petiolar_length/px_cm)
hist(data$petiolar_length)

data <- data %>% mutate(x8_9=(x8+x9)/2) %>% mutate(y8_9=(y8+y9)/2)
data <- data %>% mutate(dist_branch_length=sqrt(((x8_9-x17)^2) +((y8_9-y17)^2))) %>% mutate(dist_branch_length=dist_branch_length/px_cm)
hist(data$dist_branch_length)

data <- data %>% mutate(x11_12=(x11+x12)/2) %>% mutate(y11_12=(y11+y12)/2)
data <- data %>% mutate(mid_branch_length=sqrt(((x11_12-x20)^2) +((y11_12-y20)^2))) %>% mutate(mid_branch_length=mid_branch_length/px_cm)
hist(data$mid_branch_length)

#Remove columns I don't need

data <- data %>% dplyr::select(-x3_4, -y3_4,-x2_3, -y2_3, -x1_2, -y1_2, -x5_6, -y5_6, -x8_9, -y8_9, -x11_12,-y11_12)

#two vines are mislabelled according to scans
data$accession <- gsub("11618070", "16118070",data$accession)
data$accession <- gsub("16120921", "16129021",data$accession)

#Add in population column 

data <- data %>% mutate(pop=ifelse(accession >=16118001 & accession <=16118125 , "pop1", ifelse(accession >=16119001 & accession <=16119100 , "pop2", ifelse(accession >=16117001 & accession <=16117150 , "pop3", ifelse(accession >=16129001 & accession <=16129075 , "pop4", ifelse(accession >=16113001 & accession <=16113050 , "pop5", accession))))))

write.table(data, "data/goali_all_data.csv", sep=",", row.names = F, col.names = T)
