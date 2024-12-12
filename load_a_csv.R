my_df <- read.csv("/home/pierre/Projets_R/modality-statistics/spartacus.csv")

# Display the first few rows to verify
head(my_df)

# Print the structure of the data
str(my_df)

# Select rows where Age is greater than 25
gh_frontal_plane_dof_1 <- my_df[(my_df$joint =="glenohumeral") & (my_df$humeral_motion =="frontal plane elevation") & (my_df$degree_of_freedom ==1), ]
print(gh_frontal_plane_dof_1)

gh_frontal_plane_dof_1_in_vivo <- gh_frontal_plane_dof_1[(gh_frontal_plane_dof_1$in_vivo == "True"),]
print(gh_frontal_plane_dof_1_in_vivo)

gh_frontal_plane_dof_1_ex_vivo <- gh_frontal_plane_dof_1[(gh_frontal_plane_dof_1$in_vivo == "False"),]
print(gh_frontal_plane_dof_1_ex_vivo)


