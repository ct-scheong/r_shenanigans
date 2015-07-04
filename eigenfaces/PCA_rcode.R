#############################
# Shery Cheong
# September 30th 2014
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

library(pixmap)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = 1:38
view_list = c(  'P00A+000E+00', 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths
len_dl1 = length(dir_list_1)
len_dl2 = length(dir_list_2)

#################
# Find the total number of pixels in a picture

# Reusing code from hw 1
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
face_01_matrix = getChannels(face_01)
picdim = nrow(face_01_matrix)*ncol(face_01_matrix)
print(picdim)

# Pre-allocate a matrix with dimensions (number of pictures) x (number of pixels per picture)

faces_matrix = vector()

# Load all of the pictures; you can use code from homework 1

# preallocate an empty list. Code from previous hw.
pic_data = vector("list",length(pic_list)*length(view_list))

# creates list of image paths. Code from previous hw.
k=1
for (i in 1:length(pic_list)){
  for (j in 1:length(view_list)){
    pic_data[k] = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
    k = k + 1
  }}

# creates each individual image, store in vector 'row' and add it to the matrix going vertically down
for (k in 1:length(pic_data)){
    row = getChannels(read.pnm(file = toString(pic_data[k])))
    row.v = as.vector(row)
    faces_matrix = rbind(faces_matrix,row.v)
  }

dim(faces_matrix)

# Convert the matrix into a vector via as.vector and back with dim()
# Example:
# A = rbind(c(1,2,3),c(5,3,1))
# print(A)
# original.dimensions.A = dim(A)
# a = as.vector(A)
# print(a)
# dim(a) = original.dimensions.A
# print(a)

#Get the size of the matrix and print it
original.dimensions.faces_matrix = dim(faces_matrix)
print(original.dimensions.faces_matrix)

faces_matrix.vector = as.vector(faces_matrix)
dim(faces_matrix.vector) = original.dimensions.faces_matrix

#################
# # Problem 2b
#################
# Use colMeans() on your matrix to get "mean face" vector

meanface = colMeans(faces_matrix)

# Convert back to original size using dim()

dim(meanface) = dim(face_01_matrix)

# Generate image
meanfacepix = pixmapGrey(meanface)

# Plot and save
plot(meanfacepix)
title('hw02_2b: Mean Face')
filename = 'hw02_2b.png'
dev.copy(device=png, file=filename)
dev.off()

dim(faces_matrix)

# Subtract off meanface from matrix
faces_matrix.centered = sweep(faces_matrix, 2 , colMeans(faces_matrix) , "-")

#################
# Run prcomp() on your centered face matrix

faces_matrix.centered.pc = prcomp(faces_matrix.centered)
summary(faces_matrix.centered.pc)

#Calculate PVE and plot
pr.var = faces_matrix.centered.pc$sdev^2
pve=pr.var/sum(pr.var)

plot(pve, xlab="Principal Component", ylab="Proportion of
Variance Explained ", ylim=c(0,1),type='b')

#################
# Build your eigenface grid like the face grid in homework 1

eigenfaces = subset(faces_matrix.centered.pc$rotation, select = c(PC1:PC9))
dim(eigenfaces)

#creates empty vectors
eigenfaces_matrix = vector()
image = vector()

#combines images per row

myseqstart= as.vector(seq(from=1, to=9, by = 3))

for (k in myseqstart){
  index = k
  row1 = vector()
  for (i in 1:3){
  image = eigenfaces[,index]
  dim(image) = dim(face_01_matrix)
  row1 = cbind(row1,image)
  index = index + 1
}
eigenfaces_matrix = rbind(eigenfaces_matrix,row1)
}
# Generate image
eigenfaces_pix = pixmapGrey(eigenfaces_matrix)

# Plot and save
plot(eigenfaces_pix)
title('9 Eigenfaces')
filename = 'hw02_2d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # Problem 2e
#################
# Find the index of face yaleB01_P00A+010E+00.pgm
# Often, reading in the names and storing them as a list is a good idea
# Use the scores and loadings you found in 2c to reconstruct a face 
# by adding in 1 (or 5) bases at a time

# creates list of image names. Code from previous hw.

pic_name = vector("list",length(pic_list)*length(view_list))

k=1
for (i in 1:length(pic_list)){
  for (j in 1:length(view_list)){
    pic_name[k] = sprintf("%s_%s.pgm" , dir_list_1[pic_list[i]] , view_list[j])
    k = k + 1
  }}

# Find index of the face in question

index.face = match("yaleB01_P00A+010E+00.pgm",pic_name)

##### Adding 1 base at a time #####

# Add in the mean vector
reconstruct_matrix = vector()
reconstruct_matrix = cbind(reconstruct_matrix, meanface)

# Add in remaining images on row 1

for (i in 1:4){
  eigenface1 = faces_matrix.centered.pc$rotation[,1:i]
  scores1 = faces_matrix.centered.pc$x[,1:i]
  reconstruct = scores1 %*%  t(eigenface1)
  dim(reconstruct) = original.dimensions.faces_matrix
  reconstruct.sub = reconstruct[index.face,] + meanface
  dim(reconstruct.sub) = dim(face_01_matrix)
  reconstruct_matrix = cbind(reconstruct_matrix,reconstruct.sub)
}

# Add in rows 2 through 5

myseqstart= as.vector(seq(from=5, to=24, by = 5))
for (k in myseqstart){
  index = k
  row1 = vector()
    for (i in 1:5){
      eigenface1 = faces_matrix.centered.pc$rotation[,1:index]
      scores1 = faces_matrix.centered.pc$x[,1:index]
      reconstruct = scores1 %*%  t(eigenface1)
      dim(reconstruct) = original.dimensions.faces_matrix
      reconstruct.sub = reconstruct[index.face,] + meanface
      dim(reconstruct.sub) = dim(face_01_matrix)
      row1 = cbind(row1,reconstruct.sub)
      index = index + 1
  }
  reconstruct_matrix = rbind(reconstruct_matrix,row1)
}

# Generate image
reconstruct_pix = pixmapGrey(reconstruct_matrix)

# Plot and save
plot(reconstruct_pix)
title('Reconstruct with 1-24 eigenfaces')
filename = 'hw02_2e.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

##### Adding 5 bases at a time #####

# Add in the mean vector
reconstruct_matrix = vector()
reconstruct_matrix = cbind(reconstruct_matrix, meanface)

# Add in remaining images on row 1

for (i in 1:4){
  k = i*5
  eigenface1 = faces_matrix.centered.pc$rotation[,1:k]
  scores1 = faces_matrix.centered.pc$x[,1:k]
  reconstruct = scores1 %*%  t(eigenface1)
  dim(reconstruct) = original.dimensions.faces_matrix
  reconstruct.sub = reconstruct[index.face,] + meanface
  dim(reconstruct.sub) = dim(face_01_matrix)
  reconstruct_matrix = cbind(reconstruct_matrix,reconstruct.sub)
}

# Add in rows 2 through 5

myseqstart= as.vector(seq(from=25, to=120, by = 25))
for (k in myseqstart){
  index = k
  row1 = vector()
  for (i in 1:5){
    eigenface1 = faces_matrix.centered.pc$rotation[,1:index]
    scores1 = faces_matrix.centered.pc$x[,1:index]
    reconstruct = scores1 %*%  t(eigenface1)
    dim(reconstruct) = original.dimensions.faces_matrix
    reconstruct.sub = reconstruct[index.face,] + meanface
    dim(reconstruct.sub) = dim(face_01_matrix)
    row1 = cbind(row1,reconstruct.sub)
    index = index + 4
  }
  reconstruct_matrix = rbind(reconstruct_matrix,row1)
}

# Generate image
reconstruct_pix = pixmapGrey(reconstruct_matrix)

# Plot and save
plot(reconstruct_pix)
title('Reconstruct with 1-120 eigenfaces')
filename = 'hw02_2e_p2.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# Find the index of the faces to remove
# Find the index of face yaleB05_P00A+010E+00.pgm
# Remove pictures from matrix; run prcomp()

# creates list of subject names. Code from previous hw.

subject_name = vector("list",length(pic_list))

for (i in 1:length(pic_list)){
  subject_name[i] = sprintf("%s" , dir_list_1[i])
  }

# Find index of the face in question

index.face = match("yaleB05",subject_name)
index.face

firstpic = (index.face-1)*4 + 1
lastpic = firstpic+3

# Remove from faces matrix

faces_matrix.new = faces_matrix[-c(firstpic:lastpic),]
dim(faces_matrix.new)
# Generate mean face

meanface.new = colMeans(faces_matrix.new)

# Convert back to original size using dim()

faces_matrix.new.centered = sweep(faces_matrix.new, 2 , meanface.new , "-")

dim(faces_matrix.new.centered)

# Find index of the face in question

index.face.new = match("yaleB05_P00A+010E+00.pgm",pic_name)
index.face.new

# Run prcomp() on your centered face matrix

faces_matrix.new.centered.pc = prcomp(faces_matrix.new.centered)

# Calculate new score for image

eigenfaces.new = faces_matrix.new.centered.pc$rotation
dim(eigenfaces.new)
newscore = faces_matrix.centered[index.face.new,] %*% eigenfaces.new

dim(eigenfaces.new)
dim(newscore)

# Use new score to calculate new projection of the image
 
projection.new = newscore %*% t(eigenfaces.new)
dim(projection.new)
dim(projection.new) = dim(face_01_matrix)
dim(projection.new)
projection.new = projection.new + meanface.new

# Generate image
reconstruct_pix = pixmapGrey(projection.new)

# Plot and save
plot(reconstruct_pix)
title('Reconstruct subject 5 with all eigenfaces')
filename = 'hw02_2f.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# Generate original image

pic_data[index.face.new]

subject05 = faces_matrix[index.face.new]
dim(subject05) = dim(face_01_matrix)

subject05.pic = pixmapGrey(subject05)

# Plot and save
plot(subject05.pic)
title('Original Image')
filename = 'hw02_2f2.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()