# Install and load the fs package
if (!require(fs)) {
  install.packages("fs")
}
library(fs)

# Define the path of your package
pkg_path <- "C:/Users/trevor.riley/Documents/Rwork/CiteSource_Project/CiteSource"

# Get a list of all directories in your package
dirs <- dir_ls(pkg_path, recurse = TRUE, type = "directory")

# Initialize a data frame to store the sizes
dir_sizes <- data.frame(Directory = character(), Size = numeric())

# Loop over the directories and calculate their size
for (dir in dirs) {
  size <- as.numeric(file_info(dir_ls(dir, recurse = TRUE, type = "file"))$size)
  dir_sizes <- rbind(dir_sizes, data.frame(Directory = dir, Size = sum(size) / (1024 * 1024)))
}

# Calculate the total size of the package
total_size <- sum(dir_sizes$Size)

# Calculate the size of the vignettes directory
vignettes_size <- dir_sizes[grep("C:/Users/trevor.riley/Documents/Rwork/CiteSource_Project/CiteSource/vignettes", dir_sizes$Directory), "Size"]
vignettes_size <- sum(vignettes_size)

# Calculate the size of the package without the vignettes
size_without_vignettes <- total_size - vignettes_size

# Print the size of the package without the vignettes
print(paste("The size of the package without the vignettes is", round(size_without_vignettes, 2), "MB"))

# Filter the dir_sizes data frame to only include directories in the vignettes folder
vignettes_dir_sizes <- dir_sizes[grep("C:/Users/trevor.riley/Documents/Rwork/CiteSource_Project/CiteSource/vignettes", dir_sizes$Directory), ]

# Print the size of the directories in the vignettes folder
print(vignettes_dir_sizes)