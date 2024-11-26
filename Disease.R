# Load necessary libraries
library(rdflib)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonld)

# Read the ontology
file_path <- "/Users/mac/Documents/Disease.owl"
rdf <- rdf_parse(file_path)

# Query for classes and subclasses
query_classes <- '
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?class ?subClass WHERE {
?subClass rdfs:subClassOf ?class .
}'

results_classes <- rdf_query(rdf, query_classes)

# Convert the class-subclass results to a data frame
results_classes_df <- as_tibble(results_classes)
colnames(results_classes_df) <- c("class", "subClass")

# Extract labels from URIs (assuming URIs are in the format http://...#label)
results_classes_df <- results_classes_df %>%
  mutate(class_label = sub(".+#", "", as.character(class)),
         subClass_label = sub(".+#", "", as.character(subClass)))

# List all unique classes
unique_classes <- unique(results_classes_df$class_label)
print("Available classes in the ontology:")
print(unique_classes)

# Function to find children of a specific class
find_children <- function(df, class_name) {
  children <- df %>%
    filter(class_label == class_name) %>%
    pull(subClass_label)
  return(children)
}

# Print children for all classes
for (class_name in unique_classes) {
  children <- find_children(results_classes_df, class_name)
  print(paste("Children of class", class_name, ":"))
  if (length(children) > 0) {
    print(children)
  } else {
    print("No children found.")
  }
  cat("\n")
}

# Calculate ratio of each class compared to others
class_counts <- results_classes_df %>%
  count(class_label) %>%
  mutate(ratio = n / sum(n))

print("Ratio of each class compared to others:")
print(class_counts)
