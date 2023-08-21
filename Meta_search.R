
#===== Information =====#

### File: Meta_search.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

#===== Required packages =====#

library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(ggraph)
library(dplyr)
library(ggraph)
library(litsearchr)

#===== Importing 'golden standard' papers =====#

list.files("../naive_search/")
naive_import <- litsearchr::import_results("../naive_search/")
naive_results <- remove_duplicates(naive_import, field = "title", method = "string_osa")

### Extracting key words from titles and abstracts

keywords_gen<- extract_terms(text = paste(naive_results$title, naive_results$abstract),
                             method = "fakerake",
                             min_freq = 5, #look for words appearing in at least 5 articles
                             min_n = 1, #the minimum length ngram to consider
                             language = "English")

### Extracting key words from papers' keywords

keywords_author <- extract_terms(keywords = naive_results$keywords,
                                 method = "tagged",
                                 min_freq = 5,
                                 min_n = 1,
                                 language = "English")
### Making one list

keywords <- unique(append(keywords_gen, keywords_author))

#===== Finding potential search terms =====#

features_matrix <- create_dfm(elements = paste(naive_results$title, naive_results$abstract),
                            features = keywords)

co_network <- create_network(search_dfm = features_matrix,
                            min_studies = 4, # term has to appear in at least 4 studies
                            min_occ = 2) # term has to appear at least twice

### Checking the strength of terms

strengths <- strength(co_network)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) -> term_strengths

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

#cutoff_fig
#term_strengths

cutoff <- find_cutoff(graph = co_network,
                      method = "cumulative",
                      percent = 0.8,
                      imp_method = "strength") # find strength cutoff above which the nodes capture 80% (default) of total strength in network 

#cutoff_fig +
#  geom_hline(yintercept=cutoff, linetype="dashed")

reduced_co_network <- reduce_graph(graph = co_network, cutoff_strength = cutoff)
suggested_terms <- get_keywords(reduced_co_network)

### Manually adding potential search terms

added_terms <- c(
  "reproductive aging",
  "reproductive quality",
  "reproductive senescence",
  "reproductive trade-offs",
  "reproductive success",
  "reproductive costs",
  "reproductive investment",
  "reproductive stress",
  "parental investment",
  "parental costs",
  "parental quality",
  "parental trade-offs",
  "fecundity",
  "fertility",
  "rearing"
)

suggested_terms <- c(suggested_terms, added_terms)
write.csv(suggested_terms, file = "suggested_terms.csv", row.names = FALSE)

#===== Write Boolean search string =====#

### Manually sorting through potential search terms with concept groups

sorted_terms <- read.csv("suggested_terms.csv", header = TRUE)

grouped_terms <-list(
  telomeres = suggested_terms[c(91,103,104,105,106,107)],
  process = suggested_terms[c(1,6,17,40,48,87,88,89,90)],
  reproduction = suggested_terms[c(27,78,79,80,81,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,55)]
)

### Writing final Boolean search

write_search(
  grouped_terms,
  languages="English",
  exactphrase=TRUE,
  stemming=TRUE,
  closure="none",
  writesearch=TRUE
)

#cat(read_file("search-inEnglish.txt"))

new_results_1 <- import_results(file="extended_search_1.ris")
new_results_2 <- import_results(file="extended_search_2.ris")
new_results_3 <- import_results(file="extended_search_3.ris")

#ncol(new_results_1) # checking whether files have been imported correctly
#ncol(new_results_2)
#ncol(new_results_3)

#colnames(new_results_1)
#colnames(new_results_2)
#colnames(new_results_3)

new_results_2$C6 <- 0
new_results_3$C6 <- 0 # making sure that I can combine two ris files

new_results_2$A1 <- 0
new_results_3$A1 <- 0 # making sure that I can combine two ris files

new_results_2$MA <- 0
new_results_3$MA <- 0 # making sure that I can combine two ris files

new_results_combined_1 <- rbind(new_results_1,new_results_2)
new_results_combined <- rbind(new_results_combined_1,new_results_3)

### Checking whether all of the results of the naive search ('golden standard' papers) are in the final search

naive_results %>%
  mutate(in_new_results_combined=title %in% new_results_combined[, "title"]) ->
  naive_results

naive_results %>%
  filter(!in_new_results_combined) %>%
  select(title, keywords)
