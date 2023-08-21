
#===== Information =====#

### File: Bayesian_analysis.R
### Author: Agnes Szwarczynska
### Date: 27/06/2023
### Project: Master's thesis

#===== Required packages =====#

library(readr)
library(brms)
library(ggplot2)
library(tidybayes)
library(dplyr)
library(gridExtra)
library(ggridges)
library(glue)
library(stringr)
library(forcats)
library(bayesmeta)
library(rotl)
library(ape)
library(stringr)
library(ggtree)
library(metafor)

#===== Loading data =====# 

#data_for_B3 <- read_csv("data_for_B3.csv")

library(readxl)
data_for_B3 <- read_excel("data_for_B3.xlsx")
telo <- data_for_B3
#View(data_for_B3)

#===== Preparing data =====#

telo <- as.data.frame(data_for_B3)
telo$Author <- as.character(telo$Author)
telo$Author <- as.factor(telo$Author)

#hist(data_for_B2$z_score) ### checking the distribution of the z-scores

priors_main <- c(set_prior("normal(0,0.6)", class = "Intercept", lb = -1, ub = 1),
            set_prior("cauchy(0,0.5)", class = "sd")) ### setting up priors

priors <- c(set_prior("normal(0,0.6)", class = "Intercept", lb = -1, ub = 1),
            set_prior("normal(0,0.6)", class = "b"),
            set_prior("cauchy(0,0.5)", class = "sd")) ### setting up priors

#===== Bayesian models =====#

### Main model

m.brm_3 <- brm(Zr|se(SE_Zr) ~ 1 + (1|Author),
             family = gaussian(link = "identity"),
             data = telo,
             prior = priors_main,
             init = 0,
             iter = 40000,
             warmup = 1000,
             seed=14)

sum_m.brm_3 <- summary(m.brm_3)

### Model with all moderators

m.brm_11 <- brm(Zr|se(SE_Zr) ~ 1 + Sex + TL_period_reproduction + Method + Parental_care + SD_system + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               init = 0,
               iter = 40000,
               warmup = 1000,
               seed=14)

sum_m.brm_11 <- summary(m.brm_11)

### Model with "Sex" moderator

m.brm_2 <- brm(Zr|se(SE_Zr) ~ 1 + Sex + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               init = 0,
               iter = 40000,
               warmup = 1000,
               seed=14)

sum_m.brm_2 <- summary(m.brm_2)

### Model with "Class" moderator

m.brm_16 <- brm(Zr|se(SE_Zr) ~ 1 + Class + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               init = 0,
               iter = 10000,
               warmup = 1000,
               seed=14)

### Model with "Method" moderator

m.brm <- brm(z_score|se(SE_z_score) ~ 1 + Method + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               iter = 4000,
               warmup = 1000,
               seed=14)

### Model with "Tissue" moderator

m.brm_4 <- brm(z_score|se(SE_z_score) ~ 1 + Tissue + (1|Author),
             family = gaussian(link = "identity"),
             data = telo,
             prior = priors,
             iter = 4000,
             warmup = 1000,
             seed=14)

### Model with "Parental care" moderator

m.brm_5 <- brm(z_score|se(SE_z_score) ~ 1 + Parental_care + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               iter = 4000,
               warmup = 1000,
               seed=14)

### Model with "TL measurement period" moderator

m.brm_6 <- brm(z_score|se(SE_z_score) ~ 1 + TL_period_reproduction + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               iter = 4000,
               warmup = 1000,
               seed=14)

### Model with "Maximum lifespan" moderator

m.brm_7 <- brm(z_score|se(SE_z_score) ~ 1 + Maximum_lifespan + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               iter = 4000,
               warmup = 1000,
               seed=14)

### Model with "SD_system" moderator

m.brm_20 <- brm(Zr|se(SE_Zr) ~ 1 + SD_system + (1|Author),
               family = gaussian(link = "identity"),
               data = telo,
               prior = priors,
               iter = 40000,
               warmup = 1000,
               seed=14)

### Diagnostic plots

mcmc_plot(m.brm_3, type = "acf") ### autocorrelation 
mcmc_plot(m.brm_3, type = "neff") ### effective sample size
plot(m.brm_3)

### Posterior density plots

post.samples <- posterior_samples(m.brm_3, c("^b", "^sd"))
names(post.samples) <- c("Zr", "tau")

Zr_plot <-  ggplot(aes(x = Zr), data = post.samples) +
  geom_density(fill = "lightblue",                # set the color
               color = "lightblue", alpha = 0.7) +  
  geom_point(y = 0,                               # add point at mean
             x = mean(post.samples$Zr)) +
  labs(x = expression(italic(Zr)),
       y = element_blank()) +
  theme_minimal()

tau_plot <- ggplot(aes(x = tau), data = post.samples) +
  geom_density(fill = "lightgreen",               # set the color
               color = "lightgreen", alpha = 0.7) +  
  geom_point(y = 0, 
             x = mean(post.samples$tau)) +        # add point at mean
  labs(x = expression(tau),
       y = element_blank()) +
  theme_minimal()

grid.arrange(Zr_plot, tau_plot, ncol = 2) #arranging plots to be displayed simultaneously in one window

### Forest plot

study.draws <- spread_draws(m.brm_3, r_Author[Author,], b_Intercept) %>% 
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(m.brm_3, b_Intercept) %>% 
  mutate(Author = "Pooled Effect")

forest.data <- bind_rows(study.draws, 
                         pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)


ggplot(aes(b_Intercept, 
           relevel(Author, "Pooled Effect", 
                   after = Inf)), 
       data = forest.data) + 
  
  geom_vline(xintercept = fixef(m.brm_3)[1, 1], 
             color = "#00FFFF", size = 1) +
  geom_vline(xintercept = fixef(m.brm_3)[1, 3:4], 
             color = "black", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             linewidth = 1) +
  
  geom_pointinterval(data = forest.data.summary, 
                      linewidth = 1, 
                     aes(xmin = .lower, xmax = .upper) ) +
  
  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  
  labs(x = "Standardized correlation coefficient", # summary measure
       y = element_blank()) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, color = "black")) +
  xlim(-0.8, 0.8)

### Funnel plot 

all_authors <- as.data.frame(ranef(m.brm_3)) 
#extracting estimates with 95% credible intervals per study

#nrow(all_authors)
par(mar = c(5, 5, 2, 2))

funnel_plot <- funnel(x=all_authors$Author.Estimate.Intercept,
       sei=all_authors$Author.Est.Error.Intercept,
       ylab = "Standard error", 
       xlab = "Zr", cex = 1.2,
       back = "#E5E4E2")
par(cex.axis = 1.5, cex.lab = 2)

save_plot("Funnel_plot_demo.svg", fig = funnel_plot, width=30, height=24)


### Phylogenetic tree

names_species <- tnrs_match_names(data_for_B3$Species) ### extracting species names
phylo_tree <- tol_induced_subtree(ott_ids = names_species$ott_id, label_format = "name")
plot(phylo_tree, no.margin = TRUE)
save_plot("Phylo_tree.svg", fig = phylo_tree, width=30, height=24)

### Model with a phylogenetic tree as a moderator

phylo_tree_brl <- compute.brlen(phylo_tree) # computing branch lengths of a tree
phylo_matrix <- vcv(phylo_tree_brl, corr = TRUE) # building distance matrix
telo$obs <- 1:nrow(telo) # calculating number of data points to incorporate the info about residual variance

### In my final analysis I decided not to progress with these priors and do not adjust the max_treedepth parameter

priors_8 <- c(set_prior("uniform(-1,1)", class = "Intercept", lb= -1, ub=1),
            set_prior("cauchy(0,0.5)", class = "sd", coef = "Intercept", group = "Author"),
            set_prior("normal(0.15,2)", class = "sd", coef = "Intercept", group = "Species_2")) ### setting up priors

m.brm_8 <- brm(Zr|se(SE_Zr) ~ 1 + (1|Author) + 
                 (1|gr(Species_2, cov = phylo_matrix)) + (1|obs),
               family = gaussian(link = "identity"),
               data = telo,
               data2 = list(phylo_matrix = phylo_matrix),
               prior = priors_main,
               control = list(adapt_delta = 0.99),
                              #max_treedepth = 15),
               init = 0,
               iter = 40000,
               warmup = 1000,
               seed = 14,
               thin = 1)

#===== Analyzing class Aves =====#

# In the final analysis I also did not include this model

Aves_data <- subset(telo, Class == "Aves")

m.brm_12 <- brm(Zr|se(SE_Zr) ~ 1 + (1|Author),
                family = gaussian(link = "identity"),
                data = Aves_data,
                prior = priors,
                init = 0,
                iter = 40000,
                warmup = 2000,
                seed=14)

names_species_birds <- tnrs_match_names(Aves_data$Species) ### extracting species names
phylo_tree_birds <- tol_induced_subtree(ott_ids = names_species_birds$ott_id, label_format = "name")
plot(phylo_tree_birds, no.margin = TRUE)

phylo_tree_brl_birds <- compute.brlen(phylo_tree_birds) # computing branch lengths of a tree
phylo_matrix_birds <- vcv(phylo_tree_brl_birds, corr = TRUE) # building distance matrix

Aves_data$obs_birds <- 1:nrow(Aves_data)

m.brm_birds <- brm(z_score|se(SE_z_score) ~ 1 + (1|Author) + 
                 (1|gr(Species_2, cov = phylo_matrix_birds)) + (1|obs),
               family = gaussian(link = "identity"),
               data = Aves_data,
               data2 = list(phylo_matrix_birds = phylo_matrix_birds),
               prior = priors,
               control = list(adapt_delta = 0.95),
               iter = 4000,
               warmup = 1000,
               seed=14)

mcmc_plot(m.brm_birds, type = "acf") ### autocorrelation 
mcmc_plot(m.brm_birds, type = "neff") ### effective sample size
plot(m.brm_birds)

#===== Shuffling =====#

telo <- data_for_B3 

set.seed(167)

unique_study_ids <- unique(telo$Study_ID)
shuffled_species <- sample(unique(telo$Species))
repeated_species <- rep(shuffled_species, length.out = length(unique_study_ids))

species_mapping <- data.frame(Study_ID = unique_study_ids,
                              Shuffled_Species = repeated_species)

shuffled_dataset <- telo %>%
  left_join(species_mapping, by = "Study_ID")

shuffled_dataset <- shuffled_dataset %>%
  mutate(Shuffled_Species2 = str_replace_all(Shuffled_Species, " ", "_"))

shuffled_dataset$obs <- 1:nrow(shuffled_dataset)

m.brm_shuffled <- brm(Zr|se(SE_Zr) ~ 1 + (1|Author) + 
                 (1|gr(Shuffled_Species2, cov = phylo_matrix)) + (1|obs),
               family = gaussian(link = "identity"),
               data = shuffled_dataset,
               data2 = list(phylo_matrix = phylo_matrix),
               prior = priors_main,
               control = list(adapt_delta = 0.95),
               iter = 40000,
               warmup = 1000,
               seed=14)


#summary(m.brm_shuffled)

### Excluding Atlantic silverside and a leatherback silver turtle

reduced_dataset <- subset(telo, Study_ID != 31 & Study_ID != 7)

names_species_rd <- tnrs_match_names(reduced_dataset$Species) ### extracting species names
phylo_tree_rd <- tol_induced_subtree(ott_ids = names_species_rd$ott_id, label_format = "name")
plot(phylo_tree_rd, no.margin = TRUE)


phylo_tree_rd <- compute.brlen(phylo_tree_rd) # computing branch lengths of a tree
phylo_matrix_rd <- vcv(phylo_tree_rd, corr = TRUE) # building distance matrix
reduced_dataset$obs <- 1:nrow(reduced_dataset) 

m.brm_reduced <- brm(Zr|se(SE_Zr) ~ 1 + (1|Author) + 
                        (1|gr(Species_2, cov = phylo_matrix_rd)) + (1|obs),
                      family = gaussian(link = "identity"),
                      data = reduced_dataset,
                      data2 = list(phylo_matrix_rd = phylo_matrix_rd),
                      prior = priors_main,
                      control = list(adapt_delta = 0.99),
                      iter = 40000,
                      warmup = 1000,
                      seed=14)


#===== Combining a phylogenetic tree with distribution of Zr estimates =====#

m.brm_tree <- brm(Zr|se(SE_Zr) ~ 1 + Species + (1|Author),
                      family = gaussian(link = "identity"),
                      data = telo,
                      prior = priors,
                      control = list(adapt_delta = 0.95),
                      init = 0,
                      iter = 40000,
                      warmup = 1000,
                      seed=14)

telomeres <- fixef(m.brm_tree) # obtaining estimates of fixed effects (species)
telomeres_est <- telomeres[1:26]

ci_lower <- as.data.frame(summary(m.brm_tree)$fixed)[,"l-95% CI"] # obtaining CIs of fixed effects (species)
ci_upper <- as.data.frame(summary(m.brm_tree)$fixed)[,"u-95% CI"]

### Formatting the phylogenetic tree

common_names <- unique(select(telo,c(Species,Common_Name)))
common_names$new_tip_labels <- paste(common_names$Common_Name, " (", common_names$Species, ")", sep = "")
phylo_for_gradient <- phylo_tree_brl
phylo_for_gradient$tip.label <- common_names$new_tip_labels[match(gsub("_", " ",phylo_tree_brl$tip.label), common_names$Species)]
#plot(phylo_for_gradient)

### Creating a data frame for plotting

common_names <- common_names[order(common_names$Species),]
species_est <- data.frame(common_names$new_tip_labels,telomeres_est,ci_lower,ci_upper, row.names = NULL)

species_est$common_names.new_tip_labels <- factor(species_est$common_names.new_tip_labels,
                                        levels = c("Columbian ground squirrel (Urocitellus columbianus)",
                                                   "Eastern chipmunk (Tamias striatus)",
                                                   "Human (Homo sapiens)",
                                                   "Meerkat (Suricata suricatta)",
                                                   "Pied flycatcher (Ficedula hypoleuca)",
                                                   "European starling (Sturnus vulgaris)",
                                                   "White-browed sparrow-weaver (Plocepasser mahali)",
                                                   "Dark-eyed junco (Junco hyemalis)",
                                                   "House sparrow (Passer domesticus)",
                                                   "Tree swallow (Tachycineta bicolor)",
                                                   "Barn swallow (Hirundo rustica)",
                                                   "Seychelles warbler (Acrocephalus sechellensis)",
                                                   "Great tit (Parus major)",
                                                   "Eurasian blue tit (Cyanistes caeruleus)",
                                                   "Purple-crowned fairywren (Malurus coronatus)",
                                                   "Common tern (Sterna hirundo)",
                                                   "Thick-billed murre (Uria lomvia)",
                                                   "Cory's shearwater (Calonectris borealis)",
                                                   "Black-browed albatross (Thalassarche melanophrys)",
                                                   "Magellanic penguin (Spheniscus megellanicus)",
                                                   "King penguin (Aptenodytes patagonicus)",
                                                   "Common eider (Somateria mollissima)",
                                                   "Leatherback sea turtle (Dermochelys coriacea)",
                                                   "Sand lizard (Lacerta agilis)",
                                                   "Common lizard (Zootoca vivipara)",
                                                   "Atlantic silverside (Menidia menidia)"))

### Phylogenetic tree with a gradient

tree <- ggtree(phylo_for_gradient, ladderize = FALSE, aes(color = telomeres_est), size = 1.5) %<+% species_est +
  scale_color_continuous(low="#00008B", high= "#00FFFF") + 
  geom_tiplab(as_ylab=TRUE) +
  theme(legend.position="bottom",
        axis.text.y = element_text(size = 12, colour = "black")) +
  labs(color = "Zr\n")
#tree

#===== Re-running models with the metafor package =====#

telo_metafor <- escalc(measure = "ZCOR", ri = Zr, ni = Individuals, data = telo)
telo_metafor$obs <- 1:nrow(telo_metafor)
telo_shuffled <- escalc(measure = "ZCOR", ri = Zr, ni = Individuals, data = shuffled_dataset)
telo_shuffled$obs <- 1:nrow(telo_shuffled)

res_1 <- rma.mv(yi,vi, 
                 random = list(~ 1|Author, ~ 1|Species_2, ~ 1|obs),
                 R = list(Species_2 = phylo_matrix),
                 data = telo_metafor)


res_2 <- rma.mv(yi,vi, 
                 random = list(~ 1|Author, ~ 1|Shuffled_Species2, ~ 1|obs),
                 R = list(Shuffled_Species2 = phylo_matrix),
                 data = telo_shuffled)


res_3 <- rma.mv(yi,vi, 
                 random = list(~ 1|Author), #'intercept-only' model
                 data = telo_metafor)

#consider nested structure in random argument ~ 1|Author/ obs


res_4 <- rma.mv(yi ~ Sex + TL_measurement_period + TL_measurement_method + 
                  Parental_care + SD_system + Tissue_type,
                vi, 
                 random = list(~ 1|Author), #all moderators
                 data = telo_metafor)


res_5 <- rma.mv(yi~ Sex,vi, 
                 random = list(~ 1|Author), #signature of sex
                 data = telo_metafor)

