library("psych")
library("ez")
library("readr")
library("reghelper")
library("sjPlot")
library("lme4")
library("dplyr")
library("ggplot2")

# ==============================================================================
# REPEATED MEASURES ANOVA WITH CO2 DATA
# ==============================================================================

# Load built-in CO2 dataset (CO2 uptake in plants)
data("CO2")

# Examine data structure
str(CO2)

# Convert concentration to factor for repeated measures ANOVA
CO2$conc_factor <- as.factor(CO2$conc)

# Repeated measures ANOVA: uptake by concentration within plants
rm_anova <- ezANOVA(
  data = CO2,
  dv = uptake,         
  wid = Plant,         
  within = conc_factor
)
print(rm_anova)

# ==============================================================================
# LINEAR REGRESSION AND MODERATION ANALYSIS
# ==============================================================================

# Import movies dataset
movies <- read_csv2("movies.csv")

# Simple linear regression: Gross revenue predicted by Budget
model <- lm(Gross ~ Budget, data = movies)
summary(model)

# Diagnostic plots for assumptions
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Test normality of residuals
res <- residuals(model)
shapiro.test(res)

# Center predictors for moderation analysis
movies$Budget_c <- scale(movies$Budget, center = TRUE, scale = FALSE)[,1]
movies$Duration_c <- scale(movies$Duration, center = TRUE, scale = FALSE)[,1]

# Moderation model: Duration moderates Budget-Gross relationship
model_moderation <- lm(Gross ~ Budget_c * Duration_c, data = movies)
summary(model_moderation)

# Simple slopes analysis
simple_slopes(model_moderation)

# Visualize interaction
graph_model(model_moderation, 
            type = "pred", 
            terms = c("Budget_c", "Duration_c"))

# ==============================================================================
# MEDIATION ANALYSIS
# ==============================================================================

# Path c: Total effect
model_total <- lm(Gross ~ Budget, data = movies)

# Path a: X → M
model_critic <- lm(Critic_score ~ Budget, data = movies)

# Paths b and c': Direct and indirect effects
model_gross <- lm(Gross ~ Budget + Critic_score, data = movies)

# Extract coefficients
c_total <- coef(model_total)["Budget"]
a_path <- coef(model_critic)["Budget"]
b_path <- coef(model_gross)["Critic_score"]
c_prime <- coef(model_gross)["Budget"]

# Indirect effect
indirect_effect <- a_path * b_path

cat("\nMediation Analysis Results:\n")
cat("Total effect (c):", round(c_total, 3), "\n")
cat("Direct effect (c'):", round(c_prime, 3), "\n")
cat("Indirect effect (a*b):", round(indirect_effect, 3), "\n")

# ==============================================================================
# MULTILEVEL MODELING
# ==============================================================================

# Convert concentration back to numeric
CO2$conc_numeric <- as.numeric(as.character(CO2$conc))

# Random intercept model
model_ri <- lmer(uptake ~ conc_numeric + Type + (1 | Plant), data = CO2)
summary(model_ri)

# Add cross-level interaction
model_int <- lmer(uptake ~ conc_numeric * Treatment + Type + (1 | Plant), data = CO2)
summary(model_int)

# Visualize interaction
plot_model(model_int, type = "int", terms = c("conc_numeric", "Treatment"))

# Random slopes model
model_rs <- lmer(uptake ~ conc_numeric + Treatment + Type + (Treatment | Plant), 
                 data = CO2)
summary(model_rs)

# Model comparison
anova(model_ri, model_rs)

# ==============================================================================
# PRINCIPAL COMPONENT ANALYSIS
# ==============================================================================

# Import social life dataset
social_life <- read_csv2("social_life.csv")

# Select sociability variables
soc_vars <- social_life %>%
  select(starts_with("SSR_") | starts_with("AG_"))

# Parallel analysis for optimal components
set.seed(123)
parallel <- fa.parallel(soc_vars, fa = "pc", n.iter = 1000)

# PCA with suggested components
pca_result <- principal(soc_vars, 
                        nfactors = parallel$nfact, 
                        rotate = "oblimin", 
                        scores = TRUE)
print(pca_result)

# Internal consistency
alpha_result <- alpha(soc_vars)
print(alpha_result$total)

# Extract component scores
social_life$PC1 <- pca_result$scores[,1]
social_life$PC2 <- pca_result$scores[,2]

# Correlate with age
cor_matrix <- cor(social_life[,c("PC1", "PC2", "age")], 
                  use = "complete.obs")
print(round(cor_matrix, 3))