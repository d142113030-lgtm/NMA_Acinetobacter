# Install required R packages:
install.packages(c("gemtc", "rjags", "coda", "readxl"))
library(readxl)
library(gemtc)
library(rjags)
library(coda)

# Load data
mortality <- read_xlsx("../data/Mortality_Final.xlsx")
# Variable Definitions
study <- as.factor(mortality$Study)
responders <- as.numeric(mortality$Outcomes)
sampleSize <- as.numeric(mortality$N)
treatment <- as.factor(mortality$Treat)
data_b_bin <- data.frame(study, responders, sampleSize, treatment)
# Network
network <- mtc.network(data.ab = data_b_bin, description = "Network")
# Define the model
model <- mtc.model( network = network, type = "consistency", factor = 2.5, n.chain = 4, linearModel = "random", dic = TRUE)
# Run model
result <- mtc.run(model, sampler = NA, n.adapt = 50000, n.iter = 20000, thin = 5) summary(result)
# Plot
png("../results/trace_plot.png", width = 1000, height = 800)
plot(result)
dev.off()
# BGR statistic
gelman.diag(result)
png("../results/gelman_plot.png", width = 1000, height = 800)
gelman.plot(result)
dev.off()
#-----------------------------------------
# Relative effect
tbl <- relative.effect(result, t1 = "1", preserve.extra = FALSE, covariate = NA)
summary(tbl)
#-----------------------------------------
# Ranking
ranks <- rank.probability(result, preferredDirection = 1, covariate = NA)
print(ranks)
sucra_values <- sucra(ranks)
print(sucra_values)
#-----------------------------------------
# Nodesplit
mtc.nodesplit.comparisons(network)
inconsistency_result <- mtc.nodesplit(network, comparisons = mtc.nodesplit.comparisons(network))
summary(inconsistency_result)
plot(summary(inconsistency_result))
#-----------------------------------------
# Inconsistency model
model2 <- mtc.model(network = network,
                    type = "ume",
                    factor = 2.5,
                    n.chain = 4,
                    linearModel = "random", dic = TRUE)
# Run model
result2 <- mtc.run(model2, sampler = NA, n.adapt = 50000, n.iter = 20000, thin = 5)
summary(result2)
# BGR statistic
gelman.diag(result2)
png("gelman_plot2.png", width = 1000, height = 800)
gelman.plot(result)
dev.off()
#-----------------------------------------
#Thank you




