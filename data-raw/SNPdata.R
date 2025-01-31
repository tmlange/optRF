# Code to simulate SNP data
# Simulated data contain wheat yield in g/m^2 which is associated with 5% of the SNPs
# which occur in homozygous form for the major allele (0) or the minor allele (2)

# Define number of observations (n) and SNP number (p)
observations = 250
snps = 5000

set.seed(123)

# Create Phenotype data (wheat yield in g/m^2)
Yield = rnorm(observations, 600, 100)

# Create SNP numbers
nc_1 = round(0.02*snps) # Number of SNPs that are significantly associated with the phenotype
nc_2 = snps-nc_1 # Number of SNPs that are not significantly associated with the phenotype

# Divide the phenotypic data into three parts
P1 = Yield[Yield<quantile(Yield, 0.25)]
P2 = Yield[Yield>=quantile(Yield, 0.25) & Yield<=quantile(Yield, 0.75)]
P3 = Yield[Yield>quantile(Yield, 0.75)]


# Simulate SNP data that are associated with the phenotype
SNPdata = data.frame(Yield = c(P1, P2, P3))
for(i in 1:nc_1){

  # more major alleles for the part where the phenotypic values are small
  prob_min = sample(seq(0.05, 0.25, 0.01), 1)
  prob_maj = 1-prob_min
  tmp_1 = sample(c(0, 2), size = length(P1), replace = TRUE, prob=c(prob_maj, prob_min))

  # random distribution of SNPs in the middle part
  tmp_2 = sample(c(0, 2), size = length(P2), replace = TRUE)

  # more minor alleles for the part where the phenotypic values are big
  prob_min = sample(seq(0.25, 0.5, 0.01), 1)
  prob_maj = 1-prob_min
  tmp_3 = sample(c(0, 2), size = length(P1), replace = TRUE, prob=c(prob_maj, prob_min))

  # combining all information
  SNPdata = cbind(SNPdata, c(tmp_1, tmp_2, tmp_3))
}


# Simulate SNP data that are not associated with the phenotype
for(i in 1:nc_2){
  prob_min = sample(seq(0.05, 0.45, 0.01), 1)
  prob_maj = 1-prob_min
  tmp = sample(c(0, 2), size = observations, replace = TRUE, prob=c(prob_maj, prob_min))

  SNPdata = cbind(SNPdata, tmp)
}

# Finally, mix the rows
SNPdata = SNPdata[sample(c(1:observations)),]

# Give column and row names for the data
names(SNPdata)[2:ncol(SNPdata)] = paste("SNP", sprintf("%04d", c(1:snps)), sep="_")
row.names(SNPdata) = paste0("ID_", sprintf("%03d", c(1:nrow(SNPdata))))

usethis::use_data(SNPdata, overwrite = TRUE)
