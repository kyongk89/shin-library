# Fama-French Regression

# Download Data from Kenneth R. French - Data Library
"https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"

# Save and Load downloaded CSV file into R
Fama_French_Data <- read.table("Fama_French.csv", header=TRUE, sep=",")

# Extract Fama-French Factors and Fund Returns
rmrf <- Fama_French_Data[,2] # Excess Return on the Market Protfolio (Index)
smb <- Fama_French_Data[,3]  # Size Premium
hml <- Fama_French_Data[,4]  # Value Premium
rf <- Fama_French_Data[,5]   # Risk Free Rate
fund <- Fama_French_Data[,6] # Firm

# Calculate Excess Returns for Target Fund
Fund_Excess_R <- fund - rf

# Run Fama-French Regression
Fama_French_Regression <- lm(Fund_Excess_R ~ rmrf + smb + hml)

# Print Summary of Regression Results
print(summary(Fama_French_Regression))