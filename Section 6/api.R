library(plumber)

# Load the model
model <- readRDS("model_en_US.rds")

# Define the API endpoint
#* @param female Numeric. Female indicator (0 or 1).
#* @param senior_citizen Numeric. Senior citizen indicator (0 or 1).
#* @param partner Numeric. Partner indicator (0 or 1).
#* @param dependents Numeric. Dependents indicator (0 or 1).
#* @param tenure Numeric. Customer tenure in months.
#* @param phone_service Numeric. Phone service indicator (0 or 1).
#* @param paperless_billing Numeric. Paperless billing indicator (0 or 1).
#* @param monthly_charges Numeric. Monthly charges.
#* @param total_charges Numeric. Total charges.
#* @param multiple_lines Categorical. Options: No, No phone service, Yes.
#* @param internet_service Categorical. Options: DSL, Fiber optic, No.
#* @param online_security Categorical. Options: No, No internet service, Yes.
#* @param online_backup Categorical. Options: No, No internet service, Yes.
#* @param device_protection Categorical. Options: No, No internet service, Yes.
#* @param tech_support Categorical. Options: No, No internet service, Yes.
#* @param streaming_tv Categorical. Options: No, No internet service, Yes.
#* @param streaming_movies Categorical. Options: No, No internet service, Yes.
#* @param contract Categorical. Options: Month-to-month, One year, Two year.
#* @param payment_method Categorical. Options: Bank transfer, Credit card, Electronic check, Mailed check.
#* @post /predict
predict_churn <- function(female, senior_citizen, partner, dependents, tenure, 
                          phone_service, paperless_billing, monthly_charges, 
                          total_charges, multiple_lines, internet_service, 
                          online_security, online_backup, device_protection, 
                          tech_support, streaming_tv, streaming_movies, 
                          contract, payment_method) {
  
  # Create a new data frame for prediction
  new_data <- data.frame(
    female = as.numeric(female),
    senior_citizen = as.numeric(senior_citizen),
    partner = as.numeric(partner),
    dependents = as.numeric(dependents),
    tenure = as.numeric(tenure),
    phone_service = as.numeric(phone_service),
    paperless_billing = as.numeric(paperless_billing),
    monthly_charges = as.numeric(monthly_charges),
    total_charges = as.numeric(total_charges),
    # multiple_lines = factor(multiple_lines, levels = c("No", "No phone service", "Yes")),
    # internet_service = factor(internet_service, levels = c("DSL", "Fiber optic", "No")),
    # online_security = factor(online_security, levels = c("No", "No internet service", "Yes")),
    # online_backup = factor(online_backup, levels = c("No", "No internet service", "Yes")),
    # device_protection = factor(device_protection, levels = c("No", "No internet service", "Yes")),
    # tech_support = factor(tech_support, levels = c("No", "No internet service", "Yes")),
    # streaming_tv = factor(streaming_tv, levels = c("No", "No internet service", "Yes")),
    # streaming_movies = factor(streaming_movies, levels = c("No", "No internet service", "Yes")),
    # contract = factor(contract, levels = c("Month-to-month", "One year", "Two year")),
    # payment_method = factor(payment_method, levels = c("Bank transfer", "Credit card", "Electronic check", "Mailed check"))
    # )

    # Manually create dummy variables using ifelse
    multiple_lines_No.phone.service = ifelse(multiple_lines == "No phone service", 1, 0),
    multiple_lines_No = ifelse(multiple_lines == "No", 1, 0),
    multiple_lines_Yes = ifelse(multiple_lines == "Yes", 1, 0),
    
    internet_service_DSL = ifelse(internet_service == "DSL", 1, 0),
    internet_service_Fiber.optic = ifelse(internet_service == "Fiber optic", 1, 0),
    internet_service_No = ifelse(internet_service == "No", 1, 0),
    
    online_security_No = ifelse(online_security == "No", 1, 0),
    online_security_No.internet.service = ifelse(online_security == "No internet service", 1, 0),
    online_security_Yes = ifelse(online_security == "Yes", 1, 0),
    
    online_backup_No = ifelse(online_backup == "No", 1, 0),
    online_backup_No.internet.service = ifelse(online_backup == "No internet service", 1, 0),
    online_backup_Yes = ifelse(online_backup == "Yes", 1, 0),
    
    device_protection_No = ifelse(device_protection == "No", 1, 0),
    device_protection_No.internet.service = ifelse(device_protection == "No internet service", 1, 0),
    device_protection_Yes = ifelse(device_protection == "Yes", 1, 0),
    
    tech_support_No = ifelse(tech_support == "No", 1, 0),
    tech_support_No.internet.service = ifelse(tech_support == "No internet service", 1, 0),
    tech_support_Yes = ifelse(tech_support == "Yes", 1, 0),
    
    streaming_tv_No = ifelse(streaming_tv == "No", 1, 0),
    streaming_tv_No.internet.service = ifelse(streaming_tv == "No internet service", 1, 0),
    streaming_tv_Yes = ifelse(streaming_tv == "Yes", 1, 0),
    
    streaming_movies_No = ifelse(streaming_movies == "No", 1, 0),
    streaming_movies_No.internet.service = ifelse(streaming_movies == "No internet service", 1, 0),
    streaming_movies_Yes = ifelse(streaming_movies == "Yes", 1, 0),
    
    contract_Month.to.month = ifelse(contract == "Month-to-month", 1, 0),
    contract_One.year = ifelse(contract == "One year", 1, 0),
    contract_Two.year = ifelse(contract == "Two year", 1, 0),
    
    payment_method_Bank.transfer..automatic. = ifelse(payment_method == "Bank transfer", 1, 0),
    payment_method_Credit.card..automatic. = ifelse(payment_method == "Credit card", 1, 0),
    payment_method_Electronic.check = ifelse(payment_method == "Electronic check", 1, 0),
    payment_method_Mailed.check = ifelse(payment_method == "Mailed check", 1, 0)
  )
  
  # Make prediction using the loaded model
  prediction <- predict(model, new_data)
  
  # Return the prediction result
  return(list(churn = ifelse(prediction == 1, "Yes", "No")))
}