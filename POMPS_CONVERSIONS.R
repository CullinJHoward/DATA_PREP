
##### TIME-VARYING POMPS ######

# THIS COMPUTES THE MIN AND MAX FROM THE OBSERVED DATA AND USES THOSE (INSTEAD OF MEASURE MIN/MAX'S)

# USED WHEN THERE ARE MULTIPLE WAVES OF THE SAME VARIABLE SO THE MIN AND MAX NEED TO BE 
# RELATIVE TO THE MIN OBSERVED OR MAX OBSERVED SCORE LONGITUDINALLY. 
# THIS WILL REMOVE THE FINAL NUMBER (IT IS TREATED AS A WAVE CLASSIFIER) AND THEN MAKES
# GROUPS OF VARIABLES BASED ON THE REMAINING SUFFIX'S. CAN GO ACROSS A RANGE OF COLUMNS AND 
# IT WILL DYNAMICALLY IDENTIFY SAME VARIABLES (SO LONG AS YOU FOLLOW THE WAVE AS LAST CHARACTER PROCESS)


LONG_POMPS <- function(data, var_range) {
  # Extracting the variable names based on the given range
  var_names <- names(data)[var_range]
  
  # Identifying variables that belong to the same measure group
  var_groups <- split(var_names, sapply(var_names, function(x) substr(x, 1, nchar(x) - 1)))
  
  # Print identified groups
  cat("Identified groups for longitudinal POMPS values:\n")
  print(var_groups)
  
  for (var_group in var_groups) {
    # Converting variables to numeric if they are not already
    data[, var_group] <- lapply(data[, var_group], as.numeric)
    
    # Calculating maximum and minimum values across all variables in the group for all participants
    group_max_vals <- apply(data[, var_group], 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))
    group_min_vals <- apply(data[, var_group], 1, function(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
    
    # Creating new variables with normalized values multiplied by 100
    new_var_names <- paste0("p", var_group)
    
    for (i in seq_along(var_group)) {
      data[[new_var_names[i]]] <- ifelse(
        is.na(data[[var_group[i]]]), 
        NA, 
        ((data[[var_group[i]]] - min(group_min_vals, na.rm = TRUE)) / 
           (max(group_max_vals, na.rm = TRUE) - min(group_min_vals, na.rm = TRUE))) * 100
      )
    }
  }
  
  return(data)
}

## APPLIED LONG-POMPS
names(df)

df_LONGPOMPS <- LONG_POMPS(df, var_range = c(49:52,   #P-FACTOR 
                                             57:58,   # SOCIAL ACCEPTANCE
                                             61:64    #EMPATHIC CONCERN AND PERSPECTIVE TAKING 
))

################################################################################
#######################  RECENTERED POMPS SCORES  

## CREATES POMPS SCORES, BUT IT CENTERS THE FINAL POMPS SCORE AROUND THE VALUE SPECIFIED
## IMPORTANTLY, THE VALUE SPECIFIED IS ON THE ORINGAL SCALE (NOT THE POMPS SCALE). 
## THIS IS USEFUL WHEN A CERTAIN VALUE IS MEANINGFUL AND NEEDS TO BE RETAINED AS 0. 
## FOR INSTANCE, THE MEAN OR 0 IN A RESIDUALIZED CHANGE SCORE. 

## AGAIN, THIS USES THE OBSERVED MIN AND MAX, NOT THE SCALES POSSIBLE MIN/MAX

RECENTERED_POMPS <- function(data, var_name, center_value) {
  # Check if the variable exists in the data
  if (!(var_name %in% names(data))) {
    stop("Variable not found in the dataset.")
  }
  
  # Convert the variable to numeric if it's not already
  data[[var_name]] <- as.numeric(data[[var_name]])
  
  # Calculate the maximum and minimum values for the variable
  max_val <- max(data[[var_name]], na.rm = TRUE)
  min_val <- min(data[[var_name]], na.rm = TRUE)
  
  # Calculate the POMPS score for each observation and multiply by 100
  poms_score <- ifelse(
    is.na(data[[var_name]]), 
    NA, 
    ((data[[var_name]] - min_val) / (max_val - min_val)) * 100
  )
  
  # Compute the POMPS value at the specified center_value
  poms_at_center <- ((center_value - min_val) / (max_val - min_val)) * 100
  
  # Adjust the POMPS scores by centering on the specified value
  adjusted_poms_score <- poms_score - poms_at_center
  
  # Add the adjusted POMPS variable to the dataset with a prefix "p"
  new_var_name <- paste0("C_p", var_name)
  data[[new_var_name]] <- adjusted_poms_score
  
  # Return the modified dataset
  return(data)
}

## APPLIED RECENTERED POMPS 

names(df_LONGPOMPS)

df_LONGPOMPS_RESPOMP <- RECENTERED_POMPS(df_LONGPOMPS, var_name = "Pfac_PLC", center_value = 0) # PARENT P-FACTOR



################################################################################
####################### POMPS FOR A SINGLE TIMEPOINT - NOT CENTERED 

# Modified RECENTERED_POMPS function to calculate relative POMP scores without centering
INVARIANT_POMPS <- function(data, var_name) {
  # Check if the variable exists in the data
  if (!(var_name %in% names(data))) {
    stop("Variable not found in the dataset.")
  }
  
  # Convert the variable to numeric if it's not already
  data[[var_name]] <- as.numeric(data[[var_name]])
  
  # Calculate the maximum and minimum values for the variable
  max_val <- max(data[[var_name]], na.rm = TRUE)
  min_val <- min(data[[var_name]], na.rm = TRUE)
  
  # Calculate the relative POMP score for each observation (0 to 100)
  poms_score <- ifelse(
    is.na(data[[var_name]]), 
    NA, 
    ((data[[var_name]] - min_val) / (max_val - min_val)) * 100
  )
  
  # Add the relative POMP score variable to the dataset with a prefix "p"
  new_var_name <- paste0("p", var_name)
  data[[new_var_name]] <- poms_score
  
  # Return the modified dataset
  return(data)
}

### USAGE 
df <- INVARIANT_POMPS(df, "variable")


## CREATE A USER IDENTIFIED RANGE POMPS FUNCTION 

# THIS IS WHEN THERE IS A SET (AND MEANINGFUL) MINIMUM AND MAXIMUM
# BECAUSE I AM SUMMING ACROSS SCALES, I WANT THEM TO HOLD THEIR WEIGHTING, SO I
# AM ENTERING THE SCALE MINIMUM AND MAXIMUM AND THEN COMPUTING ITS PERCENTILE BASED 
# ON THESE VALUES. THIS IS DIFFERENT THAN THE RELATIVE POMPS SCORE I HAVE PREVIOUSLY USED
# WHERE THE BOUNDS ARE THE OBSERVED RANGE OF THE DATA 


## USAGE: DF_POMS <- SINGLE_POMP_SET(df, "CRIM_ACT", min_val = 1, max_val = 20)
# MIN IS THE LOWEST POSSIBLE SCORE, MAX IS THE MAX POSSIBLE (NOT OBSERVED) SCORE

SINGLE_POMP_SET <- function(data, var_name, min_val, max_val) {
  # Check if the variable exists in the data
  if (!(var_name %in% names(data))) {
    stop("Variable not found in the dataset.")
  }
  
  # Convert the variable to numeric if it's not already
  data[[var_name]] <- as.numeric(data[[var_name]])
  
  # Check if min_val and max_val are valid
  if (min_val >= max_val) {
    stop("min_val must be less than max_val.")
  }
  
  # Calculate the POMPS score for each observation using the provided min and max
  poms_score <- ifelse(
    is.na(data[[var_name]]), 
    NA, 
    (((data[[var_name]] - min_val) / (max_val - min_val))) * 100
  )
  
  # Add the POMPS variable to the dataset with a prefix "p"
  new_var_name <- paste0("p", var_name)
  data[[new_var_name]] <- poms_score
  
  # Return the modified dataframe
  return(data)
}
