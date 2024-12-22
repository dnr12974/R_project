# Load necessary libraries
library(ggplot2)
library(dplyr)  # For data manipulation

# Load datasets (assuming paths are correct)
educator_survey <- read.csv("C:/Users/Admin/Desktop/ResearchPaper_extended_dataset (1).csv")
student_survey <- read.csv("C:/Users/Admin/Desktop/indian_students_ai_impact_v3 (1).csv")
# Function to print summary or counts in a readable format
print_summary <- function(summary_data) {
  for (i in 1:length(summary_data)) {
    cat(names(summary_data)[i], ": ", summary_data[i], "\n")
  }
}

# Main menu function
main_menu <- function() {
  cat("\nMain Menu:\n")
  cat("1. Educator Survey Analysis (Bar Plot for each parameter)\n")
  cat("2. Student Survey Analysis (Bar Plot for each parameter)\n")
  cat("3. Educator Survey Analysis (Pie Chart for each parameter)\n")
  cat("4. Student Survey Analysis (Pie Chart for each parameter)\n")
  cat("5. Exit\n")
  choice <- readline(prompt = "Enter your choice: ")
  choice <- as.integer(choice)
  if (is.na(choice) || choice < 1 || choice > 5) {
    cat("Invalid input. Please enter a number between 1 and 5.\n")
    return(main_menu())
  }
  return(choice)
}

# Parameter menu function
parameter_menu <- function(parameters) {
  cat("\nSelect Parameter to Analyze:\n")
  for (i in 1:length(parameters)) {
    cat(i, ". ", parameters[i], "\n", sep = "")
  }
  choice <- readline(prompt = "Enter your choice: ")
  choice <- as.integer(choice)
  if (is.na(choice) || choice < 1 || choice > length(parameters)) {
    cat("Invalid input. Please enter a valid parameter number.\n")
    return(parameter_menu(parameters))
  }
  return(parameters[choice])
}

# Function to analyze educator survey (bar plot)
analyze_educator_survey_bar <- function(parameter) {
  cat("\nAnalyzing Educator Survey for parameter: ", parameter, "\n", sep = "")
  
  # Summary or counts
  summary_data <- table(educator_survey[[parameter]])
  cat("\nSummary for", parameter, "in Educator Survey:\n")
  print_summary(summary_data)  # Print summary
  
  # Bar plot
  if (is.numeric(educator_survey[[parameter]])) {
    p <- ggplot(data = educator_survey, aes(x = educator_survey[[parameter]])) +
      geom_histogram(binwidth = 1, fill = "#F6D173", color = "#7D1007") +
      ggtitle(paste("Histogram of", parameter, "in Educator Survey")) +
      xlab(parameter) +
      ylab("Frequency") +
      theme_minimal()
  } else {
    p <- ggplot(data = educator_survey, aes(x = factor(educator_survey[[parameter]]))) +
      geom_bar(fill = "#F6D173", color = "#7D1007") +
      ggtitle(paste("Bar Plot of", parameter, "in Educator Survey")) +
      xlab(parameter) +
      ylab("Frequency") +
      theme_minimal()
  }
  print(p)
}

# Function to analyze student survey (bar plot)
analyze_student_survey_bar <- function(parameter) {
  cat("\nAnalyzing Student Survey for parameter: ", parameter, "\n", sep = "")
  
  # Summary or counts
  summary_data <- table(student_survey[[parameter]])
  cat("\nSummary for", parameter, "in Student Survey:\n")
  print_summary(summary_data)  # Print summary
  
  # Bar plot
  if (is.numeric(student_survey[[parameter]])) {
    p <- ggplot(data = student_survey, aes(x = student_survey[[parameter]])) +
      geom_histogram(binwidth = 1, fill="#BDE2B9", color = "#000000") +
      ggtitle(paste("Histogram of", parameter, "in Student Survey")) +
      xlab(parameter) +
      ylab("Frequency") +
      theme_minimal()
  } else {
    p <- ggplot(data = student_survey, aes(x = factor(student_survey[[parameter]]))) +
      geom_bar(fill = "#BDE2B9", color = "#000000") +
      ggtitle(paste("Bar Plot of", parameter, "in Student Survey")) +
      xlab(parameter) +
      ylab("Frequency") +
      theme_minimal()
  }
  print(p)
}

# Function to analyze educator survey (pie chart)
analyze_educator_survey_pie <- function(parameter) {
  cat("\nAnalyzing Educator Survey for parameter: ", parameter, "\n", sep = "")
  
  # Summary or counts
  summary_data <- table(educator_survey[[parameter]])
  cat("\nSummary for", parameter, "in Educator Survey:\n")
  print_summary(summary_data)  # Print summary
  
  # Pie chart
  if (!is.numeric(educator_survey[[parameter]])) {
    labels <- as.character(names(summary_data))
    values <- summary_data
    percentages <- round(100 * values / sum(values), 1)
    labels <- paste(labels, "(", percentages, "%)", sep = "")
    
    p <- ggplot(data = NULL, aes(x = "", y = values, fill = labels)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      ggtitle(paste("Pie Chart of", parameter, "in Educator Survey")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = paste0(percentages, "%")), 
                position = position_stack(vjust = 0.5))
    print(p)
  } else {
    cat("Cannot generate pie chart for numeric parameter.\n")
  }
}

# Function to analyze student survey (pie chart)
analyze_student_survey_pie <- function(parameter) {
  cat("\nAnalyzing Student Survey for parameter: ", parameter, "\n", sep = "")
  
  # Summary or counts
  if (is.numeric(student_survey[[parameter]])) {
    # Custom breaks for age intervals
    custom_breaks <- c(18,19,20,21,22,23,24,25)  # Adjust the intervals based on your data
    binned_data <- cut(student_survey[[parameter]], breaks = custom_breaks, include.lowest = TRUE)
    
    summary_data <- table(binned_data)
    labels <- as.character(names(summary_data))
    values <- summary_data
    percentages <- round(100 * values / sum(values), 1)
    labels <- paste(labels, "(", percentages, "%)", sep = "")
    
    p <- ggplot(data = NULL, aes(x = "", y = values, fill = labels)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      ggtitle(paste("Pie Chart of", parameter, "in Student Survey")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = paste0(percentages, "%")), 
                position = position_stack(vjust = 0.5))
    print(p)
  } else {
    summary_data <- table(student_survey[[parameter]])
    cat("\nSummary for", parameter, "in Student Survey:\n")
    print_summary(summary_data)  # Print summary
    
    labels <- as.character(names(summary_data))
    values <- summary_data
    percentages <- round(100 * values / sum(values), 1)
    labels <- paste(labels, "(", percentages, "%)", sep = "")
    
    p <- ggplot(data = NULL, aes(x = "", y = values, fill = labels)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      ggtitle(paste("Pie Chart of", parameter, "in Student Survey")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = paste0(percentages, "%")), 
                position = position_stack(vjust = 0.5))
    print(p)
  }
}

# Main loop function
main_loop <- function() {
  repeat {
    choice <- main_menu()
    if (choice == 5) {
      cat("Exiting the program.\n")
      break
    } else if (choice == 1) {
      parameter <- parameter_menu(names(educator_survey))
      analyze_educator_survey_bar(parameter)
    } else if (choice == 2) {
      parameter <- parameter_menu(names(student_survey))
      analyze_student_survey_bar(parameter)
    } else if (choice == 3) {
      parameter <- parameter_menu(names(educator_survey))
      analyze_educator_survey_pie(parameter)
    } else if (choice == 4) {
      parameter <- parameter_menu(names(student_survey))
      analyze_student_survey_pie(parameter)
    } else {
      cat("Invalid choice. Please try again.\n")
    }
  }
}

# Run the main loop
main_loop()
