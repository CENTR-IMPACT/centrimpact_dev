update_workflow_step <- function(rv_workflow, step, stage, session) {
  # Update the reactive values
  rv_workflow$step <- step
  rv_workflow$stage <- stage
  
  # Set the workflow_step to the step name for display
  rv_workflow$workflow_step <- step
  
  logger::log_info("Updating workflow step: {step}, stage: {stage}")
  
  # Determine which icon to update
  workflow_id <- dplyr::case_when(
    step == "Project Setup" ~ "project_setup_icon",
    step == "Upload Data" ~ "load_data_icon",
    step == "Clean Data" ~ "clean_data_icon",
    step == "Calculate Scores" ~ "analyze_data_icon",
    step == "Visualize Findings" ~ "visualize_data_icon",
    step == "Generate Report" ~ "generate_report_icon"
  )
  
  # Set the color based on the stage
  workflow_color <- dplyr::case_when(
    stage == "initiated" ~ "#C2A14D",
    stage == "in progress" ~ "#E18B4E",
    stage == "complete" ~ "#8ba086",
    TRUE ~ "#D3D3D3"
  )
  
  # Send message to update the icon color
  session$sendCustomMessage("highlightWorkflowIcon", 
                          list(icon_id = workflow_id, color = workflow_color))
}
