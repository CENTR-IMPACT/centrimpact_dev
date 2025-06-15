update_workflow_step <- function(rv_workflow, step, stage, session) {
  # Update the reactive values
  rv_workflow$step <- step
  rv_workflow$stage <- stage
  
  logger::log_info("Updating workflow step: {step}, stage: {stage}")
  
  # Determine which icon to update
  workflow_id <- dplyr::case_when(
    step == "setup" ~ "project_setup_icon",
    step == "load" ~ "load_data_icon",
    step == "clean" ~ "clean_data_icon",
    step == "calculate" ~ "analyze_data_icon",
    step == "visualize" ~ "visualize_data_icon",
    step == "generate" ~ "generate_report_icon"
  )
  
  # Set the color based on the stage
  workflow_color <- dplyr::case_when(
    stage == "init" ~ "#888888",
    stage == "in progress" ~ "#E18B4E",
    stage == "complete" ~ "#4B7F52",
    TRUE ~ "#D3D3D3"
  )
  
  # Send message to update the icon color
  session$sendCustomMessage("highlightWorkflowIcon", 
                          list(icon_id = workflow_id, color = workflow_color))
}
