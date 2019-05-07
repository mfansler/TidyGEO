#1  
output$nav_1_ui <- renderUI({
  div(
    tertiary_button('nav_clinical_to_choose_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_1_to_2_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_clinical_to_choose_button, {
  updateTabsetPanel(session, 'top_level', selected = "Choose dataset")
})
observeEvent(input$nav_1_to_2_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '2')
})
#2  
output$nav_2_ui <- renderUI({
  div(
    tertiary_button('nav_2_to_1_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_2_to_3_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_2_to_1_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '1')
})
observeEvent(input$nav_2_to_3_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '3')
}) 
#3  
output$nav_3_ui <- renderUI({
  div(
    tertiary_button('nav_3_to_2_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_3_to_4_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_3_to_2_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '2')
})
observeEvent(input$nav_3_to_4_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '4')
})
#4  
output$nav_4_ui <- renderUI({
  div(
    tertiary_button('nav_4_to_3_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_4_to_5_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_4_to_3_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '3')
})
observeEvent(input$nav_4_to_5_button, {
  closeAlert(session, "offendingChars")
  updateTabsetPanel(session, 'clinical_side_panel', selected = '5')
})
#5
output$nav_5_ui <- renderUI({
  div(
    tertiary_button('nav_5_to_4_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_5_to_6_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_5_to_4_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '4')
})
observeEvent(input$nav_5_to_6_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '6')
})
#6
output$nav_6_ui <- renderUI({
  div(
    tertiary_button('nav_6_to_5_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_6_to_expression_button', div('Next', icon('arrow-right')), class = "right_align")
  )
})
observeEvent(input$nav_6_to_5_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '5')
})
observeEvent(input$nav_6_to_expression_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '7')
})
#7
output$nav_7_ui <- renderUI({
  div(
    tertiary_button('nav_7_to_6_button', div(icon('arrow-left'), 'Back')),
    secondary_button('nav_7_to_expression_button', 'Next - Process assay data', class = "right_align")
  )
})
observeEvent(input$nav_7_to_6_button, {
  updateTabsetPanel(session, 'clinical_side_panel', selected = '6')
})
observeEvent(input$nav_7_to_expression_button, {
  updateTabsetPanel(session, 'top_level', selected = 'Assay data')
})
#expression 1
output$expression_nav_1_ui <- renderUI({
  div(
    tertiary_button('nav_1_to_clinical_button', div(icon('arrow-left'), 'Back')),
    secondary_button('expression_nav_1_to_2_button', div('Next', icon('arrow-right')), class = 'right_align')
  )
})
observeEvent(input$nav_1_to_clinical_button, {
  updateTabsetPanel(session, 'top_level', selected = 'Clinical data')
})
observeEvent(input$expression_nav_1_to_2_button, {
  updateTabsetPanel(session, 'expression_side_panel', selected = '2')
})
#expression 2
output$expression_nav_2_ui <- renderUI({
  div(
    tertiary_button('expression_nav_2_to_1_button', div(icon('arrow-left'), 'Back'))
  )
})
observeEvent(input$expression_nav_2_to_1_button, {
  updateTabsetPanel(session, 'expression_side_panel', selected = '1')
})