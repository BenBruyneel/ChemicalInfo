
output$structurePlot <- renderChemdoodle({
  # https://github.com/zachcp/chemdoodle/blob/master/R/chemdoodle_viewer.R
  if (rv$smiles != "") {
    chemdoodle_viewer(smiles = rv$smiles,
                      width = input$structureWidth,
                      height = input$structureHeight,
                      bondscale = input$structurePlotSize,
                      atoms_font_size_2D = input$atomsFontSize2D,
                      atoms_implicitHydrogens_2D = input$showAllHydrogens,
                      atoms_displayAllCarbonLabels_2D = input$showAllC,
                      atoms_displayTerminalCarbonLabels_2D = input$showTerminalC)
  }
})
