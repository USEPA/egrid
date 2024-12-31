## -------------------------------
##
## Function create format styles
## 
## Purpose: 
## 
## This file creates all headers used in eGRID
## This includes all operating units for the specified eGRID data year
##
##
## Authors:  
##      Madeline Zhang, Abt Global
##
## -------------------------------


create_format_styles <- function(font = "Arial", size = 8.5) {
  
  #' create_format_styles
  #' 
  #' Helper function to create headers for final_formatting.R
  #'
  #' @return Formatted lists with styles
  #' @examples
  #' s <- create_format_styles() # Default font format styles into a list
  
  ### Create Styles ------
  
  ### header & description style ###
  
  # header style
  header_style <- createStyle(textDecoration = "bold",
                              fgFill = "#F2F2F2", 
                              wrapText = TRUE,
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  # description style
  desc_style <- createStyle(wrapText = TRUE,
                            halign = "center",
                            valign = "center",
                            textDecoration = "bold",
                            fgFill = "#F2F2F2", 
                            fontName = font,
                            fontSize = size,
                            border = "TopBottomLeftRight",
                            borderStyle = "thin")
  ### text styles ###
  
  # bold style (for text/characters)
  bold <- createStyle(fontName = font,
                      fontSize = size,
                      textDecoration = "bold")
  
  # basic style (for text/characters)
  basic <- createStyle(fontName = font,
                       fontSize = size)
  
  ### number styles ### 
  
  # basic style (for larger integers)
  integer <- createStyle(numFmt = "#,##0",
                         fontName = font,
                         fontSize = size)
  
  integer2 <- createStyle(numFmt = "#,##0; (#,##0)",
                          fontName = font,
                          fontSize = size)
  
  # basic style (for percentages with 1 decimal place)
  percent <- createStyle(numFmt = "0.0%",
                         fontName = font,
                         fontSize = size)
  
  # different decimal styles for different purposes
  decimal1 <- createStyle(numFmt = "#,##0.000",
                          fontName = font,
                          fontSize = size)
  
  decimal2 <- createStyle(numFmt = "#,##0.0",
                          fontName = font,
                          fontSize = size)
  
  decimal3 <- createStyle(numFmt = "#,##0.000; (#,##0.000)",
                          fontName = font,
                          fontSize = size)
  
  decimal4 <- createStyle(numFmt = "#,##0.0000",
                          fontName = font,
                          fontSize = size)
  
  decimal5 <- createStyle(numFmt = "#,##0.00",
                          fontName = font,
                          fontSize = size)
  
  ### bold number styles ### 
  
  # bold style (for large integers)
  integer_bold <- createStyle(numFmt = "#,##0",
                              fontName = font,
                              fontSize = size,
                              textDecoration = "bold")
  
  # bold style (for percentages with 1 decimal place)
  percent_bold <- createStyle(numFmt = "0.0%",
                              fontName = font,
                              fontSize = size,
                              textDecoration = "bold")
  
  ### Header Styles (colors) -----
  
  # 1. Annual Values (#F2DCDB)
  color1_header <- createStyle(textDecoration = "bold",
                               fgFill = "#F2DCDB", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color1_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#F2DCDB", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 2. Unadjusted Annual Values (#E6B8B7)
  color2_header <- createStyle(textDecoration = "bold",
                               fgFill = "#E6B8B7", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color2_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#E6B8B7", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 3. Adjustment Values (#D58785)
  color3_header <- createStyle(textDecoration = "bold",
                               fgFill = "#D58785", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color3_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#D58785", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 4. Output Emission Rates (#EBF1DE)
  color4_header <- createStyle(textDecoration = "bold",
                               fgFill = "#EBF1DE", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color4_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#EBF1DE", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 5. Input Emission Rates (#C4D79B)
  color5_header <- createStyle(textDecoration = "bold",
                               fgFill = "#C4D79B", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color5_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#C4D79B", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 6. Combustion Output Rates
  color6_header <- createStyle(textDecoration = "bold",
                               fgFill = "#76933C", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin",
                               fontColour = "white")
  
  color6_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#76933C", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin",
                             fontColour = "white")
  
  # 7. Generation by Fuel Type (#DAEEF3)
  color7_header <- createStyle(textDecoration = "bold",
                               fgFill = "#DAEEF3", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color7_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#DAEEF3", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 8. Renewable and Non-Renewable Generation (#92CDDC)
  color8_header <- createStyle(textDecoration = "bold",
                               fgFill = "#92CDDC", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color8_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#92CDDC", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  # 9. Combustion and Non-Combustion Generation (#3DA2BD) #31869B
  color9_header <- createStyle(textDecoration = "bold",
                               fgFill = "#3DA2BD", 
                               wrapText = TRUE,
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin")
  
  color9_desc <- createStyle(wrapText = TRUE,
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             fgFill = "#3DA2BD", 
                             fontName = font,
                             fontSize = size,
                             border = "TopBottomLeftRight",
                             borderStyle = "thin")
  
  color9v2_header <- createStyle(textDecoration = "bold",
                                 fgFill = "#31869B", 
                                 wrapText = TRUE,
                                 fontName = font,
                                 fontSize = size,
                                 border = "TopBottomLeftRight",
                                 borderStyle = "thin",
                                 fontColour = "white")
  
  color9v2_desc <- createStyle(wrapText = TRUE,
                               halign = "center",
                               valign = "center",
                               textDecoration = "bold",
                               fgFill = "#31869B", 
                               fontName = font,
                               fontSize = size,
                               border = "TopBottomLeftRight",
                               borderStyle = "thin",
                               fontColour = "white")
  
  # 10. Resource Mix (#FDE9D9)
  color10_header <- createStyle(textDecoration = "bold",
                                fgFill = "#FDE9D9", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color10_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#FDE9D9", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 11. Renewable and Non-Renewable Resource Mix (#FABF8F)
  color11_header <- createStyle(textDecoration = "bold",
                                fgFill = "#FABF8F", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color11_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#FABF8F", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 12. Combustion and Non-Combustion Resource Mix (#F6903C) (#E26B0A)
  color12_header <- createStyle(textDecoration = "bold",
                                fgFill = "#F6903C", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color12_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#F6903C", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  color12v2_header <- createStyle(textDecoration = "bold",
                                  fgFill = "#E26B0A", 
                                  wrapText = TRUE,
                                  fontName = font,
                                  fontSize = size,
                                  border = "TopBottomLeftRight",
                                  borderStyle = "thin")
  
  color12v2_desc <- createStyle(wrapText = TRUE,
                                halign = "center",
                                valign = "center",
                                textDecoration = "bold",
                                fgFill = "#E26B0A", 
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  # 13. Output Emission Rates by Fuel Type
  color13_header <- createStyle(textDecoration = "bold",
                                fgFill = "#FFFFCC", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color13_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#FFFFCC", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 14. Input Emission Rates by Fuel Type
  color14_header <- createStyle(textDecoration = "bold",
                                fgFill = "#FFFF99", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color14_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#FFFF99", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 15. Nonbaseload Output Emission Rates
  color15_header <- createStyle(textDecoration = "bold",
                                fgFill = "#E4DFEC", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color15_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#E4DFEC", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 16. Nonbaseload Generation by Fuel Type
  color16_header <- createStyle(textDecoration = "bold",
                                fgFill = "#B1A0C7", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin")
  
  color16_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#B1A0C7", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin")
  
  # 17. Nonbaseload Resource Mix
  color17_header <- createStyle(textDecoration = "bold",
                                fgFill = "#60497A", 
                                wrapText = TRUE,
                                fontName = font,
                                fontSize = size,
                                border = "TopBottomLeftRight",
                                borderStyle = "thin",
                                fontColour = "white")
  
  color17_desc <- createStyle(wrapText = TRUE,
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              fgFill = "#60497A", 
                              fontName = font,
                              fontSize = size,
                              border = "TopBottomLeftRight",
                              borderStyle = "thin",
                              fontColour = "white")
  
  ### Return List of Styles -----
  # naming each style for easier indexing in script
  list(header_style = header_style,
       desc_style = desc_style,
       
       bold = bold,
       basic = basic,
       
       integer = integer,
       integer2 = integer2,
       percent = percent,
       decimal1 = decimal1,
       decimal2 = decimal2,
       decimal3 = decimal3,
       decimal4 = decimal4,
       decimal5 = decimal5,
       
       integer_bold = integer_bold,
       percent_bold = percent_bold,

       color1_header = color1_header,
       color1_desc = color1_desc,
       
       color2_header = color2_header,
       color2_desc = color2_desc,
       
       color3_header = color3_header,
       color3_desc = color3_desc,
       
       color4_header = color4_header,
       color4_desc = color4_desc,
       
       color5_header = color5_header,
       color5_desc = color5_desc,
       
       color6_header = color6_header,
       color6_desc = color6_desc,
       
       color7_header = color7_header,
       color7_desc = color7_desc,
       
       color8_header = color8_header,
       color8_desc = color8_desc,
       
       color9_header = color9_header,
       color9_desc = color9_desc,
       
       color9v2_header = color9v2_header,
       color9v2_desc = color9v2_desc,
       
       color10_header = color10_header,
       color10_desc = color10_desc,
       
       color11_header = color11_header,
       color11_desc = color11_desc,
       
       color12_header = color12_header,
       color12_desc = color12_desc,
       
       color12v2_header = color12v2_header,
       color12v2_desc = color12v2_desc,
       
       color13_header = color13_header,
       color13_desc = color13_desc,
       
       color14_header = color14_header,
       color14_desc = color14_desc,
       
       color15_header = color15_header,
       color15_desc = color15_desc,
       
       color16_header = color16_header,
       color16_desc = color16_desc,
       
       color17_header = color17_header,
       color17_desc = color17_desc
  )
  
  
}


