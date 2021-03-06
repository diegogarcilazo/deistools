#' Function that create variable with Type of useless code from CIE10.
#' @param x chr cie10 four digits code.

code_useless <- function(x){
  dplyr::case_when(
    stringr::str_detect(x,
      paste0("A311|A59|A600|A630|B000|B07|B081|B088|B30|J30|J33|J342|J35|K14|L94|",
             "M03|M07|M353|M40|M459|N393|N40|N46|N60|N97|Q36|Q381|Q54|B948|B949|M45X|Y86|",
             "Y872|Y89|I10|I15|I70")) ~ "Tipo 1",
    (x >= "A710" & x <=  "A74X") ~ "Tipo 1",
    (x >= "B350" & x <=  "B36X") ~ "Tipo 1",
    (x >= "F320" & x <=  "F339") ~ "Tipo 1",
    (x >= "F400" & x <=  "F429") ~ "Tipo 1",
    (x >= "F450" & x <=  "F489") ~ "Tipo 1",
    (x >= "F510" & x <=  "F539") ~ "Tipo 1",
    (x >= "F600" & x <=  "F989") ~ "Tipo 1",
    (x >= "G430" & x <=  "G459") ~ "Tipo 1",
    (x >= "G470" & x <= "G529") ~ "Tipo 1",
    (x >= "G540" & x <= "G549") ~ "Tipo 1",
    (x >= "G560" & x <= "G589") ~ "Tipo 1",
    (x >= "H000" & x <= "H049") ~ "Tipo 1",
    (x >= "H052" & x <= "H699") ~ "Tipo 1",
    (x >= "H710" & x <= "H809") ~ "Tipo 1",
    (x >= "H830" & x <= "H93X") ~ "Tipo 1",
    (x >= "L040" & x <= "L089") ~ "Tipo 1",
    (x >= "L200" & x <= "L259") ~ "Tipo 1",
    (x >= "L280" & x <= "L879") ~ "Tipo 1",
    (x >= "L900" & x <= "L92X") ~ "Tipo 1",
    (x >= "M090" & x <= "M12X") ~ "Tipo 1",
    (x >= "M140" & x <= "M25X") ~ "Tipo 1",
    (x >= "M730" & x <= "M79X") ~ "Tipo 1",
    (x >= "M950" & x <= "M99X") ~ "Tipo 1",
    (x >= "N840" & x <= "N93X") ~ "Tipo 1",
    (x >= "Q100" & x <= "Q18X") ~ "Tipo 1",
    (x >= "Q650" & x <= "Q74X") ~ "Tipo 1",
    (x >= "Q820" & x <= "Q84X") ~ "Tipo 1",
    (x >= "G800" & x <= "G83X") ~ "Tipo 1",
    (x >= "K000" & x <= "K119") ~ "Tipo 1",
    (x >= "M470" & x <= "M60X") ~ "Tipo 1",
    (x >= "M630" & x <= "M71X") ~ "Tipo 1",
    (x >= "L980" & x <= "L983") ~ "Tipo 1",
    (x >= "L985" & x <= "L989") ~ "Tipo 1",
    (x >= "M436" & x <= "M439") ~ "Tipo 1",
    stringr::str_detect(x, paste0("A480|A483|G911|G92|I26|I271|I44|I74|I81|J69|J86|J90|",
                                  "J93|J94|K75|M86|N14")) ~ "Tipo 2",
    (x >= "A400" & x <= "A41X") ~ "Tipo 2",
    (x >= "E853" & x <= "E859") ~ "Tipo 2",
    (x >= "E860" & x <= "E87X") ~ "Tipo 2",
    (x >= "G931" & x <= "G936") ~ "Tipo 2",
    (x >= "I490" & x <= "I50X") ~ "Tipo 2",
    (x >= "J800" & x <= "J81X") ~ "Tipo 2",
    (x >= "G913" & x <= "G918") ~ "Tipo 2",
    (x >= "J938" & x <= "J939") ~ "Tipo 2",
    (x >= "J981" & x <= "J983") ~ "Tipo 2",
    (x >= "K650" & x <= "K66X") ~ "Tipo 2",
    (x >= "K760" & x <= "K764") ~ "Tipo 2",
    (x >= "K710" & x <= "K72X") & !stringr::str_detect(x, "K717") ~ "Tipo 2",
    (x >= "K920" & x <= "K922") ~ "Tipo 2",
    (x >= "N170" & x <= "N19X") ~ "Tipo 2",
    stringr::str_detect(x, "D65|J96") ~ "Tipo 3",
    (x >= "I450" & x <= "I46X") ~ "Tipo 3",
    stringr::str_detect(x, "C80|C26|C39|C579|C649|C76|A499|B839|B99|E889|I51|I99|X59") ~ "Tipo 4",
    (x >= "D000" & x <= "D13X") ~ "Tipo 4",
    (x >= "D160" & x <= "D18X") ~ "Tipo 4",
    (x >= "D200" & x <= "D24X") ~ "Tipo 4",
    (x >= "D280" & x <= "D48X") ~ "Tipo 4",
    (x >= "Y100" & x <= "Y34X") ~ "Tipo 4",
    (x >= "R000" & x <= "R99X") ~ "Tipo 5",
    TRUE ~ NA_character_
    )}
