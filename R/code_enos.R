#' From cie10 to Enos. At the moment infectous diseases.
#' @param x death code.
#' @param age age.
#' @param age_code age code. Assume code_age levels 1 = years, 2 = months, 3 = days, 4 = hours, 5 = minutes.
#' @param sex sex. Assume sex levels 1 = male, 2 = female.

code_enos <- function(x, age, age_code, sex){

  dplyr::case_when(
    ((x >= 'A020' & x <= 'A042') | (x >= 'A044' & x <= 'A09X')) ~ 'Diarreas Agudas', #ok
    stringr::str_detect(x, 'A01') ~ 'Fiebre Tifoidea Y Paratifoidea',#ok
    #(x >= 'A000' & x <= 'A09X') ~ '10 - ENFERMEDADES DE TRANSIMISION ALIMENTARIA - (ETA)',
    #(x >= 'A540' & x <= 'A542') ~ '18 - URETRITIS GONOCOCCICA (GONORREA  BLENORRAGIA)',
    stringr::str_detect(x, 'A50') ~ 'Sífilis Congénita',#ok
    ((x >= 'A510' & x <= 'A519') | (x >= 'A530' & x <= 'A539')) & sex == 1~ 'Sífilis temprana y sin especificar en varones',#ok
    ((x >= 'A510' & x <= 'A519') | (x >= 'A530' & x <= 'A539')) & sex == 2 ~ 'Sífilis temprana y sin especificar en mujeres',#ok
    (x >= 'B200' & x <= 'B24X') & ((age < 15 & age_code == 1) | (age_code %in% 2:5)) ~ 'HIV pediátrico',#ok
    stringr::str_detect(x, 'Z21') ~ 'Infección por HIV asintomático', #ok
    (x >= 'B200' & x <= 'B24X') & (age >= 15 & age_code == 1) ~ 'SIDA',#ok
    (x >= 'T360' & x <= 'T509') ~ 'Intoxicación Medicamentosa',#ok
    #(x >= 'T510' & x <= 'T65X') ~ '34 - OTROS TÓXICOS',
    #(x >= 'V010' & x <= 'V99X') ~ '35 - ACCIDENTES DE TRANSPORTE',
    #(x >= 'W000' & x <= 'X59X') ~ '36 - OTRAS CAUSAS EXTERNAS DE TRAUMATISMOS ACCIDENTALES',
    #(x >= 'X600' & x <= 'X84X') ~ '37 - LESIONES AUTOINFLINGIDAS INTENCIONALMENTE',
    #(x >= 'Y100' & x <= 'Y34X') ~ '38 - EVENTOS DE INTENCIÓN NO DETERMINADA',
    #(x >= 'Y400' & x <= 'Y84X') ~ '39 - COMPLICACIONES DE LA ATENCIÓN MÉDICA Y QUIRÚRGICA',
    #(x >= 'W000' & x <= 'Y34X') & !str_detect(x, 'Y06|Y07') ~ '40 - ACCIDENTES EN EL HOGAR',
    stringr::str_detect(x, 'A17') & ((age <= 4 & age_code == 1) | (age_code %in% 2:5)) ~ 'Meningoencefalitis Tuberculosa en menores de 5 años', #ok
    #(x >= 'J090' & x <= 'J11X') ~ '46 - INFLUENZA',
    (x >= 'J120' & x <= 'J189') ~ 'Neumonía',#ok
    (x >= 'A15' & x <= 'A169') | (stringr::str_detect(x, 'A17') & (age >= 5 & age_code == 1)) |
      (x >= 'A180' & x <= 'A199') | stringr::str_detect(x, 'B90')~ 'Tuberculosis',#ok
    (x >= 'B500' & x <= 'B54X')  ~ 'Paludismo (Malaria)',#ok
    stringr::str_detect(x, 'A30') ~ 'Lepra',#ok
    stringr::str_detect(x, 'T630') ~ 'Ofidismo', #ok
    stringr::str_detect(x, 'T633') ~ 'Aracnoidismo', #ok
    stringr::str_detect(x, 'T632') ~ 'Alacranismo', #ok
    #stringr::str_detect(x, 'A059') ~ '05 - INTOXICACION PARALÍTICA POR MOLUSCOS (IPM)',
    stringr::str_detect(x, 'A043') ~ 'Síndrome Urémico Hemolítico (SUH)',#ok
    stringr::str_detect(x, 'A00') ~ 'Cólera', #ok
    stringr::str_detect(x, 'B75') ~ 'Triquinosis',#ok
    stringr::str_detect(x, 'A051') & (age >= 1 & age_code == 1) ~ 'Botulismo', #ok
    stringr::str_detect(x, 'A051') & ((age < 1 & age_code == 1) | (age_code %in% 2:5)) ~ 'Botulismo del lactante',#ok
    stringr::str_detect(x, 'B15') ~ 'HEPATITIS VIRAL TIPO A',#ok
    stringr::str_detect(x, 'B16') ~ 'HEPATITIS VIRAL TIPO B',#ok
    stringr::str_detect(x, 'B171') ~ 'HEPATITIS VIRAL TIPO C',#ok
    stringr::str_detect(x, 'B178') ~ 'HEPATITIS VIRAL TIPO D',#ok
    stringr::str_detect(x, 'B172') ~ 'HEPATITIS VIRAL TIPO E',#ok
    stringr::str_detect(x, 'B01') ~ 'VARICELA',#ok
    stringr::str_detect(x, 'B26') ~ 'PAROTIDITIS INFECCIOSA (Paperas)',#ok
    (x >= 'A800' & x <= 'A809') ~ 'Poliomelitis',#ok
    stringr::str_detect(x, 'A37') ~ 'COQUELUCHE (Tos Convulsa)',#ok
    stringr::str_detect(x, 'A35') ~ 'TÉTANOS OTRAS EDADES', #ok
    stringr::str_detect(x, 'A33') ~ 'TETANOS NEONATAL', #ok
    stringr::str_detect(x, 'A36') ~ 'Difteria', #ok
    stringr::str_detect(x, 'P350') ~ 'SINDROME DE RUBEOLA CONGENITA (SRC)', #ok
    (x >= 'B050' & x <= 'B069') ~ 'Enfermedad Febril exantemática (Sarampión-Rubéola)', #ok
    stringr::str_detect(x, 'P350') ~ 'RUBEOLA CONGÉNITA',#ok
    stringr::str_detect(x, 'T58') ~ 'INTOXICACIÓN POR MONÓXIDO DE CARBONO',#ok
    stringr::str_detect(x, 'T60') ~ 'INTOXICACIÓN POR PLAGUICIDAS',#ok
    stringr::str_detect(x, 'A870') ~ 'Meningoencefalitis virales por Enterovirus',#ok
    stringr::str_detect(x, 'B261') ~ 'MENINGITIS URLEANA',#ok
    stringr::str_detect(x, 'A39') ~ 'Meningitis y otras invasivas por Neisseria meningitidis',#ok
    stringr::str_detect(x, 'G000') ~ 'MENINGITIS POR HAEMOPHILUS',#ok
    stringr::str_detect(x, 'G001') ~ 'MENINGITIS NEUMOCÓCCICA',#ok
    stringr::str_detect(x, 'G020') ~ 'Meningitis por otros virus',#ok
    (x >= 'G002' & x <= 'G008') ~ 'Meningitis bacterianas por otros agentes',#ok
    stringr::str_detect(x, 'G03') ~ 'Meningitis sin especificar etiología',#ok
    stringr::str_detect(x, 'B375') | stringr::str_detect(x, 'B384') | stringr::str_detect(x, 'B451') ~ 'Meningitis micóticas', #ok
    (x >= 'G009' & x <= 'G01X') ~ 'Meningitis bacteriana sin especificar agente', #ok
    (x >= 'A750' & x <= 'A799') ~ 'Ricketsiosis', #ok
    stringr::str_detect(x, 'A81') ~ 'Encefalopatía espongiforme', #ok
    stringr::str_detect(x, 'A833') ~ 'Encefalitis de San Luis', #ok
    stringr::str_detect(x, 'J21') & ((age <= 2 & age_code == 1) | (age_code %in% 2:5)) ~ 'BRONQUIOLITIS < 2',#ok
    #stringr::str_detect(x, 'U049') ~ '49 - SINDROME RESPIRATORIO AGUDO SEVERO',
    stringr::str_detect(x, 'B550') ~ 'LEISHMANIASIS VISCERAL',#ok
    stringr::str_detect(x, 'B552') ~ 'LEISHMANIASIS MUCOSA',#ok
    stringr::str_detect(x, 'B551') ~ 'LEISHMANIASIS CUTANEA',#ok
    stringr::str_detect(x, 'A95') ~ 'Fiebre Amarilla',#ok
    #stringr::str_detect(x, 'A852') ~ '56 - ENCEFALITIS POR ARBOVIRUS',
    stringr::str_detect(x, 'A90') ~ 'Dengue',#ok
    #stringr::str_detect(x, 'A91') ~ '58 - DENGUE HEMORRAGICO/ SINDROME DE CHOQUE DEL DENGUE (DH/SCD)',
    stringr::str_detect(x, 'B57') ~ 'Enfermedad de chagas',#ok
    stringr::str_detect(x, 'A75') ~ 'Tifus Epidémico',#ok
    stringr::str_detect(x, 'A20') ~ 'Peste',#ok
    stringr::str_detect(x, 'A923') ~ 'Fiebre del nilo occidental',#ok
    stringr::str_detect(x, 'A920') ~ 'Fiebre Chikunguña',#ok
    (x >= 'A928' & x <= 'A929') ~ 'Enfermedad por virus Zika', #ok
    stringr::str_detect(x, 'A82') ~ 'Rabia Humana', #ok
    stringr::str_detect(x, 'A70') ~ 'Psitacosis',#ok
    stringr::str_detect(x, 'A220') ~ 'Carbunco Cutáneo',#ok
    stringr::str_detect(x, 'A22') & !stringr::str_detect(x, 'A220') ~ 'Carbunco Extracutáneo',#ok
    stringr::str_detect(x, 'B67') ~ 'Hidatidosis',#ok
    stringr::str_detect(x, 'A23') ~ 'Brucelosis',#ok
    stringr::str_detect(x, 'B334') ~ 'Hantavirus - Síndrome Pulmonar',#ok
    stringr::str_detect(x, 'A27') ~ 'Leptospirosis',#ok
    stringr::str_detect(x, 'A960') ~ 'Fiebre Hemorrágica Argentina',#ok
    stringr::str_detect(x, 'B03') ~ 'Viruela', #ok
    is.na(x) ~ NA_character_,
    T ~ 'Not ENOs')
}




