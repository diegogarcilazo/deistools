#' From cie10 to Enos. At the moment infectous diseases.
#' @param x death code.
#' @param age age.
#' @param age_code age code. Assume code_age levels 1 = years, 2 = months, 3 = days, 4 = hours, 5 = minutes.
#' @param sex sex. Assume sex levels 1 = male, 2 = female.

code_enos <- function(x, age, age_code, sex){

  dplyr::case_when(
    ((x >= 'A010' & x <= 'A042') & (x >= 'A044' & x <= 'A09X'))  ~ 'DIARREAS AGUDAS', #ok
    stringr::str_detect(x, 'A01') ~ '07 - FIEBRE TIFOIDEA Y PARATIFOIDEA',#ok
    #(x >= 'A000' & x <= 'A09X') ~ '10 - ENFERMEDADES DE TRANSIMISION ALIMENTARIA - (ETA)',
    #(x >= 'A540' & x <= 'A542') ~ '18 - URETRITIS GONOCOCCICA (GONORREA  BLENORRAGIA)',
    stringr::str_detect(x, 'A50') ~ 'SIFILIS CONGENITA',#ok
    (x >= 'A510' & x <= 'A53X') & sex == 1~ 'Sífilis temprana y sin especificar en varones',#ok
    (x >= 'A510' & x <= 'A53X') & sex == 2 ~ 'Sífilis temprana y sin especificar en mujeres',#ok
    (x >= 'B200' & x <= 'B24X') & ((age < 15 & age_code == 1) | (age_code %in% 2:5)) ~ 'HIV pediátrico',#ok
    stringr::str_detect(x, 'Z21') ~ 'Infección por HIV', #ok
    (x >= 'B200' & x <= 'B24X') & (age > 15 & age_code == 1) ~ 'SIDA',#ok
    (x >= 'T360' & x <= 'T50X') ~ '31 - INTOXICACIÓN MEDICAMENTOSA',#ok
    #(x >= 'T510' & x <= 'T65X') ~ '34 - OTROS TÓXICOS',
    #(x >= 'V010' & x <= 'V99X') ~ '35 - ACCIDENTES DE TRANSPORTE',
    #(x >= 'W000' & x <= 'X59X') ~ '36 - OTRAS CAUSAS EXTERNAS DE TRAUMATISMOS ACCIDENTALES',
    #(x >= 'X600' & x <= 'X84X') ~ '37 - LESIONES AUTOINFLINGIDAS INTENCIONALMENTE',
    #(x >= 'Y100' & x <= 'Y34X') ~ '38 - EVENTOS DE INTENCIÓN NO DETERMINADA',
    #(x >= 'Y400' & x <= 'Y84X') ~ '39 - COMPLICACIONES DE LA ATENCIÓN MÉDICA Y QUIRÚRGICA',
    #(x >= 'W000' & x <= 'Y34X') & !str_detect(x, 'Y06|Y07') ~ '40 - ACCIDENTES EN EL HOGAR',
    stringr::str_detect(x, 'A17') & ((age <= 4 & age_code == 1) | (age_code %in% 2:5)) ~ '45 - MENINGOENCEFALITIS TUBERCULOSA EN MENORES DE 5 AÑOS', #ok
    #(x >= 'J090' & x <= 'J11X') ~ '46 - INFLUENZA',
    (x >= 'J120' & x <= 'J18X') ~ '47 - NEUMONIA',#ok
    (x >= 'A150' & x <= 'A16X') | (stringr::str_detect(x, 'A17') & (age >= 5 & age_code == 1)) |
      (x >= 'A180' & x <= 'A19X') | stringr::str_detect(x, 'B90')~ 'TUBERCULOSIS',#ok
    (x >= 'B500' & x <= 'B54X')  ~ '59 - PALUDISMO (MALARIA)',#ok
    stringr::str_detect(x, 'A30') ~ '01 - LEPRA',#ok
    stringr::str_detect(x, 'T630') ~ '02 - OFIDISMO', #ok
    stringr::str_detect(x, 'T633') ~ '03 - ARACNIDISMO', #ok
    stringr::str_detect(x, 'T632') ~ '04 - ESCORPIONISMO', #ok
    #stringr::str_detect(x, 'A059') ~ '05 - INTOXICACION PARALÍTICA POR MOLUSCOS (IPM)',
    stringr::str_detect(x, 'A043') ~ '08 - SINDROME UREMICO HEMOLITICO (SUH)',#ok
    stringr::str_detect(x, 'A00') ~ '09 - COLERA', #ok
    stringr::str_detect(x, 'B75') ~ '11 - TRIQUINOSIS',#ok
    stringr::str_detect(x, 'A051') & (age > 1 & age_code == 1) ~ '12a - BOTULISMO', #ok
    stringr::str_detect(x, 'A051') & ((age < 1 & age_code == 1) | (age_code %in% 2:5)) ~ '12b - BOTULISMO DEL LACTANTE',#ok
    stringr::str_detect(x, 'B15') ~ '13 - HEPATITIS VIRAL TIPO A',#ok
    stringr::str_detect(x, 'B16') ~ '14 - HEPATITIS VIRAL TIPO B',#ok
    stringr::str_detect(x, 'B171') ~ '15 - HEPATITIS VIRAL TIPO C',#ok
    stringr::str_detect(x, 'B178') ~ '16 - HEPATITIS VIRAL TIPO D',#ok
    stringr::str_detect(x, 'B178') ~ '17 - HEPATITIS VIRAL TIPO E',#ok
    stringr::str_detect(x, 'B01') ~ '21 - VARICELA',#ok
    stringr::str_detect(x, 'B26') ~ '22 - PAROTIDITIS INFECCIOSA (Paperas)',#ok
    (x >= 'A800' & x <= 'A809') ~ 'POLIOMIELITIS',#ok
    stringr::str_detect(x, 'A37') ~ '24 - COQUELUCHE (Tos Convulsa)',#ok
    stringr::str_detect(x, 'A35') ~ '25 - TÉTANOS OTRAS EDADES', #ok
    stringr::str_detect(x, 'A33') ~ '26 - TETANOS NEONATAL', #ok
    stringr::str_detect(x, 'A36') ~ '27 - DIFTERIA', #ok
    stringr::str_detect(x, 'P350') ~ '28 - SINDROME DE RUBEOLA CONGENITA (SRC)', #ok
    (x >= 'B050' & x <= 'B06X') ~ '46 - Enfermedad Febril exantemática (Sarampión-Rubéola)', #ok
    stringr::str_detect(x, 'P350') ~ '30B - RUBEOLA CONGÉNITA',#ok
    stringr::str_detect(x, 'T58') ~ '32 - INTOXICACIÓN POR MONÓXIDO DE CARBONO',#ok
    stringr::str_detect(x, 'T60') ~ '33 - INTOXICACIÓN POR PLAGUICIDAS',#ok
    stringr::str_detect(x, 'A870') ~ 'Meningoencefalitis virales por Enterovirus',#ok
    stringr::str_detect(x, 'B261') ~ '41C - MENINGITIS URLEANA',#ok
    stringr::str_detect(x, 'A39') ~ 'Meningitis y otras invasivas por Neisseria meningitidis',#ok
    stringr::str_detect(x, 'G000') ~ '43 - MENINGITIS POR HAEMOPHILUS',#ok
    stringr::str_detect(x, 'G001') ~ '44 - MENINGITIS NEUMOCÓCCICA',#ok
    stringr::str_detect(x, 'G020') ~ 'Meningitis por otros virus',#ok
    stringr::str_detect(x, 'G03') ~ 'Meningitis sin especificar etiología',#ok
    (x >= 'G021' & x <= 'G028') ~ 'Meningitis micóticas y parasitarias', #ok
    (x >= 'G009' & x <= 'G01X') ~ 'Meningitis bacteriana sin especificar agente', #ok
    (x >= 'G021' & x <= 'G028') ~ 'Meningitis bacterianas por otros agentes', #ok
    (x >= 'A750' & x <= 'A79X') ~ 'Ricketsiosis', #ok
    stringr::str_detect(x, 'A81') ~ 'Encefalopatía espongiforme', #ok
    stringr::str_detect(x, 'A833') ~ 'Encefalitis de San Luis', #ok
    stringr::str_detect(x, 'J21') & ((age <= 2 & age_code == 1) | (age_code %in% 2:5)) ~ 'BRONQUIOLITIS < 2',#ok
    #stringr::str_detect(x, 'U049') ~ '49 - SINDROME RESPIRATORIO AGUDO SEVERO',
    stringr::str_detect(x, 'B550') ~ '51 - LEISHMANIASIS VISCERAL',#ok
    stringr::str_detect(x, 'B552') ~ '52 - LEISHMANIASIS MUCOSA',#ok
    stringr::str_detect(x, 'B551') ~ '53 - LEISHMANIASIS CUTANEA',#ok
    stringr::str_detect(x, 'A95') ~ '54 - FIEBRE AMARILLA',#ok
    #stringr::str_detect(x, 'A852') ~ '56 - ENCEFALITIS POR ARBOVIRUS',
    stringr::str_detect(x, 'A90') ~ '57 - DENGUE',#ok
    #stringr::str_detect(x, 'A91') ~ '58 - DENGUE HEMORRAGICO/ SINDROME DE CHOQUE DEL DENGUE (DH/SCD)',
    stringr::str_detect(x, 'B57') & ((age < 1 & age_code == 1) | (age_code %in% 2:5)) ~ 'ENFERMEDAD DE CHAGAS (AGUDO Y CONGÉNITO)',#ok
    stringr::str_detect(x, 'B57') & (age >= 1 & age_code == 1) ~ 'Chagas agudo vectorial',#ok
    stringr::str_detect(x, 'A75') ~ '61 - TIFUS EPIDEMICO',#ok
    stringr::str_detect(x, 'A20') ~ '62 - PESTE',#ok
    stringr::str_detect(x, 'A923') ~ '63 - FIEBRE DEL NILO OCCIDENTAL',#ok
    stringr::str_detect(x, 'A920') ~ 'Fiebre Chikunguña',#ok
    (x >= 'A928' & x <= 'A929') ~ 'Enfermedad por virus Zika', #ok
    stringr::str_detect(x, 'A82') ~ '64 - RABIA HUMANA', #ok
    stringr::str_detect(x, 'A70') ~ '65 - PSITACOSIS',#ok
    stringr::str_detect(x, 'A220') ~ '66A - CARBUNCO CUTÁNEO',#ok
    stringr::str_detect(x, 'A22') & !stringr::str_detect(x, 'A220') ~ '66B - CARBUNCO EXTRACUTÁNEO',#ok
    stringr::str_detect(x, 'B670') ~ '67 - HIDATIDOSIS',#ok
    stringr::str_detect(x, 'A23') ~ '68 - BRUCELOSIS',#ok
    stringr::str_detect(x, 'B334') ~ '69 - HANTAVIRUS - SINDROME PULMONAR',#ok
    stringr::str_detect(x, 'A27') ~ '70 - LEPTOSPIROSIS',#ok
    stringr::str_detect(x, 'A960') ~ '71 - FIEBRE HEMORRÁGICA ARGENTINA',#ok
    T ~ 'Not ENOs')
}
