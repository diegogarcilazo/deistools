library(tidyverse)

tbl <- readxl::read_excel("/home/diego/Dropbox/Rprojects/db_def/datasets/equiv_cie.xlsx")

I <- "^[A|B][0-9][0-9]|^[0][0-9][0-9]|^[1][0-3][0-9]|^R75|279[56]|7958"
II <- "^C[0-9][0-9]|^D[0-3][0-9]|^D4[0-8]|^1[4-9][0-9]|^2[0-3][0-9]|^273[13]|^2898"
III <- "(?=^D[5-8][0-9]|^279|^28[0-9]|^273[02])(?!^2795|^2796|^2898)"
IV <- "(?=^E[0-8][0-9]|^E90|^2[4-6][0-9]|^27[0-8]|^330[01])(?!^273[0-3]|^274)"
V <- "^F[0-9][0-9]|^29[0-9]|^3[0-1][0-9]"
VI_VIII <- "(?=^G[0-9][0-9]|^H[0-8][0-9]|^H9[0-5]|^3[2-8][0-9]|^435)(?!^330[01])"
IX <- "(?=^I[0-9][0-9]|^39[0-9]|^4[0-5][0-9])(?!^4275|^4590|^435|^446)"
X <- "^J[0-9][0-9]|^4[6-9][0-9]|^5[0-1][0-9]|^7860"
XI <- "^K[0-8][0-9]|^K9[0-3][0-9]|^5[2-7][0-9]"
XII <- "^L[0-9][0-9]|^6[8-9][0-9]|^70[0-9]"
XIII <- "(?=^M[0-9][0-9]|^7[1-3][0-9])(?!^274|^446)"
XIV <- "^N[0-9][0-9]|^5[8-9][0-9]|^6[0-2][0-9]"
XV <- "^O[0-9][0-9]|^6[3-6][0-9]|^67[0-6]"
XVI <- "^P[0-8][0-9]|^P9[0-6]|^7[67][0-9]"
XVII <- "^Q[0-9][0-9]|^7[45][0-9]"
XVIII <- "(?=^R[0-6][0-9]|^R7[0-4]|^R76|^R[7-9][0-9]|^7[89][0-9]|^4275|^4590)(?!^7860|^7958)"
XX <- "^V[0-9][1-9]|^V[1-9][0-9]|^[W-X][0-9][0-9]|^Y[0-8][0-9]|^[89][0-9][0-9]"
`064` <- "^J4[0-4]|^49[0-2]|^49[4-6]|^J47"
`090` <- "^V[0-9][1-9]|^V[1-9][0-9]|^8[0-3][0-9]|^84[0-8]"
`055` <- "^I21|^410"
`059` <- "^I6[0-9]|^43[0-4|6-8]"
`006` <- "^B2[0-4]|79[56]"
`007` <- "^R75|^7958"
`023` <- "^C50|^17[45]"
`024` <- "^C53|^180"
`028` <- "^C61|^185"
`012` <- "^C18|^153"


chap <- c(I, II, III, IV, V, VI_VIII, IX, X, XI, XII, XIII, XIV, XV, XVI, XVII, XVIII, XX,
          `064`, `090`, `055`, `059`, `006`, `007`, `023`, `024`, `028`, `012`)
chap_abb <- c('I','II','III','IV','V','VI-VIII','IX','X','XI','XII','XIII','XIV','XV','XVI',
              'XVII','XVIII','XX', '064', '090', '055', '059', '006', '007', '023', '024', '028', '012')

regex <- tibble(code_abb = chap_abb, code_regex = chap)

equiv_cie <- tbl %>%
  separate(capitulo, c("code_abb", "code_num"), sep = "\\.") %>%
  separate(`Grupos de causas`, c("code_abb2", "names"), sep = " ", remove = F) %>%
  mutate(code_num2 = coalesce(code_num, code_abb2)) %>%
  mutate(code_num = str_trim(code_num2),
         code_abb = coalesce(code_abb, code_num),
         code_des = str_trim(str_remove_all(`Grupos de causas`, "[0-9]"))) %>%
  left_join(regex) %>%
  select(code_abb, code_des, detail_cie10 = `detallada CIE-10`, detail_cie09 = `detallada CIE-9`,  code_regex)

saveRDS(equiv_cie, "/home/diego/Dropbox/Rprojects/my_dev/biotools/data/equiv_cie.rds")
