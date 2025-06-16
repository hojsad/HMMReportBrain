f <- np$load("./files/f.npy") #
p <- np$load("./files/p.npy") #
c <- np$load("./files/c.npy")
mean_c <- np$load("./files/mean_c.npy")
#f <- data.frame(f)
psd <- np$load("./files/psd.npy")
#psd <- data.frame(psd)
coh <- np$load("./files/coh.npy")
#covs <- np$load("./files/covs.npy")
fo <- np$load("./files/fo.npy")
fo <- data.frame(fo)
intv <- np$load("./files/intv.npy")
intv <- data.frame(intv)
lt <- np$load("./files/lt.npy")
lt <- data.frame(lt)
sr <- np$load("./files/sr.npy")
sr <- data.frame(sr)
w <- np$load("./files/w.npy")
nnmf <- np$load("./files/nnmf_2.npy")
#filenames <- read_csv("filenames.csv")
demo <- read_csv("./files/demo118.csv")
roi <- read_csv("./files/roi.csv")
psd_mean <- read_csv("./files/psd_mean_df.csv")
pow <- read_csv("./files/p_mean_df.csv")
ROI_Glasser <- read_csv("./files/ROI_Glasser.csv")
demo32gt <- read_csv("./files/demo32.csv")

#########################################################
ID <- demo$original
ROI <- roi$ROI
States_names <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6"
)
Group <- c(
  "LBD",
  "aMCI_AD",
  "PD",
  "NC"
)
fr <- c(
  1,
  1.5,
  2,
  2.5,
  3,
  3.5,
  4,
  4.5,
  5,
  5.5,
  6,
  6.5,
  7,
  7.5,
  8,
  8.5,
  9,
  9.5,
  10,
  10.5,
  11,
  11.5,
  12,
  12.5,
  13,
  13.5,
  14,
  14.5,
  15,
  15.5,
  16,
  16.5,
  17,
  17.5,
  18,
  18.5,
  19,
  19.5,
  20,
  20.5,
  21,
  21.5,
  22,
  22.5,
  23,
  23.5,
  24,
  24.5,
  25,
  25.5,
  26,
  26.5,
  27,
  27.5,
  28,
  28.5,
  29,
  29.5,
  30,
  30.5,
  31,
  31.5,
  32,
  32.5,
  33,
  33.5,
  34,
  34.5,
  35,
  35.5,
  36,
  36.5,
  37,
  37.5,
  38,
  38.5,
  39,
  39.5,
  40,
  40.5,
  41,
  41.5,
  42,
  42.5,
  43,
  43.5,
  44,
  44.5
)
dimnames(p) <- list(ID, States_names, ROI)
dimnames(coh) <- list(ID, States_names, ROI, ROI, fr)
dimnames(psd) <- list(ID, States_names, ROI, fr)
LBD_indices <- demo$original[demo$Group == "LBD"]
LBD_p <- p[LBD_indices, , , drop = FALSE]
LBD_c <- coh[LBD_indices, , , , , drop = FALSE]
LBD_p_mean <- apply(LBD_p, c(2, 3), mean)
LBD_c_mean <- apply(LBD_c, c(2, 3, 4), mean)
LBD_psd <- psd[LBD_indices, , , , drop = FALSE]
LBD_psd_mean <- apply(LBD_psd, c(2, 3, 4), mean)
LBD_psd_states <- apply(LBD_psd_mean, c(1, 3), mean)

#aMCI_AD_indices <- demo$ID[demo$flag == "aMCI_AD"]
#aMCI_AD_p <- p[aMCI_AD_indices, , , drop = FALSE]
#aMCI_AD_c <- c[aMCI_AD_indices, , , , drop = FALSE]
#aMCI_AD_p_mean <- apply(aMCI_AD_p, c(2, 3), mean)
#aMCI_AD_c_mean <- apply(aMCI_AD_c, c(2, 3,4), mean)
#aMCI_AD_psd <- psd[aMCI_AD_indices, , , , drop = FALSE]
#aMCI_AD_psd_mean <- apply(aMCI_AD_psd, c(2, 3, 4), mean)
#aMCI_AD_psd_states <- apply(aMCI_AD_psd_mean,c(1, 3), mean)

PD_indices <- demo$original[demo$Group == "PD"]
PD_p <- p[PD_indices, , , drop = FALSE]
PD_c <- coh[PD_indices, , , , , drop = FALSE]
PD_p_mean <- apply(PD_p, c(2, 3), mean)
PD_c_mean <- apply(PD_c, c(2, 3, 4), mean)
PD_psd <- psd[PD_indices, , , , drop = FALSE]
PD_psd_mean <- apply(PD_psd, c(2, 3, 4), mean)
PD_psd_states <- apply(PD_psd_mean, c(1, 3), mean)

NC_indices <- demo$original[demo$Group == "NC"]
NC_p <- p[NC_indices, , , drop = FALSE]
NC_c <- coh[NC_indices, , , , , drop = FALSE]
NC_p_mean <- apply(NC_p, c(2, 3), mean)
NC_c_mean <- apply(NC_c, c(2, 3, 4), mean)
NC_psd <- psd[NC_indices, , , , drop = FALSE]
NC_psd_mean <- apply(NC_psd, c(2, 3, 4), mean)
NC_psd_states <- apply(NC_psd_mean, c(1, 3), mean)

CC_indices <- grep("^sub-", demo$original)
CC_p <- p[CC_indices, , , drop = FALSE]
CC_c <- coh[CC_indices, , , , , drop = FALSE]
CC_p_mean <- apply(CC_p, c(2, 3), mean)
CC_c_mean <- apply(CC_c, c(2, 3, 4), mean)
CC_psd <- psd[CC_indices, , , , drop = FALSE]
CC_psd_mean <- apply(CC_psd, c(2, 3, 4), mean)
CC_psd_states <- apply(CC_psd_mean, c(1, 3), mean)

demo_nc <- demo %>% filter(Group == "NC")
NINANC_indices <- grep("^NINA", demo_nc$original)
NINANC_p <- p[NINANC_indices, , , drop = FALSE]
NINANC_c <- coh[NINANC_indices, , , , , drop = FALSE]
NINANC_p_mean <- apply(NINANC_p, c(2, 3), mean)
NINANC_c_mean <- apply(NINANC_c, c(2, 3, 4), mean)
NINANC_psd <- psd[NINANC_indices, , , , drop = FALSE]
NINANC_psd_mean <- apply(NINANC_psd, c(2, 3, 4), mean)
NINANC_psd_states <- apply(NINANC_psd_mean, c(1, 3), mean)
################################################################

LBD_psd_df <- psd_df(LBD_psd_states, f)
PD_psd_df <- psd_df(PD_psd_states, f)
NC_psd_df <- psd_df(NC_psd_states, f)
CC_psd_df <- psd_df(CC_psd_states, f)
NINANC_psd_df <- psd_df(NINANC_psd_states, f)
psd_list <- list(LBD_psd_df, PD_psd_df, NC_psd_df, CC_psd_df, NINANC_psd_df)
psd_flag <- c("LBD", "PD", "NC", "CC", "NINANC")
psd_data <- tibble(Group = psd_flag, data = psd_list)
psd_state_df <- unnest(psd_data)
psd_state_df_tidy <- psd_state_df %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    values_to = "Power"
  )

#########################################################
LBD_tidy_df_hemi <- process_data(LBD_p_mean, roi, "LBD")
LBD_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(LBD_tidy_df_hemi)

# Process PD data
PD_tidy_df_hemi <- process_data(
  PD_p_mean,
  roi,
  "PD"
)
PD_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(PD_tidy_df_hemi)

# Process NC data
NC_tidy_df_hemi <- process_data(NC_p_mean, roi, "NC")
NC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(NC_tidy_df_hemi)

# Process CC data
CC_tidy_df_hemi <- process_data(CC_p_mean, roi, "CC")
CC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(CC_tidy_df_hemi)

# Process NINA NC data
NINANC_tidy_df_hemi <- process_data(
  NINANC_p_mean,
  roi,
  "NINANC"
)
NINANC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(NINANC_tidy_df_hemi)

glasser_states_list <- list(
  LBD_glasser_states,
  PD_glasser_states,
  NC_glasser_states,
  CC_glasser_states,
  NINANC_glasser_states
)
glasser_states_flag <- c("LBD", "PD", "NC", "CC", "NINANC")
glasser_states_data <- tibble(
  Group = glasser_states_flag,
  data = glasser_states_list
)
glasser_states_df <- unnest(glasser_states_data)
glasser_states_df_tidy <- glasser_states_df %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    values_to = "Power"
  )

# Process aMCI_AD data
# aMCI_AD_tidy_df_hemi <- process_data(aMCI_AD_p_mean, roi, "aMCI_AD")
# aMCI_AD_glasser_states <- glasser %>%
#   as_tibble() %>%
#   left_join(aMCI_AD_tidy_df_hemi)

#############################################

atlas_df <- read_csv("./files/atlas_df.csv")
atlas_df$`ROI.Name` <- roi$ROI
atlas_df$`x.mni` <- as.integer(atlas_df$X)
atlas_df$`y.mni` <- as.integer(atlas_df$Y)
atlas_df$`z.mni` <- as.integer(atlas_df$Z)
atlas_df$network <- roi$ROI
atlas_df <- atlas_df |> select(`ROI.Name`, `x.mni`, `y.mni`, `z.mni`, network)
atlas_df$network <- gsub("_lh|_rh", "", atlas_df$network)
check_atlas(atlas_df)
##############################################

O_C_CC_power_diff <- np$load("./files/O_C_CC_power_diff.npy")

LBD_NC_CC_power_diff <- np$load("./files/LBD_NC_CC_power_diff.npy")
LBD_NCCC_power_diff <- np$load("./files/LBD_NCCC_power_diff.npy")
LBD_NCNINA_power_diff <- np$load("./files/LBD_NCNINA_power_diff.npy")

PD_NC_CC_power_diff <- np$load("./files/PD_NC_CC_power_diff.npy")
PD_NCCC_power_diff <- np$load("./files/PD_NCCC_power_diff.npy")
PD_NCNINA_power_diff <- np$load("./files/PD_NCNINA_power_diff.npy")

LBD_PD_power_diff <- np$load("./files/LBD_PD_power_diff.npy")

Open_Close_tidy_df_hemi <- process_data(
  O_C_CC_power_diff,
  roi,
  "Open_Close"
)
Open_Close_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(Open_Close_tidy_df_hemi)
LBD_NC_CC_tidy_df_hemi <- process_data(LBD_NC_CC_power_diff, roi, "LBD_NC")
LBD_NC_CC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(LBD_NC_CC_tidy_df_hemi)

LBD_NC_NCCC_tidy_df_hemi <- process_data(LBD_NCCC_power_diff, roi, "LBD_NCCC")
LBD_NC_NCCC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(LBD_NC_NCCC_tidy_df_hemi)

LBD_NC_NCNINA_tidy_df_hemi <- process_data(
  LBD_NCNINA_power_diff,
  roi,
  "LBD_NCNINA"
)
LBD_NC_NCNINA_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(LBD_NC_NCNINA_tidy_df_hemi)

PD_NC_CC_tidy_df_hemi <- process_data(PD_NC_CC_power_diff, roi, "PD_NC")
PD_NC_CC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(PD_NC_CC_tidy_df_hemi)

PD_NC_NCCC_tidy_df_hemi <- process_data(PD_NCCC_power_diff, roi, "PD_NCCC")
PD_NC_NCCC_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(PD_NC_NCCC_tidy_df_hemi)

PD_NC_NCNINA_tidy_df_hemi <- process_data(
  PD_NCNINA_power_diff,
  roi,
  "PD_NCNINA"
)
PD_NC_NCNINA_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(PD_NC_NCNINA_tidy_df_hemi)

LBD_PD_tidy_df_hemi <- process_data(LBD_PD_power_diff, roi, "LBD_PD")
LBD_PD_glasser_states <- glasser %>%
  as_tibble() %>%
  left_join(LBD_PD_tidy_df_hemi)

glasser_states_dif_list <- list(
  Open_Close_glasser_states,
  LBD_NC_CC_glasser_states,
  LBD_NC_NCCC_glasser_states,
  LBD_NC_NCNINA_glasser_states,
  PD_NC_CC_glasser_states,
  PD_NC_NCCC_glasser_states,
  PD_NC_NCNINA_glasser_states,
  LBD_PD_glasser_states
) #,CC_psd_df,NINANC_psd_df)
glasser_states_dif_flag <- c(
  "Open v.s. Close",
  "LBD v.s. NC",
  "LBD v.s. NC(CamCan)",
  "LBD v.s. NC(NINA)",
  "PD v.s. NC",
  "PD v.s. NC(CamCan)",
  "PD v.s. NC(NINA)",
  "LBD v.s. PD"
) #,"CC","NINANC")
glasser_states_data_dif <- tibble(
  Group = glasser_states_dif_flag,
  data = glasser_states_dif_list
)
glasser_states_dif_df <- unnest(glasser_states_data_dif)
glasser_states_df_dif_tidy <- glasser_states_dif_df %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    values_to = "Difference"
  )

fo_ID <- cbind(demo$original, demo$Group, (data.frame(fo)))
colnames(fo_ID) <- c(
  "ID",
  "Group",
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6"
)
lt_ID <- cbind(demo$original, demo$Group, (data.frame(lt)))
colnames(lt_ID) <- c(
  "ID",
  "Group",
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6"
)
intv_ID <- cbind(demo$original, demo$Group, (data.frame(intv)))
colnames(intv_ID) <- c(
  "ID",
  "Group",
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6"
)
sr_ID <- cbind(demo$original, demo$Group, (data.frame(sr)))
colnames(sr_ID) <- c(
  "ID",
  "Group",
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6"
)

fo_lt_intv_sr_list <- list(fo_ID, lt_ID, intv_ID, sr_ID) #,CC_psd_df,NINANC_psd_df)
fo_lt_intv_sr_flag <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

fo_lt_intv_sr_data <- tibble(
  Variable = fo_lt_intv_sr_flag,
  data = fo_lt_intv_sr_list
)
fo_lt_intv_sr_df <- unnest(fo_lt_intv_sr_data)
fo_lt_intv_sr_tidy <- fo_lt_intv_sr_df %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    values_to = "Measures"
  )

LBD_NC_compare <- fo_lt_intv_sr_tidy |>
  filter(Group == "NC" | Group == "LBD")
LBD_NC_CC_compare <- fo_lt_intv_sr_tidy |>
  filter((Group == "NC" & grepl("sub-", ID)) | Group == "LBD")
LBD_NC_NINA_compare <- fo_lt_intv_sr_tidy |>
  filter((Group == "NC" & grepl("NINA", ID)) | Group == "LBD")
PD_NC_compare <- fo_lt_intv_sr_tidy |>
  filter(Group == "NC" | Group == "PD")
PD_NC_CC_compare <- fo_lt_intv_sr_tidy |>
  filter((Group == "NC" & grepl("sub-", ID)) | Group == "PD")
PD_NC_NINA_compare <- fo_lt_intv_sr_tidy |>
  filter((Group == "NC" & grepl("NINA", ID)) | Group == "PD")

LBD_PD_compare <- fo_lt_intv_sr_tidy |> filter(Group == "LBD" | Group == "PD")

compare_list <- list(
  LBD_NC_compare,
  LBD_NC_CC_compare,
  LBD_NC_NINA_compare,
  PD_NC_compare,
  PD_NC_CC_compare,
  PD_NC_NINA_compare,
  LBD_PD_compare
) #,CC_psd_df,NINANC_psd_df)
compare_flag <- c(
  "LBD v.s. NC",
  "LBD v.s. NC(CamCan)",
  "LBD v.s. NC(NINA)",
  "PD v.s. NC",
  "PD v.s. NC(CamCan)",
  "PD v.s. NC(NINA)",
  "LBD v.s. PD"
)
compare_data <- tibble(comparison = compare_flag, data = compare_list)
compare_df <- unnest(compare_data)

LBD_NC_CC_fo_lt_intv_sr <- np$load("./files/LBD_NC_CC_fo_lt_intv_sr.npy")
LBD_NC_CC_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  LBD_NC_CC_fo_lt_intv_sr
)
LBD_NC_CC_fo_lt_intv_sr_sig_df <- data.frame(LBD_NC_CC_fo_lt_intv_sr_sig)

LBD_NCCC_fo_lt_intv_sr <- np$load("./files/LBD_NCCC_fo_lt_intv_sr.npy")
LBD_NCCC_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  LBD_NCCC_fo_lt_intv_sr
)
LBD_NCCC_fo_lt_intv_sr_sig_df <- data.frame(LBD_NCCC_fo_lt_intv_sr_sig)

LBD_NCNINA_fo_lt_intv_sr <- np$load("./files/LBD_NCNINA_fo_lt_intv_sr.npy")
LBD_NCNINA_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  LBD_NCNINA_fo_lt_intv_sr
)
LBD_NCNINA_fo_lt_intv_sr_sig_df <- data.frame(LBD_NCNINA_fo_lt_intv_sr_sig)

###
PD_NC_CC_fo_lt_intv_sr <- np$load("./files/PD_NC_CC_fo_lt_intv_sr.npy")
PD_NC_CC_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  PD_NC_CC_fo_lt_intv_sr
)
PD_NC_CC_fo_lt_intv_sr_sig_df <- data.frame(PD_NC_CC_fo_lt_intv_sr_sig)

PD_NCCC_fo_lt_intv_sr <- np$load("./files/PD_NCCC_fo_lt_intv_sr.npy")
PD_NCCC_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  PD_NCCC_fo_lt_intv_sr
)
PD_NCCC_fo_lt_intv_sr_sig_df <- data.frame(PD_NCCC_fo_lt_intv_sr_sig)

PD_NCNINA_fo_lt_intv_sr <- np$load("./files/PD_NCNINA_fo_lt_intv_sr.npy")
PD_NCNINA_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  PD_NCNINA_fo_lt_intv_sr
)
PD_NCNINA_fo_lt_intv_sr_sig_df <- data.frame(PD_NCNINA_fo_lt_intv_sr_sig)

LBD_PD_fo_lt_intv_sr <- np$load("./files/LBD_PD_fo_lt_intv_sr.npy")
LBD_PD_fo_lt_intv_sr_sig <- convert_pvals_to_significance(
  LBD_PD_fo_lt_intv_sr
)
LBD_PD_fo_lt_intv_sr_sig_df <- data.frame(LBD_PD_fo_lt_intv_sr_sig)
##

LBD_NC_CC_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

LBD_NCCC_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

LBD_NCNINA_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)
####
PD_NC_CC_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

PD_NCCC_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

PD_NCNINA_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

LBD_PD_fo_lt_intv_sr_sig_df$Variable <- c(
  "Fractional Occupancy",
  "Mean Life Time (s)",
  "Mean Interval (s)",
  "Switching Rate (Hz)"
)

colnames(LBD_NC_CC_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)
colnames(LBD_NCCC_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)

colnames(LBD_NCNINA_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)

###
colnames(PD_NC_CC_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)
colnames(PD_NCCC_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)

colnames(PD_NCNINA_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)

colnames(LBD_PD_fo_lt_intv_sr_sig_df) <- c(
  "State 1",
  "State 2",
  "State 3",
  "State 4",
  "State 5",
  "State 6",
  "Variable"
)
sig_list <- list(
  LBD_NC_CC_fo_lt_intv_sr_sig_df,
  LBD_NCCC_fo_lt_intv_sr_sig_df,
  LBD_NCNINA_fo_lt_intv_sr_sig_df,
  PD_NC_CC_fo_lt_intv_sr_sig_df,
  PD_NCCC_fo_lt_intv_sr_sig_df,
  PD_NCNINA_fo_lt_intv_sr_sig_df,
  LBD_PD_fo_lt_intv_sr_sig_df
) #,CC_psd_df,NINANC_psd_df)
sig_flag <- c(
  "LBD v.s. NC",
  "LBD v.s. NC(CamCan)",
  "LBD v.s. NC(NINA)",
  "PD v.s. NC",
  "PD v.s. NC(CamCan)",
  "PD v.s. NC(NINA)",
  "LBD v.s. PD"
) #,"CC","NINANC")
sig_data <- tibble(comparison = sig_flag, data = sig_list)
sig_df <- unnest(sig_data)
sig_tidy <- sig_df %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    values_to = "Significance"
  )

summary_data <- compare_df |> left_join(sig_tidy)
#demo4 <- demo %>% select(ID = session, Sex, CAF, MFS, ODF, MOCA, Age, Group)
#cog_data <- fo_lt_intv_sr_tidy %>% left_join(demo4)
#cog <- cog_data %>%
#select(Variable, State, Group, Measures, CAF, MFS, ODF, MOCA, Age, Sex)

LBD_NC_CC_Coh_diff <- np$load("./files/LBD_NC_CC_Coh_diff.npy")
LBD_NCCC_Coh_diff <- np$load("./files/LBD_NCCC_Coh_diff.npy")
LBD_NCNINA_Coh_diff <- np$load("./files/LBD_NCNINA_Coh_diff.npy")
LBD_PD_Coh_diff <- np$load("./files/LBD_PD_Coh_diff.npy")
PD_NC_CC_Coh_diff <- np$load("./files/PD_NC_CC_Coh_diff.npy")
PD_NCCC_Coh_diff <- np$load("./files/PD_NCCC_Coh_diff.npy")
PD_NCNINA_Coh_diff <- np$load("./files/PD_NCNINA_Coh_diff.npy")
