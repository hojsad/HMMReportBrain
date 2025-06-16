O_C_plot <- glasser_states_df_dif_tidy |> filter(Group == "Open v.s. Close") |> 
  power_diff_plot()
O_C_df <- glasser_states_df_dif_tidy |> filter(Group == "Open v.s. Close") |>
  select("ROI","hemi","State","Difference") |>
  mutate(
    Difference = if_else(Difference %in% sort(unique(Difference), decreasing = TRUE)[1:20], Difference, NA_real_)
  ) |> filter(Difference != "NA")|>  distinct()


group_plot <- glasser_states_df_tidy %>% power_plot2()
#a <- O_C_df %>%
  #dplyr::select(cyl:wt, mpg) %>% 
  #head() %>%
  #gt() %>%
  #gt_plt_bar(column = Difference, keep_column = TRUE, width = 35)
oc_table <- wrap_table(O_C_df,panel = "cols", space = "free") 
O_C_plot|oc_table
library(aplot)
O_C_plot %>% 
  insert_top(oc_table)

GroupStatePlot <- glasser_states_df_tidy %>% group_by(geometry,State) %>%
  mutate(
    #threshold = quantile(Power, 0.95, na.rm = TRUE),  # Calculate 75th percentile
    #Power = if_else(Power >= threshold, Power, NA_real_)  # Keep values in the top 25%
  Power = mean(Power, na.rm = TRUE)) %>% 
    #) %>%
  ungroup() %>% 
  group_by(State) %>%
 mutate(
threshold = quantile(Power, 0.90, na.rm = TRUE),
Power = if_else(Power >= threshold, Power, NA_real_)) %>% 
  select(-threshold) %>%  # Remove the temporary column
  ungroup() %>% power_plot()



power_plots <-  power_plot(glasser_states_df_tidy)
power_diff_plots <- power_diff_plot(glasser_states_df_dif_tidy)


sum_plots <- sum_plot(summary_data)
psd_plots <- psd_plot(psd_state_df_tidy)

power_diff_table <- glasser_states_df_dif_tidy %>% select(Group,ROI,hemi,State,Difference) %>%
  filter(Difference!= "NA" ) %>%  distinct()
stats_data <- summary_data |> select(!"ID")%>%
  select(!"Group") |> distinct()
write_csv(power_diff_table,"power_diff_table.csv")
write_csv(summary_data,"summary_data.csv")
write_csv(stats_data,"stats_data.csv")
ggsave("power_plots.png", power_plots ,device = "png" , dpi = 1200, width=24, height=12)
ggsave("power_diff_plots.png", power_diff_plots ,device = "png" , dpi = 1200, width=24, height=12)
ggsave("psd_plots.png", psd_plots ,device = "png" , dpi = 1200, width=12, height=4)
ggsave("sum_plots.png", sum_plots ,device = "png" , dpi = 1200, width=24, height=12)
ggsave("power_plots2.png",power_plots / GroupStatePlot ,device = "png" , dpi = 1200, width=24, height=12)

NADIFF <- glasser_states_df_dif_tidy %>% select(Group,ROI,hemi,State,Difference) %>%
  filter(Group == "LBD v.s. NC" )
NAID <- NADIFF %>%  left_join(roi)


ROI_dif_tidy <-  glasser_states_df_dif_tidy|> filter(Group == "LBD v.s. NC") |> 
  select(ROI,hemi,State,Difference) %>% 
  filter(Difference != "NA") 

psd_comparison <- ROI_dif_tidy %>% left_join(psd_tidy_test)
psd_state <- psd_comparison %>%  filter(State == "State 5") %>% 
  filter(Group == "LBD")


test <-  psd_state %>% select("Region","fr","value") %>% 
  group_by(fr) %>%
  summarize(mean_value = mean(value, na.rm = TRUE)) %>% 
  

test <- NC_psd_mean[3,,] %>% melt()
ggplot(test) +
  geom_line(aes(x= X2, y= value ))+
  xlab("Frequency (Hz)") + ylab("PSD (a.u.)") +
  facet_wrap(~X1)

a <-LBDPD_powerr_plot(1)|LBDPD_powerr_plot(2)|LBDPD_powerr_plot(3)|LBDPD_powerr_plot(4)|LBDPD_powerr_plot(5)|LBDPD_powerr_plot(6)
b <-LBDNC_powerr_plot(1)|LBDNC_powerr_plot(2)|LBDNC_powerr_plot(3)|LBDNC_powerr_plot(4)|LBDNC_powerr_plot(5)|LBDNC_powerr_plot(6)
power_diff_plots_LBD <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC"|Group == "LBD v.s. PD") %>%
  power_diff_plot()

power_diff_plots_LBD_NC <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot()

power_diff_plots_LBD_PD <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. PD") %>%
  power_diff_plot() 


panel1 <- (power_plots / GroupStatePlot/power_diff_plots_LBD_NC/power_diff_plots_LBD_PD)+
  plot_annotation(tag_levels = 'A',
                  caption = "Figure 1: (A) Six distinct brain states were identified through Hidden Markov Model analysis of MEG data, visualized on cortical surface maps. (B) Key regions of interest (ROIs) characterize each state: State 1 involves the Lateral Temporal Cortex,Anterior Cingulate and Medial Prefrontal Cortex, and Medial Temporal Cortex; State 2 includes the Primary and Early Visual Cortex,Dorsal Stream Visual Cortex, Ventral Stream Visual Cortex, and Intraparietal Sulcus & PGP; State 3 comprises the Insular & Frontoparietal  Operculum Cortex, Auditory Association Cortex, and Lateral Temporal Cortex; State 4 encompasses the Superior Somatosensory and Motor Cortex,Supplementary Motor Area, Inferior Somatosensory and Motor Cortex, and Premotor Cortex; State 5 involves the Ventral Stream Visual Cortex, Medial Bank of the Intraparietal Sulcus, Early Auditory Cortex, Temporal-Parieto-Occipital Junction, Auditory Association Cortex, and Inferior Parietal Cortex, reflecting Task-Positive/Negative Networks; and State 6 includes the Primary and Early Visual Cortex, Superior Somatosensory and Motor Cortex, and Dorsal Stream Visual Cortex. (C) Significant differences in regional activation between LBD and NC groups were observed, with State 1 showing increased activation in LBD across 20 regions, including bilateral dorsal stream visual cortex, posterior cingulate cortex, and inferior parietal regions (p<0.05), while State 5 exhibited decreased activation in LBD, particularly in the left early auditory cortex and auditory association cortex (p<0.05). (D) Differences between LBD and PD groups revealed that State 1 showed higher activation in LBD across five regions, including the right superior somatosensory/motor cortex and inferior parietal areas (p<0.05), whereas State 5 demonstrated reduced activation in LBD in four regions, including the left early auditory cortex and right inferior somatosensory/motor cortex (p<0.05)." ,
                  theme = theme(plot.caption = element_textbox_simple(size=14) ))

ggsave("fig1.png", panel1,device = "png" , dpi = 1200, width=20, height=12)
group_plot <- glasser_states_df_tidy %>% power_plot2()
layout <- "
A
A
B
"
panel111 <- (power_plots / group_plot)+
  #plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A',
                  caption = "Figure 1: (A) Six distinct brain states were identified through Hidden Markov Model analysis of MEG data, visualized on cortical surface maps. Key regions of interest (ROIs) characterize each state: State 1 involves the Lateral Temporal Cortex,Anterior Cingulate and Medial Prefrontal Cortex, and Medial Temporal Cortex; State 2 includes the Primary and Early Visual Cortex,Dorsal Stream Visual Cortex, Ventral Stream Visual Cortex, and Intraparietal Sulcus & PGP; State 3 comprises the Insular & Frontoparietal  Operculum Cortex, Auditory Association Cortex, and Lateral Temporal Cortex; State 4 encompasses the Superior Somatosensory and Motor Cortex,Supplementary Motor Area, Inferior Somatosensory and Motor Cortex, and Premotor Cortex; State 5 involves the Ventral Stream Visual Cortex, Medial Bank of the Intraparietal Sulcus, Early Auditory Cortex, Temporal-Parieto-Occipital Junction, Auditory Association Cortex, and Inferior Parietal Cortex, reflecting Task-Positive/Negative Networks; and State 6 includes the Primary and Early Visual Cortex, Superior Somatosensory and Motor Cortex, and Dorsal Stream Visual Cortex. (B) Power maps of different groups for each state." ,
                  theme = theme(plot.caption = element_textbox_simple(size = 12) ))

ggsave("fig111.png", panel111,device = "png" , dpi = 1200, width=12, height=10)


power_diff_plots_LBD_1 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 1")
power_diff_plots_LBD_2 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 2")
power_diff_plots_LBD_3 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 3")
power_diff_plots_LBD_4 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 4")
power_diff_plots_LBD_5 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 5")
power_diff_plots_LBD_6 <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC") %>% 
  power_diff_plot3("State 6")

LBDNC_state <- (power_diff_plots_LBD_1 | power_diff_plots_LBD_2 | power_diff_plots_LBD_3 | power_diff_plots_LBD_4 | power_diff_plots_LBD_5 |
  power_diff_plots_LBD_6)
power_diff_plots_LBD_PD <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. PD") %>%
  power_diff_plot() 

summary_diff_plot <- summary_data %>% 
  filter(comparison %in% c("LBD v.s. NC", "LBD v.s. PD")) %>%
  sum_plot()
summary_diff_plot_LBD_NC <- summary_data %>% 
  filter(comparison %in% c("LBD v.s. NC")) %>%
  sum_plot()
summary_diff_plot_LBD_PD <- summary_data %>% 
  filter(comparison %in% c("LBD v.s. PD")) %>%
  sum_plot2()

panel2 <- (summary_diff_plot_LBD_NC / summary_diff_plot_LBD_PD) +
  plot_annotation(tag_levels = 'A',
                  caption = "Figure 2: Group Differences in Dynamic Connectivity Metrics Between LBD, NC, and PD Groups. (A) Comparison of dynamic connectivity metrics between Lewy Body Dementia (LBD) and Normal Cognition (NC) groups, including Fractional Occupancy (FO), Mean Lifetime (MLT), Mean Interval (MI), and Switching Rate (SR). (B) Comparison of the same metrics between LBD and Parkinson’s Disease (PD) groups. Fractional Occupancy represents the proportion of time spent in a given state, Mean Lifetime reflects the average duration a state remains active, Mean Interval denotes the average time between successive visits to the same state, and Switching Rate quantifies the frequency of state transitions. Statistically significant differences between groups are indicated by asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001), while 'n.s.' denotes non-significant differences.",
                  theme = theme(plot.caption = element_textbox_simple(size = 14) ))
ggsave("fig2.png", panel2,device = "png" , dpi = 1200, width=14, height=18)
a <- LBDNC_power_plot()
b <-LBDPD_power_plot()
panel3 <- (psd_plots / a /b ) +
  plot_annotation(tag_levels = 'A',
                  caption = "Figure 3: Power Spectral Density (PSD) Analysis Across Brain States for LBD, PD, and NC Groups. (A) Average power spectral density (PSD) across all regions of interest (ROIs) for each brain state in the Lewy Body Dementia (LBD), Parkinson’s Disease (PD), and Normal Cognition (NC) groups. (B) Comparison of PSD between LBD and NC groups for ROIs that showed significant power differences, revealing a distinct peak in the alpha band for NC in State 5. (C) Comparison of PSD between LBD and PD groups for the same significantly different ROIs, showing a similar pattern as in (B), with a peak in the alpha band for NC in State 5.",
                  theme = theme(plot.caption = (plot.caption = element_textbox_simple(size =14) )))
ggsave("fig3.png", Panel3,device = "png" , dpi = 1200, width=15, height=15)
layout <- "
AB
CD
"
Panel33 <- (power_diff_plots_LBD_NC+power_diff_plots_LBD_PD + a  +b ) +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A',
                  caption = "Figure 3: Power Spectral Density (PSD) Analysis Across Brain States for LBD, PD, and NC Groups.(A) Significant differences in regional activation between LBD and NC groups were observed, with State 1 showing increased activation in LBD across 20 regions, including bilateral dorsal stream visual cortex, posterior cingulate cortex, and inferior parietal regions (p<0.05), while State 5 exhibited decreased activation in LBD, particularly in the left early auditory cortex and auditory association cortex (p<0.05). (B) Differences between LBD and PD groups revealed that State 1 showed higher activation in LBD across five regions, including the right superior somatosensory/motor cortex and inferior parietal areas (p<0.05), whereas State 5 demonstrated reduced activation in LBD in four regions, including the left early auditory cortex and right inferior somatosensory/motor cortex (p<0.05). (C) Comparison of PSD between LBD and NC groups for ROIs that showed significant power differences, revealing a distinct peak in the alpha band for NC in State 5. (D) Comparison of PSD between LBD and PD groups for the same significantly different ROIs, showing a similar pattern as in (C), with a peak in the alpha band for NC in State 5.",
                  theme = theme(plot.caption = (plot.caption = element_textbox_simple(size =14) )))
ggsave("fig33.png", Panel33,device = "png" , dpi = 1200, width=20, height=7)


layout <- "
AAAAAABBBBBBCCCCCCDDDDDDEEEEEEFFFFFF
AAAAAABBBBBBCCCCCCDDDDDDEEEEEEFFFFFF
AAAAAABBBBBBCCCCCCDDDDDDEEEEEEFFFFFF
AAAAAABBBBBBCCCCCCDDDDDDEEEEEEFFFFFF
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
"
panelLBDNC <- power_diff_plots_LBD_NC+ plot_layout(guides = 'collect'))+(summary_diff_plot_LBD_NC) + (b+ plot_layout(guides = 'collect')) +plot_layout(design = layout)
library(gtExtras)
(LBDNC_state) + summary_diff_plot_LBD_NC + LBDNC_power_plot()+plot_layout(design = layout)
power_diff_plots_LBD_PD + (summary_diff_plot_LBD_PD + LBDPD_power_plot()+plot_layout(design = layout))
power_diff_plots_LBD_NC +  summary_diff_plot_LBD_NC + LBDNC_power_plot()+plot_layout(design = layout,guides = 'collect')




"#000004FF" "#1B0C41FF" "#4A0C6BFF" "#781C6DFF" "#A52C60FF"
[6] "#CF4446FF" "#ED6925FF" "#FD9A1AFF" "#F7D03CFF" "#FCFFA4FF"

tfmat <- LBD_NC_C_power_diff != 0
tfmat <- t(tfmat)
NC <- NC_psd_mean[1,,] %>% melt()
LBD <- LBD_psd_mean[1,,] %>% melt()
c_list <- list(NC,LBD)
c_flag <- c("NC","LBD")#,"CC","NINANC")
c_data <- tibble(Group = c_flag, data = c_list)
c_df <- unnest(c_data)

unique_values <- unique(c_df$X1)
tf_map <- setNames(tfmat[,1], unique_values)
filtered_df <- c_df %>%
  filter(tf_map[X1]) 

car_summary <- filtered_df %>% #filter(Group=="NC") %>% 
  dplyr::group_by(Group) %>%
  
  dplyr::summarize(
    mean = mean(value),
    sd = sd(value),
    # must end up with list of data for each row in the input dataframe
    mpg_data = list(value),
    .groups = "drop"
  )


power_diff_plots_LBD <- glasser_states_df_dif_tidy %>% 
  filter(Group == "LBD v.s. NC"|Group == "LBD v.s. PD") %>%
  power_diff_plot()

<- power_plots / power_diff_plots_LBD
Panel2 <- summary_diff_plot_LBD_NC / summary_diff_plot_LBD_PD
Panel3 <- psd_plots / LBDNC_power_plot() / LBDPD_power_plot()


lbd_test <- glasser_states_df_tidy %>% filter(Group == "LBD") %>% 
filter(State == "State 5") %>% select(Power)
nc_test <- glasser_states_df_tidy %>% filter(Group == "PD") %>% 
  filter(State == "State 5") %>% select(Power)
mean(nc_test$Power - lbd_test$Power,na.rm = TRUE)

state_imp_roi <- glasser_states_df_tidy %>% group_by(geometry,State) %>%
  mutate(
    #threshold = quantile(Power, 0.95, na.rm = TRUE),  # Calculate 75th percentile
    #Power = if_else(Power >= threshold, Power, NA_real_)  # Keep values in the top 25%
    Power = mean(Power, na.rm = TRUE)) %>% 
  #) %>%
  ungroup() %>% 
  group_by(State) %>%
  mutate(
    threshold = quantile(Power, 0.90, na.rm = TRUE),
    Power = if_else(Power >= threshold, Power, NA_real_)) %>% 
  select(-threshold) %>%  # Remove the temporary column
  ungroup() %>% select(State, ROI,geometry, Power) %>% filter(Power != "NA") %>% 
  distinct()

fo <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% #filter(State == "State 1") %>% 
  select(State,Measures,CAF,MFS,ODF,MOCA) %>% ggpairs( mapping = aes(color = State))

sr <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% #filter(State == "State 1") %>% 
  select(State,Measures,CAF,MFS,ODF,MOCA) %>% ggpairs( mapping = aes(color = State))
library(Hmisc)
cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  # turn all three matrices (r, n, and P into a data frame)
  Mdf <- map(M, ~data.frame(.x))
  # return the three data frames in a list
  return(Mdf)
}
cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 2") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
  
 dif1 <- glasser_states_df_dif_tidy |> filter(Group == "LBD v.s. PD") |>
  select("ROI","hemi","State","Difference") |> filter(State == "State 5") |>
  mutate(
    Difference = if_else(Difference %in% sort(unique(Difference), decreasing = TRUE)[1:20], Difference, NA_real_)
  ) |> filter(Difference != "NA")|>  distinct()
 
 dif1 |> gt()
 
 
 demounique <- demo %>%
   distinct(ID, .keep_all = TRUE) |> select(Group,Sex, Age) |>
   filter(Group != "aMCI_AD")
 demounique |> 
   tbl_summary(by = Group, 
 statistic = list(
   all_continuous() ~ "{mean}  ({sd})",
   all_categorical() ~ "{n} / {N} ({p}%)"
 ),
 digits = all_continuous() ~ 2
 
 )

 lbd_test <- glasser_states_df_tidy %>% filter(Group == "LBD") %>% 
  filter(State == "State 5") %>% select(Power)
nc_test <- glasser_states_df_tidy %>% filter(Group == "PD") %>% 
  filter(State == "State 5") %>% select(Power)
mean(nc_test$Power - lbd_test$Power,na.rm = TRUE)


fo <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% #filter(State == "State 1") %>% 
  select(State,Measures,CAF,MFS,ODF,MOCA) %>% ggpairs( mapping = aes(color = State))

sr <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% #filter(State == "State 1") %>% 
  select(State,Measures,CAF,MFS,ODF,MOCA) %>% ggpairs( mapping = aes(color = State))
library(Hmisc)
cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  # turn all three matrices (r, n, and P into a data frame)
  Mdf <- map(M, ~data.frame(.x))
  # return the three data frames in a list
  return(Mdf)
}
focor1 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 1") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

focor2 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 2") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

focor3 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 3") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

focor4 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 4") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
focor5 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 5") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
focor6 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Fractional Occupancy") %>% filter(State == "State 6") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

focor <- list(focor1,focor2,focor3,focor4,focor5,focor6)
state <- c("State 1","State 2","State 3","State 4","State 5","State 6")
fo_cor <- tibble(State = state, data = focor)
fo_cor <- unnest(fo_cor)
ltcor1 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 1") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

ltcor2 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 2") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

ltcor3 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 3") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

ltcor4 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 4") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
ltcor5 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 5") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
ltcor6 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Life Time (s)") %>% filter(State == "State 6") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

ltcor <- list(ltcor1,ltcor2,ltcor3,ltcor4,ltcor5,ltcor6)
state <- c("State 1","State 2","State 3","State 4","State 5","State 6")
lt_cor <- tibble(State = state, data = ltcor)
lt_cor <- unnest(lt_cor)

intv1 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 1") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

intv2 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 2") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

intv3 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 3") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

intv4 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 4") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
intv5 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 5") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
intv6 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Mean Interval (s)") %>% filter(State == "State 6") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

intv <- list(intv1,intv2,intv3,intv4,intv5,intv6)
state <- c("State 1","State 2","State 3","State 4","State 5","State 6")
intv_cor <- tibble(State = state, data = intv)
intv_cor <- unnest(intv_cor)
sr1 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 1") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

sr2 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 2") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

sr3 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 3") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

sr4 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 4") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
sr5 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 5") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")
sr6 <- cog %>% filter(Group != "NC") %>% filter(Group != "aMCI_AD") %>% 
  filter(Variable == "Switching Rate (Hz)") %>% filter(State == "State 6") %>% 
  select( Measures,CAF,MFS,ODF,MOCA) %>% correlation_with_column("Measures")

sr <- list(sr1,sr2,sr3,sr4,sr5,sr6)
state <- c("State 1","State 2","State 3","State 4","State 5","State 6")
sr_cor <- tibble(State = state, data = sr)
sr_cor <- unnest(sr_cor)


state_imp_roi <- glasser_states_df_tidy %>% group_by(geometry,State) %>%
  mutate(
    #threshold = quantile(Power, 0.95, na.rm = TRUE),  # Calculate 75th percentile
    #Power = if_else(Power >= threshold, Power, NA_real_)  # Keep values in the top 25%
    Power = mean(Power, na.rm = TRUE)) %>% 
  #) %>%
  ungroup() %>% 
  group_by(State) %>%
  mutate(
    threshold = quantile(Power, 0.90, na.rm = TRUE),
    Power = if_else(Power >= threshold, Power, NA_real_)) %>% 
  select(-threshold) %>%  # Remove the temporary column
  ungroup() %>% select(State, ROI,geometry, Power) %>% filter(Power != "NA") %>% 
  distinct()
s1 <- state_imp_roi %>% filter(State == "State 1") %>% select(ROI) %>% distinct()
s2 <- state_imp_roi %>% filter(State == "State 2") %>% select(ROI) %>% distinct()
s3 <- state_imp_roi %>% filter(State == "State 3") %>% select(ROI) %>% distinct()
s4 <- state_imp_roi %>% filter(State == "State 4") %>% select(ROI) %>% distinct()
s5 <- state_imp_roi %>% filter(State == "State 5") %>% select(ROI) %>% distinct()
s6 <- state_imp_roi %>% filter(State == "State 6") %>% select(ROI) %>% distinct()

demo32gt %>% filter(study==1) %>% filter(Group != "aMCI_AD") %>% select(Age, Sex,Group) %>% 
  tbl_summary(
    by = Group,
    label = list(age = "Patient Age"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(age = c(0, 1))
  ) #%>% add_p() 



panel1new <- (power_plots )+
  plot_annotation(
                  caption = "Figure 1: Six distinct brain states were identified through Hidden Markov Model analysis of MEG data, visualized on cortical surface maps. Key regions of interest (ROIs) characterize each state: State 1 involves the Lateral Temporal Cortex,Anterior Cingulate and Medial Prefrontal Cortex, and Medial Temporal Cortex; State 2 includes the Primary and Early Visual Cortex,Dorsal Stream Visual Cortex, Ventral Stream Visual Cortex, and Intraparietal Sulcus & PGP; State 3 comprises the Insular & Frontoparietal  Operculum Cortex, Auditory Association Cortex, and Lateral Temporal Cortex; State 4 encompasses the Superior Somatosensory and Motor Cortex,Supplementary Motor Area, Inferior Somatosensory and Motor Cortex, and Premotor Cortex; State 5 involves the Ventral Stream Visual Cortex, Medial Bank of the Intraparietal Sulcus, Early Auditory Cortex, Temporal-Parieto-Occipital Junction, Auditory Association Cortex, and Inferior Parietal Cortex, reflecting Task-Positive/Negative Networks; and State 6 includes the Primary and Early Visual Cortex, Superior Somatosensory and Motor Cortex, and Dorsal Stream Visual Cortex.",
                  theme = theme(plot.caption = element_textbox_simple(size = 11) ))
ggsave("fig11.png", panel1new,device = "png" , dpi = 1200, width=16, height=4)




Panel1 <- (power_plots + theme(axis.text = element_text(size = 18)))+
  plot_annotation(#tag_levels = 'A',
                  caption ="**Figure 1.** Six distinct brain states were identified using hidden Markov model (HMM) analysis of MEG data, visualized as cortical surface maps. Each state is characterized by specific regional activations.
                  *State 1* engages the lateral temporal cortex, anterior cingulate, medial prefrontal cortex, and medial temporal cortex. *State 2* is associated with the primary and early visual cortex, dorsal and ventral stream visual areas, and the intraparietal sulcus. *State 3* includes the insular cortex, frontoparietal operculum, auditory association cortex, and lateral temporal cortex. *State 4* is primarily defined by activation in the superior and inferior somatosensory and motor cortices, supplementary motor area, and premotor cortex. *State 5* involves the ventral stream visual cortex, medial intraparietal sulcus, early auditory cortex, temporo-parieto-occipital junction, auditory association cortex, and inferior parietal cortex, corresponding to task-positive and task-negative network interactions. *State 6* includes the primary and early visual cortex, superior somatosensory and motor cortex, and dorsal stream visual cortex.", 
  theme = theme(plot.caption = element_textbox_simple(size=14)))

ggsave("figure1.png", Panel1,device = "png" , dpi = 600, width=8.27, height=11.69)
ggsave("figure11.png", Panel1,device = "png" , dpi = 1200, width=8.27, height=8.27)


Panel2 <- (summary_diff_plot_LBD_NC / summary_diff_plot_LBD_PD) +
  plot_annotation(tag_levels = 'A',
                  caption = "**Figure 2.** Group differences in dynamic connectivity metrics between Lewy body dementia (LBD), normal cognition (NC), and Parkinson’s disease (PD) cohorts. **(A)** Comparison of fractional occupancy (FO), mean lifetime (MLT), mean interval (MI), and switching rate (SR) between LBD and NC groups. **(B)** Comparison of the same metrics between LBD and PD  groups. FO represents the proportion of time spent in each state, MLT quantifies the average duration a state remains active, MI measures the time elapsed between recurring visits to the same state, and SR reflects the frequency of transitions between states. Statistically significant differences between groups are marked (* for *p* < 0.05,   ** for *p* < 0.01,  *** for *p* < 0.001), while *'n.s.'* denotes non-significant differences.",
                  theme = theme(plot.caption = element_textbox_simple(size = 14),
                                axis.text = element_text(size = 14)))
ggsave("figure2.png", Panel2,device = "png" , dpi = 1200, width=11, height=14)


Panel3 <- (psd_plots / a /b ) +
  plot_annotation(tag_levels = 'A',
                  caption = "**Figure 3.** Power spectral density (PSD) analysis of brain states across Lewy body dementia(LBD), Parkinson’s disease (PD), and normal cognition (NC) groups. **(A)** Average PSD across all regions of interest (ROIs) for each brain state, highlighting spectral differences between groups. **(B)** Comparison of PSD between LBD and NC groups for ROIs with significant power differences, revealing an alpha-band peak in NC during State 5. **(C)** PSD comparison between LBD and PD groups for the same ROIs, showing a similar trend, with NC and PD displaying alpha-band activity while LBD exhibits a shift towards theta-band activity.",
                  theme = theme(plot.caption = (plot.caption = element_textbox_simple(size =14) )))
ggsave("figure3.png", Panel3,device = "png" , dpi = 1200, width=9, height=12)
