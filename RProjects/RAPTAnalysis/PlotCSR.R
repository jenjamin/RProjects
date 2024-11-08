library(tidyverse)
library(RAPTools)
library(RAPTAnalysis)

PlotData <- CSR_DepthCalc(
  RangeFilePath = c("C:\\Users\\gc9307\\OneDrive - UK Atomic Energy Authority\\Documents\\Data\\APT\\R5111_26931Initial.RRNG"),
  DataFilePath = c("C:\\Users\\gc9307\\AppData\\Local\\CAMECA\\IVAS\\OfflineModeData\\Reconstructions\\fe803129-919b-413e-8dc2-cfe2758f1773.apt"),
  RunNumber = "Run 26907",
  StableIsotopeMass = 184,
  BinSize = 2,
  ChargeStates = c(1,2,3,4)
)

RunNumber = "Run 26907"
#### Plot CSR ####
CSRPlot <- ggplot(PlotData %>%
                    mutate(CountsInBin = rowSums(across(-AveDepth)))  %>%
                    mutate(across(-c(AveDepth,CountsInBin), ~.x/CountsInBin, .names = "{col}")) %>%
                    pivot_longer(cols = ends_with("+"),
                                 names_to = "ChargeState",
                                 values_to = "Fraction"),
                  aes(AveDepth, Fraction)
) +
  geom_point(aes(color = ChargeState),
             shape = 4,
             stroke = 1.5) +
  theme_classic() +
  theme(aspect.ratio = 1,
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylim(0,1) +
  labs(title = paste0("Charge State Ratios - ",RunNumber),
       x = "Depth in Sample (nm)",
       y = "Relative Fraction",
       color = "W Charge State") +
  scale_colour_viridis_d()
