library(tidyverse)
library(RAPTools)
library(RAPTAnalysis)

CSRPlotData <- CSR_DepthCalc(
  RangeFilePath = c(""),
  DataFilePath = c(""),
  RunNumber = "",
  StableIsotopeMass = 184,
  BinSize = 2,
  ChargeStates = c(1,2,3,4)
)

RunNumber = ""
#### Plot CSR ####
CSRPlot <- ggplot(CSRPlotData %>%
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

HydrogenIsotopesDepth <- HIsotope_DepthCalc(
  RangeFilePath = c(
    ""
  ), DataFilePath = c(
    ""
  ), BinSize = 2)

HPlot <- ggplot(HydrogenIsotopesDepth %>%
         pivot_longer(cols = -c(TotalNumberIons,AveDepth),
                      names_to = "IonType",
                      values_to = "Concentration"),
       aes(AveDepth, Concentration)
) +
  geom_point(aes(color = IonType),
             shape = 4,
             stroke = 1.5) +
  theme_classic() +
  theme(aspect.ratio = 1,
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylim(0,NA) +
  labs(title = paste0("H Isotope Concentrations - ",RunNumber),
       x = "Depth in Sample (nm)",
       y = "Concentration (ionic %)",
       color = "Ion Type") +
  scale_color_manual(values = c("H" = "#FF0079",
                                "H2"="#0698FF",
                                "H4"="#FFCD00",
                                "Deuterium+"="#9AFF00"))

EvapTimeData <- TimeBetweenEvap_DepthCalc(
  RangeFilePath = c(
    ""
  ),
  DataFilePath = c(
    ""
  ),
  BinSize = 2,
  PulseFreq = 250000,
  DetectionEfficiency = 0.80)

ggplot(EvapTimeData,
       aes(AveDepth, t_between_events)
) +
  geom_point(aes(),
             shape = 1,
             stroke = 1.5,
             color = "black") +
  theme_classic() +
  theme(aspect.ratio = 1,
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylim(0,NA) +
  labs(title = paste0("Time Between Evap Events - ",RunNumber),
       x = "Depth in Sample (nm)",
       y = "Time Between Evaporation Events (ms)")
