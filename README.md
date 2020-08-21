# Scripts, input and output data from _Simulation of O3 and NOX in Sao Paulo street urban canyons with VEIN (v0.2.2) and MUNICH (v1.0)_

 Here are the input and output data to create the results for the paper _Simulation of O3 and NOX in Sao Paulo street urban canyons with VEIN (v0.2.2) and MUNICH (v1.0)_ .

 ## Directories
 The information is organized in three folders:
 * 01_scripts: Scripts to create the Figures and maps of the paper, and to download the observations information from CETESB air quality stations (AQS).

 * 02_data:
   * bg_obs: The background information for MUNICH and observation to comparison from AQS.

   * emiss: Emission for Pinheiros neighbor and Paulista Avenue in &mu;g/km/h.

   * rmsp_shp: Sao Paulo city shape file.

   * tests: MUNICH results for different scenarios. `xN`, means the increment in all emissions by N. `bg_san` is an scenario where Santos AQS is used as background.
  `nox_x4`, scenario where only NOX emission are incremented by 4.

   * wrfinputs: WRF's domains.

   * wrf_met_aqs_station: WRF model output and meteorological observation from AQS.

 * 03_output:
   * paper_figs: Figures in the paper.

   * test_figs: Preliminary figures.

   * text_files: tables and performance statistics.
   

 ```bash
.
├── 01_scripts
├── 02_data
│   ├── bg_obs
│   ├── emiss
│   ├── rmsp_shp
│   ├── tests
│   │   ├── avpau_ugs
│   │   ├── av_pau_ugs_x2
│   │   ├── av_pau_ugs_x4
│   │   ├── pin_ugs
│   │   ├── pin_ugs_bg_san
│   │   ├── pin_ugs_nox_x4
│   │   ├── pin_ugs_x2
│   │   ├── pin_ugs_x3
│   │   └── pin_ugs_x4
│   ├── wrfinputs
│   └── wrf_met_aqs_station
└── 03_output
    ├── paper_figs
    ├── test_figs
    └── text_files

 ```
