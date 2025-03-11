# IndusChemFate PBTK Model - R Implementation

This repository contains two R implementations of the IndusChemFate Physiologically Based ToxicoKinetic (PBTK) model, which was originally developed in Visual Basic for MS Excel by Frans Jongeneelen and Wil ten Berge.

## Overview

IndusChemFate is a multi-chemical PBTK model designed to estimate blood and urine concentrations of chemicals and their metabolites under various exposure scenarios. The model is capable of simulating:

- Multiple exposure routes (inhalation, dermal, and oral)
- Various human subjects (adult males, adult females, children with normal weight and obese) 
- Experimental animals (rat and mouse)
- Different activity levels (at rest and light work)
- Metabolism and elimination of parent compounds and metabolites

## Original Model

The original IndusChemFate model (version 2.0) was developed as an MS Excel application with Visual Basic code and is available as freeware from the CEFIC-LRI website.

**Original Authors:**
- Frans Jongeneelen, IndusTox Consult
- Wil ten Berge, Santoxar

**Original Publication:**
- Jongeneelen, F.J. and ten Berge, W.F. (2011). A Generic, Cross-Chemical Predictive PBTK Model with Multiple Entry Routes Running as Application in MS Excel; Design of the Model and Comparison of Predictions with Experimental Results. *Annals of Occupational Hygiene*, 55: 841-864.
- Jongeneelen, F.J. and ten Berge, W.F. (2011). Simulation of urinary excretion of 1-hydroxypyrene in various scenarios of exposure to PAH with a generic, cross-chemical predictive PBTK-model. *International Archives of Occupational and Environmental Health*, first online, November 2011.

## R Implementations

This repository provides two R implementations of the IndusChemFate model:

1. **Full Implementation (`Jaqpot_service.R`):** 
   - Complete implementation that supports up to 4 metabolites
   - Includes all 11 body compartments (adipose tissue, bone, brain, heart, kidney, intestine, liver, lung, muscle, skin, bone marrow)
   - Supports all exposure routes and physiological scenarios
   - Available at: [Jaqpot Full Model](https://app.jaqpot.org/dashboard/models/2039)

2. **Lite Implementation (`Lite_Jaqpot_service.R`):** 
   - Simplified version that supports only 1 metabolite
   - Reduces computational complexity while maintaining core functionality
   - Ideal for basic toxicokinetic modeling needs
   - Available at: [Jaqpot Lite Model](https://app.jaqpot.org/dashboard/models/2038/description)

**Key Difference:** The lite version significantly reduces the model's complexity by limiting metabolite tracking to a single metabolite, making it more computationally efficient while preserving the core physiologically based toxicokinetic modeling approach.

## Model Features

- **Physiological Parameters:** The model includes standardized physiological parameters for humans with normal weight and obesity, as well as experimental animals (rat and mouse).
- **Exposure Routes:** Inhalation, dermal, and oral exposure are all supported.
- **Metabolism:** Michaelis-Menten saturable metabolism can be modeled in any of the 11 body compartments or in liver only.
- **QSPRs:** The model incorporates published and in-house developed algorithms (Quantitative Structure-Property Relationships) for blood:air partitioning, tissue:blood partitioning, and renal excretion.
- **Dermal Absorption:** A novel dermal physiologically based module considers dermal deposition rate, duration of deposition, and evaporation during skin contact.

## Required Input Parameters

Key input parameters include:
- Physical-chemical properties (molecular weight, vapor pressure, water solubility, log Kow)
- Exposure concentrations and time patterns
- Metabolism parameters (Vmax and Km)
- Selection of human or animal subject
- Exercise level

## Limitations

- The model is intended as a first-tier or screening tool
- Predictions are typically accurate within an order of magnitude
- Simplified physiological processes compared to more specialized models

## Attribution

The original IndusChemFate model was developed by Frans Jongeneelen and Wil ten Berge. The R implementations in this repository are provided as adaptations of their original work.

Please cite the original authors when using this model:
```
Jongeneelen, F.J. and ten Berge, W.F. (2011). A Generic, Cross-Chemical Predictive PBTK Model with Multiple Entry Routes Running as Application in MS Excel; Design of the Model and Comparison of Predictions with Experimental Results. Annals of Occupational Hygiene, 55: 841-864.
```

This adaptation of the model follows the original freeware provisions which state:
- The program may be freely distributed provided no charge is levied
- The disclaimer text is always attached
- The program is open source and improvements can be added
- If improvements are made, the name IndusChemFate may not be used
- Mirroring or reproducing any part of the program without proper reference is prohibited
- The program may not be used for commercial purposes
