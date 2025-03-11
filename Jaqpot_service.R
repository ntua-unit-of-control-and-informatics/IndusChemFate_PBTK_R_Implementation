# Required libraries
library(deSolve)  # For solving ODEs

# Organ 1 = "Adipose tissue"
# Organ 2 = "Bone"
# Organ 3 = "Brain"
# Organ 4 = "Heart"
# Organ 5 = "Kidney"
# Organ 6 = "Intestine"
# Organ 7 = "Liver"
# Organ 8 = "Lung"
# Organ 9 = "Muscle"
# Organ 10 = "Skin"
# Organ 11 = "BoneMarrow"

# Exposure Scenarios
# 1: Normal man (in rest)"
# 2: Obese man (in rest)"
# 3: Normal man (light work)"
# 4: Obese man (light work)"
# 5: Normal woman (in rest)"
# 6: Obese woman (in rest)"
# 7: Normal woman (light work)"
# 8: Obese woman (light work)"
# 9: Normal child (in rest)"
# 10: Obese Child (in rest)"
# 11: Normal child (playing)"
# 12: Obese child (playing)"
# 13: Mouse (experimental study)"
# 14: Rat (experimental study)"

#' Create model parameters
create.params <- function(user_input) {
  with(as.list(user_input),{
    # Base physiological parameters
    params <- list()
    params$SkinArea = SkinArea # user defined
    params$RespProt = RespProt
    params$DermProt = DermProt
    params$Temp = Temp # Skin temperature in Celsius degrees
    
    if(body_weight > 0){
      body_weight_provided = TRUE
      params$BodyWt = body_weight
    }else{
      body_weight_provided = FALSE
    }
    
    # Handle different scenarios (from the original Select Case)
    if(scenario == 1) {  # Normal man - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,70) # kg  normal man
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0832 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.184 * params$BodyWt # litre
      params$VolBone = 0.0589 * params$BodyWt # litre
      params$VolBrain = 0.0192 * params$BodyWt # litre
      params$VolHeart = 0.0051 * params$BodyWt # litre
      params$VolKidney = 0.0046 * params$BodyWt # litre
      params$VolIntest = 0.0222 * params$BodyWt # litre
      params$VolLiver = 0.0225 * params$BodyWt # litre
      params$VolLungs = 0.0144 * params$BodyWt # litre
      params$VolMuscle = 0.4576 * params$BodyWt # litre
      params$VolSkin = 0.0553 * params$BodyWt # litre
      params$VolMarrow = 0.0729 * params$BodyWt # litre
      params$TotSkin = 18000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 6 * params$BodyWt # 'litres per hour
      params$AlvVent = 7 * params$BodyWt # 'litres per hour
      params$FrAdip = 0.08226  # fraction cardiac output rest
      params$FrBone = 0.04139  # fraction cardiac output rest
      params$FrBrain = 0.09814 # fraction cardiac output rest
      params$FrHeart = 0.10536 # fraction cardiac output rest
      params$FrKidney = 0.16886  # fraction cardiac output rest
      params$FrLivVen = 0.152  # fraction cardiac output rest
      params$FrLivArt = 0.0381 # fraction cardiac output rest
      params$FrLung = 0.0234 #fraction cardiac output rest
      params$FrMuscle = 0.13711 # fraction cardiac output rest
      params$FrSkin = 0.06783  # fraction cardiac output rest
      params$FrMarrow = 0.08515  # fraction cardiac output rest
      
    } else if(scenario == 2){ # Obese man - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,80) # kg  obese man
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.06787 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.33458 * params$BodyWt # litre
      params$VolBone = 0.04805 * params$BodyWt # litre
      params$VolBrain = 0.01563 * params$BodyWt # litre
      params$VolHeart = 0.0042 * params$BodyWt # litre
      params$VolKidney = 0.00373 * params$BodyWt # litre
      params$VolIntest = 0.01808 * params$BodyWt # litre
      params$VolLiver = 0.01831 * params$BodyWt # litre
      params$VolLungs = 0.01178 * params$BodyWt # litre
      params$VolMuscle = 0.37318 * params$BodyWt # litre
      params$VolSkin = 0.04513 * params$BodyWt # litre
      params$VolMarrow = 0.05948 * params$BodyWt # litre
      params$TotSkin = 18000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 6 * params$BodyWt # litres per hour
      params$AlvVent = 7 * params$BodyWt # litres per hour
      params$FrAdip = 0.08226 # fraction cardiac output rest
      params$FrBone = 0.04139 # fraction cardiac output rest
      params$FrBrain = 0.09814 # fraction cardiac output rest
      params$FrHeart = 0.10536 # fraction cardiac output rest
      params$FrKidney = 0.16886 # fraction cardiac output rest
      params$FrLivVen = 0.152 # fraction cardiac output rest
      params$FrLivArt = 0.0381 # fraction cardiac output rest
      params$FrLung = 0.0234 # fraction cardiac output rest
      params$FrMuscle = 0.13711 # fraction cardiac output rest
      params$FrSkin = 0.06783 # fraction cardiac output rest
      params$FrMarrow = 0.08515 # fraction cardiac output rest
      
    }else if(scenario == 3){ # Normal man - Light work
      params$BodyWt = ifelse(body_weight_provided, body_weight,70) # kg  normal man
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0832 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.184 * params$BodyWt # litre
      params$VolBone = 0.0589 * params$BodyWt # litre
      params$VolBrain = 0.0192 * params$BodyWt # litre
      params$VolHeart = 0.0051 * params$BodyWt # litre
      params$VolKidney = 0.0046 * params$BodyWt # litre
      params$VolIntest = 0.0222 * params$BodyWt # litre
      params$VolLiver = 0.0225 * params$BodyWt # litre
      params$VolLungs = 0.0144 * params$BodyWt # litre
      params$VolMuscle = 0.4576 * params$BodyWt # litre
      params$VolSkin = 0.0553 * params$BodyWt # litre
      params$VolMarrow = 0.0729 * params$BodyWt # litre
      params$TotSkin = 18000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 9 * params$BodyWt # litres per hour
      params$AlvVent = 18 * params$BodyWt # litres per hour
      params$FrAdip = 0.06957 # fraction cardiac output rest
      params$FrBone = 0.035 # fraction cardiac output rest
      params$FrBrain = 0.08299 # fraction cardiac output rest
      params$FrHeart = 0.1057 # fraction cardiac output rest
      params$FrKidney = 0.14279 # fraction cardiac output rest
      params$FrLivVen = 0.12888 # fraction cardiac output rest
      params$FrLivArt = 0.03222 # fraction cardiac output rest
      params$FrLung = 0.01978 # fraction cardiac output rest
      params$FrMuscle = 0.25369 # fraction cardiac output rest
      params$FrSkin = 0.05736 # fraction cardiac output rest
      params$FrMarrow = 0.07201 # fraction cardiac output rest
      
    }else if(scenario == 4){ # Obese Man - Light work
      params$BodyWt = ifelse(body_weight_provided, body_weight,80) # kg  obese man
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.06787 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.33458 * params$BodyWt # litre
      params$VolBone = 0.04805 * params$BodyWt # litre
      params$VolBrain = 0.01563 * params$BodyWt # litre
      params$VolHeart = 0.0042 * params$BodyWt # litre
      params$VolKidney = 0.00373 * params$BodyWt # litre
      params$VolIntest = 0.01808 * params$BodyWt # litre
      params$VolLiver = 0.01831 * params$BodyWt # litre
      params$VolLungs = 0.01178 * params$BodyWt # litre
      params$VolMuscle = 0.37318 * params$BodyWt # litre
      params$VolSkin = 0.04513 * params$BodyWt # litre
      params$VolMarrow = 0.05948 * params$BodyWt # litre
      params$TotSkin = 18000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 9 * params$BodyWt # litres per hour
      params$AlvVent = 18 * params$BodyWt # litres per hour
      params$FrAdip = 0.06957 # fraction cardiac output rest
      params$FrBone = 0.035 # fraction cardiac output rest
      params$FrBrain = 0.08299 # fraction cardiac output rest
      params$FrHeart = 0.1057 # fraction cardiac output rest
      params$FrKidney = 0.14279 # fraction cardiac output rest
      params$FrLivVen = 0.12888 # fraction cardiac output rest
      params$FrLivArt = 0.03222 # fraction cardiac output rest
      params$FrLung = 0.01978 # fraction cardiac output rest
      params$FrMuscle = 0.25369 # fraction cardiac output rest
      params$FrSkin = 0.05736 # fraction cardiac output rest
      params$FrMarrow = 0.07201 # fraction cardiac output rest 
      
    }else if(scenario == 5){ # Normal Woman - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,60) # kg  normal woman
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0744 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.281 * params$BodyWt # litre
      params$VolBone = 0.0491 * params$BodyWt # litre
      params$VolBrain = 0.0216 * params$BodyWt # litre
      params$VolHeart = 0.0054 * params$BodyWt # litre
      params$VolKidney = 0.0047 * params$BodyWt # litre
      params$VolIntest = 0.026 * params$BodyWt # litre
      params$VolLiver = 0.0243 * params$BodyWt # litre
      params$VolLungs = 0.0156 * params$BodyWt # litre
      params$VolMuscle = 0.3774 * params$BodyWt # litre
      params$VolSkin = 0.06 * params$BodyWt # litre
      params$VolMarrow = 0.0606 * params$BodyWt # litre
      params$TotSkin = 16000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 5 * params$BodyWt # litres per hour
      params$AlvVent = 5 * params$BodyWt # litres per hour
      params$FrAdip = 0.1421 # fraction cardiac output rest
      params$FrBone = 0.02963 # fraction cardiac output rest
      params$FrBrain = 0.09523 # fraction cardiac output rest
      params$FrHeart = 0.14512 # fraction cardiac output rest
      params$FrKidney = 0.12849 # fraction cardiac output rest
      params$FrLivVen = 0.16326 # fraction cardiac output rest
      params$FrLivArt = 0.04081 # fraction cardiac output rest
      params$FrLung = 0.02407 # fraction cardiac output rest
      params$FrMuscle = 0.09523 # fraction cardiac output rest
      params$FrSkin = 0.07709 # fraction cardiac output rest
      params$FrMarrow = 0.05895 # fraction cardiac output rest
      
    }else if(scenario == 6){ # Obese woman - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,70) # kg  obese woman
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.05808 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.43872 * params$BodyWt # litre
      params$VolBone = 0.0383 * params$BodyWt # litre
      params$VolBrain = 0.01684 * params$BodyWt # litre
      params$VolHeart = 0.00421 * params$BodyWt # litre
      params$VolKidney = 0.00365 * params$BodyWt # litre
      params$VolIntest = 0.02029 * params$BodyWt # litre
      params$VolLiver = 0.01894 * params$BodyWt # litre
      params$VolLungs = 0.01221 * params$BodyWt # litre
      params$VolMuscle = 0.29463 * params$BodyWt # litre
      params$VolSkin = 0.04686 * params$BodyWt # litre
      params$VolMarrow = 0.04728 * params$BodyWt # litre
      params$TotSkin = 16000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 5 * params$BodyWt # litres per hour
      params$AlvVent = 5 * params$BodyWt # litres per hour
      params$FrAdip = 0.1421 # fraction cardiac output rest
      params$FrBone = 0.02963 # fraction cardiac output rest
      params$FrBrain = 0.09523 # fraction cardiac output rest
      params$FrHeart = 0.14512 # fraction cardiac output rest
      params$FrKidney = 0.12849 # fraction cardiac output rest
      params$FrLivVen = 0.16326 # fraction cardiac output rest
      params$FrLivArt = 0.04081 # fraction cardiac output rest
      params$FrLung = 0.02407 # fraction cardiac output rest
      params$FrMuscle = 0.09523 # fraction cardiac output rest
      params$FrSkin = 0.07709 # fraction cardiac output rest
      params$FrMarrow = 0.05895 # fraction cardiac output rest
      
    }else if(scenario == 7){ # Normal woman - Light work
      params$BodyWt = ifelse(body_weight_provided, body_weight,60) # kg  normal woman
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0744 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.281 * params$BodyWt # litre
      params$VolBone = 0.0491 * params$BodyWt # litre
      params$VolBrain = 0.0216 * params$BodyWt # litre
      params$VolHeart = 0.0054 * params$BodyWt # litre
      params$VolKidney = 0.0047 * params$BodyWt # litre
      params$VolIntest = 0.026 * params$BodyWt # litre
      params$VolLiver = 0.0243 * params$BodyWt # litre
      params$VolLungs = 0.0156 * params$BodyWt # litre
      params$VolMuscle = 0.3774 * params$BodyWt # litre
      params$VolSkin = 0.06 * params$BodyWt # litre
      params$VolMarrow = 0.0606 * params$BodyWt # litre
      params$TotSkin = 16000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 8 * params$BodyWt # litres per hour
      params$AlvVent = 13 * params$BodyWt # litres per hour
      params$FrAdip = 0.12046 # fraction cardiac output rest
      params$FrBone = 0.02512 # fraction cardiac output rest
      params$FrBrain = 0.08073 # fraction cardiac output rest
      params$FrHeart = 0.1441 # fraction cardiac output rest
      params$FrKidney = 0.10892 # fraction cardiac output rest
      params$FrLivVen = 0.1384 # fraction cardiac output rest
      params$FrLivArt = 0.0346 # fraction cardiac output rest
      params$FrLung = 0.02041 # fraction cardiac output rest
      params$FrMuscle = 0.21193 # fraction cardiac output rest
      params$FrSkin = 0.06535 # fraction cardiac output rest
      params$FrMarrow = 0.04998 # fraction cardiac output rest
      
    }else if(scenario == 8){ # Obese woman - Light work
      params$BodyWt = ifelse(body_weight_provided, body_weight,70) # kg  obese woman
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0832 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.184 * params$BodyWt # litre
      params$VolBone = 0.0589 * params$BodyWt # litre
      params$VolBrain = 0.0192 * params$BodyWt # litre
      params$VolHeart = 0.0051 * params$BodyWt # litre
      params$VolKidney = 0.0046 * params$BodyWt # litre
      params$VolIntest = 0.0222 * params$BodyWt # litre
      params$VolLiver = 0.0225 * params$BodyWt # litre
      params$VolLungs = 0.0144 * params$BodyWt # litre
      params$VolMuscle = 0.4576 * params$BodyWt # litre
      params$VolSkin = 0.0553 * params$BodyWt # litre
      params$VolMarrow = 0.0729 * params$BodyWt # litre
      params$TotSkin = 16000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 8 * params$BodyWt # litres per hour
      params$AlvVent = 13 * params$BodyWt # litres per hour
      params$FrAdip = 0.12046 # fraction cardiac output rest
      params$FrBone = 0.02512 # fraction cardiac output rest
      params$FrBrain = 0.08073 # fraction cardiac output rest
      params$FrHeart = 0.1441 # fraction cardiac output rest
      params$FrKidney = 0.10892 # fraction cardiac output rest
      params$FrLivVen = 0.1384 # fraction cardiac output rest
      params$FrLivArt = 0.0346 # fraction cardiac output rest
      params$FrLung = 0.02041 # fraction cardiac output rest
      params$FrMuscle = 0.21193 # fraction cardiac output rest
      params$FrSkin = 0.06535 # fraction cardiac output rest
      params$FrMarrow = 0.04998 # fraction cardiac output rest
      
    }else if(scenario == 9){ # Normal child - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,16) # kg  normal child
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.1014 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.2148 * params$BodyWt # litre
      params$VolBone = 0.0694 * params$BodyWt # litre
      params$VolBrain = 0.0965 * params$BodyWt # litre
      params$VolHeart = 0.0076 * params$BodyWt # litre
      params$VolKidney = 0.0062 * params$BodyWt # litre
      params$VolIntest = 0.0264 * params$BodyWt # litre
      params$VolLiver = 0.0347 * params$BodyWt # litre
      params$VolLungs = 0.0167 * params$BodyWt # litre
      params$VolMuscle = 0.3402 * params$BodyWt # litre
      params$VolSkin = 0.0403 * params$BodyWt # litre
      params$VolMarrow = 0.0458 * params$BodyWt # litre
      params$TotSkin = 6000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 12 * params$BodyWt # litres per hour
      params$AlvVent = 10 * params$BodyWt # litres per hour
      params$FrAdip = 0.05611 # fraction cardiac output rest
      params$FrBone = 0.0239 # fraction cardiac output rest
      params$FrBrain = 0.23763 # fraction cardiac output rest
      params$FrHeart = 0.27724 # fraction cardiac output rest
      params$FrKidney = 0.10891 # fraction cardiac output rest
      params$FrLivVen = 0.12146 # fraction cardiac output rest
      params$FrLivArt = 0.03036 # fraction cardiac output rest
      params$FrLung = 0.0239 # fraction cardiac output rest
      params$FrMuscle = 0.04951 # fraction cardiac output rest
      params$FrSkin = 0.02578 # fraction cardiac output rest
      params$FrMarrow = 0.04522 # fraction cardiac output rest
      
    }else if(scenario == 10){ # Obese child - Rest
      params$BodyWt = ifelse(body_weight_provided, body_weight,20) # kg  obese child
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.08022 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.37857 * params$BodyWt # litre
      params$VolBone = 0.05495 * params$BodyWt # litre
      params$VolBrain = 0.07637 * params$BodyWt # litre
      params$VolHeart = 0.00604 * params$BodyWt # litre
      params$VolKidney = 0.00495 * params$BodyWt # litre
      params$VolIntest = 0.02088 * params$BodyWt # litre
      params$VolLiver = 0.02747 * params$BodyWt # litre
      params$VolLungs = 0.01319 * params$BodyWt # litre
      params$VolMuscle = 0.26923 * params$BodyWt # litre
      params$VolSkin = 0.03187 * params$BodyWt # litre
      params$VolMarrow = 0.03626 * params$BodyWt # litre
      params$TotSkin = 6000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 12 * params$BodyWt # litres per hour
      params$AlvVent = 10 * params$BodyWt # litres per hour
      params$FrAdip = 0.05611 # fraction cardiac output rest
      params$FrBone = 0.0239 # fraction cardiac output rest
      params$FrBrain = 0.23763 # fraction cardiac output rest
      params$FrHeart = 0.27724 # fraction cardiac output rest
      params$FrKidney = 0.10891 # fraction cardiac output rest
      params$FrLivVen = 0.12146 # fraction cardiac output rest
      params$FrLivArt = 0.03036 # fraction cardiac output rest
      params$FrLung = 0.0239 # fraction cardiac output rest
      params$FrMuscle = 0.04951 # fraction cardiac output rest
      params$FrSkin = 0.02578 # fraction cardiac output rest
      params$FrMarrow = 0.04522 # fraction cardiac output rest
      
    }else if(scenario == 11){ # Normal child - Playing
      params$BodyWt = ifelse(body_weight_provided, body_weight,16) # kg  normal child
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.1014 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.2148 * params$BodyWt # litre
      params$VolBone = 0.0694 * params$BodyWt # litre
      params$VolBrain = 0.0965 * params$BodyWt # litre
      params$VolHeart = 0.0076 * params$BodyWt # litre
      params$VolKidney = 0.0062 * params$BodyWt # litre
      params$VolIntest = 0.0264 * params$BodyWt # litre
      params$VolLiver = 0.0347 * params$BodyWt # litre
      params$VolLungs = 0.0167 * params$BodyWt # litre
      params$VolMuscle = 0.3402 * params$BodyWt # litre
      params$VolSkin = 0.0403 * params$BodyWt # litre
      params$VolMarrow = 0.0458 * params$BodyWt # litre
      params$TotSkin = 6000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 15 * params$BodyWt # litres per hour
      params$AlvVent = 30 * params$BodyWt # litres per hour
      params$FrAdip = 0.04995 # fraction cardiac output rest
      params$FrBone = 0.02128 # fraction cardiac output rest
      params$FrBrain = 0.21155 # fraction cardiac output rest
      params$FrHeart = 0.26708 # fraction cardiac output rest
      params$FrKidney = 0.09696 # fraction cardiac output rest
      params$FrLivVen = 0.10813 # fraction cardiac output rest
      params$FrLivArt = 0.02703 # fraction cardiac output rest
      params$FrLung = 0.02128 # fraction cardiac output rest
      params$FrMuscle = 0.13354 # fraction cardiac output rest
      params$FrSkin = 0.02295 # fraction cardiac output rest
      params$FrMarrow = 0.04025 # fraction cardiac output rest
      
    }else if(scenario == 12){ # Obese child - Playing
      params$BodyWt = ifelse(body_weight_provided, body_weight,20) # kg  obese child
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.08022 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.37857 * params$BodyWt # litre
      params$VolBone = 0.05495 * params$BodyWt # litre
      params$VolBrain = 0.07637 * params$BodyWt # litre
      params$VolHeart = 0.00604 * params$BodyWt # litre
      params$VolKidney = 0.00495 * params$BodyWt # litre
      params$VolIntest = 0.02088 * params$BodyWt # litre
      params$VolLiver = 0.02747 * params$BodyWt # litre
      params$VolLungs = 0.01319 * params$BodyWt # litre
      params$VolMuscle = 0.26923 * params$BodyWt # litre
      params$VolSkin = 0.03187 * params$BodyWt # litre
      params$VolMarrow = 0.03626 * params$BodyWt # litre
      params$TotSkin = 6000 # body surface cm2
      params$SkinFac = 1 # ratio skin permeability
      params$UrinFlow = 0.0214 # litre/kgbw/day
      params$GlomFiltr = 0.08 # fraction of renal arterial flow
      
      params$CardOutp = 15 * params$BodyWt # litres per hour
      params$AlvVent = 30 * params$BodyWt # litres per hour
      params$FrAdip = 0.04995 # fraction cardiac output rest
      params$FrBone = 0.02128 # fraction cardiac output rest
      params$FrBrain = 0.21155 # fraction cardiac output rest
      params$FrHeart = 0.26708 # fraction cardiac output rest
      params$FrKidney = 0.09696 # fraction cardiac output rest
      params$FrLivVen = 0.10813 # fraction cardiac output rest
      params$FrLivArt = 0.02703 # fraction cardiac output rest
      params$FrLung = 0.02128 # fraction cardiac output rest
      params$FrMuscle = 0.13354 # fraction cardiac output rest
      params$FrSkin = 0.02295 # fraction cardiac output rest
      params$FrMarrow = 0.04025 # fraction cardiac output rest
      
    }else if(scenario == 13){ # Mouse experimental study
      params$BodyWt = ifelse(body_weight_provided, body_weight,0.03) # kg  average mouse
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.09 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.09 * params$BodyWt # litre
      params$VolBone = 0.07 * params$BodyWt # litre
      params$VolBrain = 0.016 * params$BodyWt # litre
      params$VolHeart = 0.005 * params$BodyWt # litre
      params$VolKidney = 0.017 * params$BodyWt # litre
      params$VolIntest = 0.041 * params$BodyWt # litre
      params$VolLiver = 0.05 * params$BodyWt # litre
      params$VolLungs = 0.007 * params$BodyWt # litre
      params$VolMuscle = 0.36 * params$BodyWt # litre
      params$VolSkin = 0.16 * params$BodyWt # litre
      params$VolMarrow = 0.03 * params$BodyWt # litre
      params$TotSkin = 100 # body surface cm2
      params$SkinFac = 3 # ratio skin permeability
      params$UrinFlow = 0.05 # litre/kgbw/day
      params$GlomFiltr = 0.16 # fraction of renal arterial flow
      
      params$CardOutp = 60 * params$BodyWt # litres per hour mouse
      params$AlvVent = 73 * params$BodyWt # litres per hour mouse
      params$FrAdip = 0.045 # fraction cardiac output rest
      params$FrBone = 0.056 # fraction cardiac output rest
      params$FrBrain = 0.039 # fraction cardiac output rest
      params$FrHeart = 0.079 # fraction cardiac output rest
      params$FrKidney = 0.202 # fraction cardiac output rest
      params$FrLivVen = 0.225 # fraction cardiac output rest
      params$FrLivArt = 0.056 # fraction cardiac output rest
      params$FrLung = 0.006 # fraction cardiac output rest
      params$FrMuscle = 0.18 # fraction cardiac output rest
      params$FrSkin = 0.067 # fraction cardiac output rest
      params$FrMarrow = 0.045 # fraction cardiac output rest
      
    }else if(scenario == 14){ # Rat experimental study
      params$BodyWt = ifelse(body_weight_provided, body_weight,0.3) # kg  average rat
      params$FrArt = 0.3 # fraction arterial blood
      params$FrVen = 0.7 # fraction venous blood
      params$VolBlood = 0.0963 * params$BodyWt # litre
      params$VolBlungArt = params$FrArt * params$VolBlood # volume arterial blood
      params$VolBlungVen = params$FrVen * params$VolBlood # volume venous blood
      params$VolAdip = 0.07 * params$BodyWt # litre
      params$VolBone = 0.06 * params$BodyWt # litre
      params$VolBrain = 0.006 * params$BodyWt # litre
      params$VolHeart = 0.0035 * params$BodyWt # litre
      params$VolKidney = 0.007 * params$BodyWt # litre
      params$VolIntest = 0.027 * params$BodyWt # litre
      params$VolLiver = 0.04 * params$BodyWt # litre
      params$VolLungs = 0.005 * params$BodyWt # litre
      params$VolMuscle = 0.4 * params$BodyWt # litre
      params$VolSkin = 0.2 * params$BodyWt # litre
      params$VolMarrow = 0.03 * params$BodyWt # litre
      params$TotSkin = 450 # body surface cm2
      params$SkinFac = 3 # ratio skin permeability
      params$UrinFlow = 0.2 # litre/kgbw/day
      params$GlomFiltr = 0.16 # fraction of renal arterial flow
      
      # Rat parameters
      params$CardOutp = 22 * params$BodyWt # litres per hour rat
      params$AlvVent = 35 * params$BodyWt # litres per hour rat
      params$FrAdip = 0.0454 # fraction cardiac output rest
      params$FrBone = 0.0454 # fraction cardiac output rest
      params$FrBrain = 0.0227 # fraction cardiac output rest
      params$FrHeart = 0.0567 # fraction cardiac output rest
      params$FrKidney = 0.1701 # fraction cardiac output rest
      params$FrLivVen = 0.1927 # fraction cardiac output rest
      params$FrLivArt = 0.0272 # fraction cardiac output rest
      params$FrLung = 0.0227 # fraction cardiac output rest
      params$FrMuscle = 0.3152 # fraction cardiac output rest
      params$FrSkin = 0.068 # fraction cardiac output rest
      params$FrMarrow = 0.034 # fraction cardiac output rest
    }
    
    
    params$VolCmpAdip = params$VolAdip #  'litre
    params$VolCmpBone = params$VolBone #  'litre
    params$VolCmpBrain = params$VolBrain #  'litre
    params$VolCmpHeart = params$VolHeart #  'litre
    params$VolCmpKidney = params$VolKidney #  'litre
    params$VolCmpIntestine = params$VolIntest #  'litre
    params$VolCmpLiver = params$VolLiver #  'litre
    params$VolCmpLung = params$VolLungs #  'litre
    params$VolCmpMuscle = params$VolMuscle #  'litre
    params$VolCmpSkin = params$VolSkin #  'litre
    params$VolCmpMarrow = params$VolMarrow #  'litre
    
    params$FlowOrgAdip = params$FrAdip * params$CardOutp #  'litres/hour
    params$FlowOrgBone = params$FrBone * params$CardOutp #  'litres/hour
    params$FlowOrgBrain = params$FrBrain * params$CardOutp #  'litres/hour
    params$FlowOrgHeart = params$FrHeart * params$CardOutp #  'litres/hour
    params$FlowOrgKidney = params$FrKidney * params$CardOutp #  'litres/hour
    params$FlowOrgLivVen = params$FrLivVen * params$CardOutp #  'litres/hour
    params$FlowOrgIntestine = params$FlowOrgLivVen
    params$FlowOrgLivArt = params$FrLivArt * params$CardOutp #  'litres/hour
    params$FlowOrgLiver = (params$FlowOrgIntestine +   params$FlowOrgLivArt)
    params$FlowOrgLung = params$FrLung * params$CardOutp #  'litres/hour
    params$FlowOrgMuscle = params$FrMuscle * params$CardOutp #  'litres/hour
    params$FlowOrgSkin = params$FrSkin * params$CardOutp #  'litres/hour
    params$FlowOrgMarrow = params$FrMarrow * params$CardOutp #   'litres/hour
    
    # Extract chemical properties
    # Parent Substance
    # Parent substance (index 0)
    mw_0 <- mw_0 # molecular weight
    params$mw_0 = mw_0
    vpPa_0 <- vpPa_0 # vapour pressure in Pascal
    lkow_0 <- lkow_0 # log(Kow) in blood at pH 7.4
    wsol_0 <- wsol_0 # water solubility in mg/litres
    params$wsol_0 = wsol_0
    params$dens_0 <- dens_0 # density in mg/cm3 or g/litre needed for dermal absorption
    FracHpRt_0 = FracHpRt_0 # enterohepatic circulation relative to blood
    # Only for parent compound
    DermLKow = DermLKow # log(Kow) in skin at pH 5.5
    DermKow = 10 ^ DermLKow
    params$DecrBolusRt = DecrBolusRt # absorption rate into intestinal tissue
    
    
    # First metabolite (index 1)
    mw_1 <- mw_1 # molecular weight
    vpPa_1 <- vpPa_1 # vapour pressure in Pascal
    lkow_1 <- lkow_1 # log(Kow) in blood at pH 7.4
    wsol_1 <- wsol_1 # water solubility in mg/litres
    dens_1 <- dens_1 # density in mg/cm3 or g/litre needed for dermal absorption
    
    # Second metabolite (index 2)
    mw_2 <- mw_2 # molecular weight
    vpPa_2 <- vpPa_2 # vapour pressure in Pascal
    lkow_2 <- lkow_2 # log(Kow) in blood at pH 7.4
    wsol_2 <- wsol_2 # water solubility in mg/litres
    dens_2 <- dens_2 # density in mg/cm3 or g/litre needed for dermal absorption
    
    # Third metabolite (index 3)
    mw_3 <- mw_3 # molecular weight
    vpPa_3 <- vpPa_3 # vapour pressure in Pascal
    lkow_3 <- lkow_3 # log(Kow) in blood at pH 7.4
    wsol_3 <- wsol_3 # water solubility in mg/litres
    dens_3 <- dens_3 # density in mg/cm3 or g/litre needed for dermal absorption
    
    # Fourth metabolite (index 4)
    mw_4 <- mw_4 # molecular weight
    vpPa_4 <- vpPa_4 # vapour pressure in Pascal
    lkow_4 <- lkow_4 # log(Kow) in blood at pH 7.4
    wsol_4 <- wsol_4 # water solubility in mg/litres
    dens_4 <- dens_4 # density in mg/cm3 or g/litre needed for dermal absorption
    
    # Define the organs and indices
    organs <- c("Adip", "Bone", "Brain", "Heart", "Kidney", "Intestine", 
                "Liver", "Lung", "Muscle", "Skin", "Marrow")
    
    # Assign all Vmax and Km parameters from user_input to params
    for(organ in organs) {
      for(i in 0:4) {
        # Create parameter names
        vmax_name <- paste0("Vmax_", organ, "_", i)
        Vmax_p_name <- paste0("Vmax_p_", organ, "_", i)
        km_name <- paste0("Km_", organ, "_", i)
        
        # Assign values from user_input to params
        params[[vmax_name]] <- user_input[[vmax_name]]
        params[[Vmax_p_name]] <- user_input[[Vmax_p_name]]
        params[[km_name]] <- user_input[[km_name]]
      }
    }
    
    # Henry coefficient
    # Helper function to calculate Henry coefficient
    calculate.lhen <- function(vpPa, mw, wsol, temp){
      lhen <- log10(vpPa * mw / wsol / 8.314 / (temp + 273.15))
      return(lhen)
    }
    
    # Log Octanol:Air partition coefficient
    # Helper function for LKoair calculation
    calculate.koair <- function(lkow, lhen){
      lkoair <- lkow - lhen
      return(lkoair)
    }
    
    # ratio concentration blood/air via QSAR
    # Helper function to calculate blood/air partition coefficient
    calculate.rcba <- function(lhen, lkoair, vpPa, scenario) {
      if(scenario %in% c(1:12)){
        if (lhen < -1) {
          rcba <- 0.4445 * (1 / 10^lhen) + 0.005189 * 10^lkoair
        } else {
          if (vpPa > 4000) {
            rcba <- 0.8417 * (1 / 10^lhen) + 0.006232 * 10^lkoair
          } else {
            rcba <- 0.4445 * (1 / 10^lhen) + 0.005189 * 10^lkoair
          }
        }  
      }else if(scenario %in% c(13,14)){
        if (lhen < -0.7){
          rcba <- 0.1933 * (1 / 10^lhen) + 0.002592 *10^lkoair
        }else{
          rcba <- 2.32245 * (1 / 10^lhen) + 0.0067389 *10^lkoair
        }
      }
      return(rcba)
    }
    
    # Tissue:Blood partition coefficients
    # Helper function to calculate tissue: blood partition coefficient
    calculate.rtisbl <- function(scenario, lkow){
      if(scenario %in% c(1:12)){
        kow = 10^lkow
        if(kow < 0.1){
          kow = 0.1
        }
        R_Adip_ven =  (0.8 * kow ^ 1.03 + 0.2) / (0.0056 * kow ^ 1.03 + 0.83) - 0.38  #ratio adipose/blood tissue
        if(R_Adip_ven < 0.1){
          R_Adip_ven = 0.1
        }
        
        R_Bone_ven = (0.031 * kow ^ 0.81 + 0.792) / (0.0056 * kow ^ 0.81 + 0.83) - 0.22 # ratio bone/blood
        R_Brain_ven = (0.133 * kow ^ 0.48 + 0.775) / (0.0056 * kow ^ 0.48 + 0.83) - 0.21 # ratio brain/blood
        R_Heart_ven = (0.031 * kow ^ 0.81 + 0.792) / (0.0056 * kow ^ 0.81 + 0.83) - 0.22 # ratio heart/blood
        R_Kidney_ven = (0.053 * kow ^ 0.57 + 0.785) / (0.0056 * kow ^ 0.57 + 0.83) - 0.19 # ratio kidney/blood
        R_Intestine_ven = (0.049 * kow ^ 0.81 + 0.711) / (0.0056 * kow ^ 0.81 + 0.83) - 0.35 # ratio intestine/blood
        R_Liver_ven = (0.049 * kow ^ 0.81 + 0.711) / (0.0056 * kow ^ 0.81 + 0.83) - 0.35 # ratio liver/blood
        R_Lung_ven = (0.031 * kow ^ 0.81 + 0.792) / (0.0056 * kow ^ 0.81 + 0.83) - 0.22 # ratio lung/blood
        R_Muscle_ven = (0.031 * kow ^ 0.81 + 0.792) / (0.0056 * kow ^ 0.81 + 0.83) - 0.22 # ratio muscle/blood
        R_Skin_ven = (0.031 * kow ^ 0.81 + 0.792) / (0.0056 * kow ^ 0.81 + 0.83) - 0.22 # ratio skin/blood
        R_Marrow_ven = (0.133 * kow ^ 0.48 + 0.775) / (0.0056 * kow ^ 0.48 + 0.83) - 0.21 # ratio marrow/blood  
      }else {
        Dlkow = lkow
        if(Dlkow < -1){
          Dlkow = -1
        }else if(Dlkow > 5){
          Dlkow = 5
        }
        R_Adip_ven = 10 ^ (1.106 * Dlkow - 0.1253 * Dlkow ^ 2 - 0.451) #ratio adipose/blood tissue
        if(R_Adip_ven < 0.05){
          R_Adip_ven = 0.05
        }
        R_Bone_ven = 10 ^ (0.05211 * Dlkow - 0.2239) #ratio bone/blood
        R_Brain_ven = 10 ^ (0.1207 * Dlkow - 0.0453) #ratio brain/blood
        R_Heart_ven = 10 ^ (0.05211 * Dlkow - 0.2239)    #ratio heart/blood
        R_Kidney_ven = 10 ^ (0.3125 * Dlkow - 0.1784) #ratio kidney/blood
        if(R_Kidney_ven < 0.5){
          R_Kidney_ven = 0.5
        }
        R_Intestine_ven = 10 ^ (0.15094 * Dlkow - 0.1111) #ratio intestine/blood
        R_Liver_ven = 10 ^ (0.15094 * Dlkow - 0.1111)    #ratio liver/blood
        R_Lung_ven = 10 ^ (0.05211 * Dlkow - 0.2239)    #ratio lung/blood
        R_Muscle_ven = 10 ^ (0.05211 * Dlkow - 0.2239)    #ratio muscle/blood
        R_Skin_ven = 10 ^ (0.05211 * Dlkow - 0.2239)   #ratio skin/blood
        R_Marrow_ven = 10 ^ (0.1207 * Dlkow - 0.0453)   #ratio marrow/blood
      }
      return(c(R_Adip_ven, R_Bone_ven, R_Brain_ven, R_Heart_ven, R_Kidney_ven, R_Intestine_ven, R_Liver_ven, R_Lung_ven, R_Muscle_ven, R_Skin_ven, R_Marrow_ven))
    }
    
    
    # Create arrays to store parameters for all potential substances (0-4)
    LHen <- rep(0, 5)
    LKoair <- rep(0, 5) 
    RCba <- rep(1, 5)
    
    # Lists to store tissue:blood partition ratios
    R_tissues_ven <- vector("list", 5)
    for(i in 1:5) {
      R_tissues_ven[[i]] <- rep(1, 11)  # 11 tissues
    }
    
    # Calculate parameters only for substances that are included (0 to N_subs-1)
    for(i in 0:(N_subs-1)) {
      # Get the property variable names for this substance
      mw_var <- paste0("mw_", i)
      vpPa_var <- paste0("vpPa_", i)
      lkow_var <- paste0("lkow_", i)
      wsol_var <- paste0("wsol_", i)
      
      # Extract the property values
      mw <- get(mw_var)
      vpPa <- get(vpPa_var)
      lkow <- get(lkow_var)
      wsol <- get(wsol_var)
      
      # Calculate parameters
      LHen[i+1] <- calculate.lhen(vpPa, mw, wsol, params$Temp)
      LKoair[i+1] <- calculate.koair(lkow, LHen[i+1])
      RCba[i+1] <- calculate.rcba(LHen[i+1], LKoair[i+1], vpPa, scenario)
      R_tissues_ven[[i+1]] <- calculate.rtisbl(scenario, lkow)
    }
    
    # Assign calculated values to parameters
    params$LHen_0 <- LHen[1]
    params$LHen_1 <- LHen[2]
    params$LHen_2 <- LHen[3]
    params$LHen_3 <- LHen[4]
    params$LHen_4 <- LHen[5]
    
    params$LKoair_0 <- LKoair[1]
    params$LKoair_1 <- LKoair[2]
    params$LKoair_2 <- LKoair[3]
    params$LKoair_3 <- LKoair[4]
    params$LKoair_4 <- LKoair[5]
    
    params$RCba_0 <- RCba[1]
    params$RCba_1 <- RCba[2]
    params$RCba_2 <- RCba[3]
    params$RCba_3 <- RCba[4]
    params$RCba_4 <- RCba[5]
    
    # Tissue partition coefficients for each substance and tissue
    organs <- c("Adip", "Bone", "Brain", "Heart", "Kidney", 
                "Intestine", "Liver", "Lung", "Muscle", "Skin", "Marrow")
    
    for(j in 1:11) {
      for(i in 0:4) {
        # Create parameter name like R_Adip_ven_0, R_Bone_ven_2, etc.
        param_name <- paste0("R_", organs[j], "_ven_", i)
        
        # Assign value - use calculated value for included substances, 1 for others
        if(i < N_subs) {
          params[[param_name]] <- R_tissues_ven[[i+1]][j]
        } else {
          params[[param_name]] <- 1  # Set to 1 to avoid division by zero
        }
      }
    }
    
    # Calculate fraction in water phase for included substances
    for(i in 0:4) {
      if(i < N_subs) {
        lkow_var <- paste0("lkow_", i)
        lkow <- get(lkow_var)
        params[[paste0("FrWsol_", i)]] <- 0.993 / (0.993 + 0.007 * 10 ^ lkow)
      } else {
        params[[paste0("FrWsol_", i)]] <- 0  # Not used in denominators, can be 0
      }
    }
    
    # Calculate renal removal for included substances
    for(i in 0:4) {
      if(i < N_subs) {
        # Get the substance's log Kow value
        lkow_var <- paste0("lkow_", i)
        lkow <- get(lkow_var)
        
        # Check if a specific resorption value is provided (overriding the calculation)
        resorpt_var <- paste0("Resorpt_", i)
        resorpt_provided <- FALSE
        resorpt_var_value <- get(resorpt_var)
        if(resorpt_var_value != -100) {
          # If it's a numeric value (not NA), use it
          if(!is.na(as.numeric(resorpt_var_value))) {
            resorpt <- as.numeric(resorpt_var_value)
            resorpt_provided <- TRUE
          }
        }
        
        # If no specific value is provided or it's not a number, calculate using the sigmoid function
        if(!resorpt_provided) {
          # Calculate tubular resorption using normal distribution with mean=-0.5, sd=0.5
          resorpt <- pnorm(lkow, mean=-0.5, sd=0.5)
        }
        
        # Set removal fraction (1 - resorption)
        params[[paste0("RemovKdn_", i)]] <- 1 - resorpt
        
        # Apply bounds as in the VB code
        if(params[[paste0("RemovKdn_", i)]] < 0.01) params[[paste0("RemovKdn_", i)]] <- 0.01
        if(params[[paste0("RemovKdn_", i)]] > 0.99) params[[paste0("RemovKdn_", i)]] <- 0.99
      } else {
        params[[paste0("RemovKdn_", i)]] <- 0  # Not used in denominators, can be 0
      }
    }
    
    # Calculate enterohepatic circulation for included substances
    for(i in 0:4) {
      if(i < N_subs) {
        frachprt_var <- paste0("FracHpRt_", i)
        frachprt <- get(frachprt_var)
        liver_ven_var <- paste0("R_Liver_ven_", i)
        
        params[[paste0("EntHepRt_", i)]] <- (params$FlowOrgLivArt + params$FlowOrgLivVen) * 
          frachprt / 
          (params$VolLiver * params[[liver_ven_var]])
      } else {
        params[[paste0("EntHepRt_", i)]] <- 0  # Not used in denominators, can be 0
      }
    }
    
    params$LKPSC = -2.59 + 0.7318 * DermLKow - 0.006832 * mw_0
    params$KPSC = 10 ^ params$LKPSC # cm/hour  intercellular route through lipid matrix of stratum corneum
    params$KPOL = 0.043 / mw_0 ^ 1.361 # cm/hour  transcellular routes through protein matrix of stratum corneum
    params$Kpw0 = params$SkinFac * (params$KPSC + params$KPOL) # cm/hour  combined permeation coefficient of the inter- and trans-cellular route of SC
    params$MassLoad = 0.0004 * params$dens_0 # Amount in milligrams/cm2
    params$Dsco = 0.002 # cm
    params$Depi = 0.008 # cm
    params$Fpart = max(0.2, 0.72 * DermKow ^ 0.43)   # Bunge SC partition data wet skin with corrected LKow
    params$MaxCap = params$Fpart * params$Dsco * wsol_0 / 1000 # Amount in milligrams/cm2
    if(params$MaxCap > params$MassLoad){
      params$MaxCap = params$MassLoad
    }
    params$Tlag = params$Dsco * params$Fpart / params$Kpw0 / 6 # lag time in hours
    params$DermCm2 = params$Kpw0 * wsol_0 / 1000 # mg/cm2/hour = maximum flux
    params$Kwa = 8.314 * (params$Temp + 273) * wsol_0 / (mw_0 * vpPa_0)  # ratio water/air
    
    # Evaporation liquid according to TGD 2e Ed
    params$Vw = 1080 # m/h
    params$Kva = 0.054396 # m2/h
    params$Le = 0.1
    params$Dair = 0.06 * sqrt(76 / mw_0) # m2/h
    params$Beta = 0.0111 * (params$Vw ^ 0.96) * (params$Dair ^ 0.19) / (params$Kva ^ 0.15) / (params$Le ^ 0.04) # meter/hour
    params$EvapL = params$Beta * mw_0 * vpPa_0 / (8.314 * (params$Temp + 273) * 10) # mg/cm2/hour
    
    # evaporation substance from dry stratum corneum
    params$Kair = 120 * sqrt(76 / mw_0) # cm/hour stagnant air layer thickness 3 cm close to skin
    params$Kevapw = 1 / (1 / params$Kpw0 + params$Kwa / params$Kair) # cm/hour
    
    if(scenario == 1) {  # Normal man at rest
      params$Kair <- 36 * sqrt(76 / mw_0)  # 10 cm stagnant air layer
    } else {
      params$Kair <- 120 * sqrt(76 / mw_0)  # 3 cm stagnant air layer
    }
    
    params$Kpa0 = 1 / (1 / (params$Kpw0 * params$Kwa) + 1 / params$Kair) # cm/hour permeation coefficient from ambient air vehicle
    
    # Assign exposure parameters
    params$Cexp = Cexp
    params$Cexp_times = Cexp_times
    params$OralDose = OralDose
    params$OralDose_times = OralDose_times
    params$DeposRate = DeposRate # dermal deposition rate
    params$DeposRate_times =DeposRate_times # dermal deposition rate events times
    
    return(params)
  })
}

#' Create initial conditions
create.inits <- function(parameters) {
  with(as.list(parameters), {
    # Initialize all state variables to zero
    # Venous blood from organs
    # M_Ven_0 <- 0; M_Ven_1 <- 0; M_Ven_2 <- 0; M_Ven_3 <- 0; M_Ven_4 <- 0;
    # Adipose tissue
    M_Adip_0 <- 0; M_Adip_1 <- 0; M_Adip_2 <- 0; M_Adip_3 <- 0; M_Adip_4 <- 0;
    
    # Bone
    M_Bone_0 <- 0; M_Bone_1 <- 0; M_Bone_2 <- 0; M_Bone_3 <- 0; M_Bone_4 <- 0;
    
    # Brain
    M_Brain_0 <- 0; M_Brain_1 <- 0; M_Brain_2 <- 0; M_Brain_3 <- 0; M_Brain_4 <- 0;
    
    # Heart
    M_Heart_0 <- 0; M_Heart_1 <- 0; M_Heart_2 <- 0; M_Heart_3 <- 0; M_Heart_4 <- 0;
    
    # Kidney
    M_Kidney_0 <- 0; M_Kidney_1 <- 0; M_Kidney_2 <- 0; M_Kidney_3 <- 0; M_Kidney_4 <- 0;
    
    # Intestine
    M_Intestine_0 <- 0; M_Intestine_1 <- 0; M_Intestine_2 <- 0; M_Intestine_3 <- 0; M_Intestine_4 <- 0;
    
    # Liver
    M_Liver_0 <- 0; M_Liver_1 <- 0; M_Liver_2 <- 0; M_Liver_3 <- 0; M_Liver_4 <- 0;
    
    # Lung
    Cinh_0 <- 0;
    M_Lung_0 <- 0; M_Lung_1 <- 0; M_Lung_2 <- 0; M_Lung_3 <- 0; M_Lung_4 <- 0;
    
    # Muscle
    M_Muscle_0 <- 0; M_Muscle_1 <- 0; M_Muscle_2 <- 0; M_Muscle_3 <- 0; M_Muscle_4 <- 0;
    
    # Skin
    M_Skin_0 <- 0; M_Skin_1 <- 0; M_Skin_2 <- 0; M_Skin_3 <- 0; M_Skin_4 <- 0;
    DeposRate <- 0;
    StratCornAdd <- 0; StratCornDecr <- 0; EvapReal <- 0; EvapSC <- 0; Sum_Skin_Air<-0; Sum_Skin_Liq <- 0;
    
    # Bone marrow
    M_Marrow_0 <- 0; M_Marrow_1 <- 0; M_Marrow_2 <- 0; M_Marrow_3 <- 0; M_Marrow_4 <- 0;
    
    # Blood compartments
    M_LungArt_0 <- 0; M_LungArt_1 <- 0; M_LungArt_2 <- 0; M_LungArt_3 <- 0; M_LungArt_4 <- 0;
    M_LungVen_0 <- 0; M_LungVen_1 <- 0; M_LungVen_2 <- 0; M_LungVen_3 <- 0; M_LungVen_4 <- 0;
    
    # Excretion
    UrinExcr_0 <- 0; UrinExcr_1 <- 0; UrinExcr_2 <- 0; UrinExcr_3 <- 0; UrinExcr_4 <- 0;
    
    # Inhalation and exhalation
    M_inhaled_0 <- 0;
    Μ_Exh_0 <- 0; Μ_Exh_1 <- 0; Μ_Exh_2 <- 0; Μ_Exh_3 <- 0; Μ_Exh_4 <- 0;
    
    # Intestinal cavity and bolus
    M_IntCav_0 <- 0; M_IntCav_1 <- 0; M_IntCav_2 <- 0; M_IntCav_3 <- 0; M_IntCav_4 <- 0;
    BolusDose <- 0;
    
    # Skin surface compartments
    MassSurf <- 0; StratCorn <- 0;
    
    # Urine volume
    UrinVol <- 0; 
    
    Sum_Bolus <- 0; 
    
    # Return all initial states in a named vector
    return(c(
      # 'M_Ven_0'=M_Ven_0, 'M_Ven_1'=M_Ven_1, 'M_Ven_2'=M_Ven_2, 'M_Ven_3'=M_Ven_3, 'M_Ven_4'=M_Ven_4,
      'M_Adip_0'=M_Adip_0, 'M_Adip_1'=M_Adip_1, 'M_Adip_2'=M_Adip_2, 'M_Adip_3'=M_Adip_3, 'M_Adip_4'=M_Adip_4,
      'M_Bone_0'=M_Bone_0, 'M_Bone_1'=M_Bone_1, 'M_Bone_2'=M_Bone_2, 'M_Bone_3'=M_Bone_3, 'M_Bone_4'=M_Bone_4,
      'M_Brain_0'=M_Brain_0, 'M_Brain_1'=M_Brain_1, 'M_Brain_2'=M_Brain_2, 'M_Brain_3'=M_Brain_3, 'M_Brain_4'=M_Brain_4,
      'M_Heart_0'=M_Heart_0, 'M_Heart_1'=M_Heart_1, 'M_Heart_2'=M_Heart_2, 'M_Heart_3'=M_Heart_3, 'M_Heart_4'=M_Heart_4,
      'M_Kidney_0'=M_Kidney_0, 'M_Kidney_1'=M_Kidney_1, 'M_Kidney_2'=M_Kidney_2, 'M_Kidney_3'=M_Kidney_3, 'M_Kidney_4'=M_Kidney_4,
      'M_Intestine_0'=M_Intestine_0, 'M_Intestine_1'=M_Intestine_1, 'M_Intestine_2'=M_Intestine_2, 'M_Intestine_3'=M_Intestine_3, 'M_Intestine_4'=M_Intestine_4,
      'M_Liver_0'=M_Liver_0, 'M_Liver_1'=M_Liver_1, 'M_Liver_2'=M_Liver_2, 'M_Liver_3'=M_Liver_3, 'M_Liver_4'=M_Liver_4,
      'Cinh_0'=Cinh_0, 'M_Lung_0'=M_Lung_0, 'M_Lung_1'=M_Lung_1, 'M_Lung_2'=M_Lung_2, 'M_Lung_3'=M_Lung_3, 'M_Lung_4'=M_Lung_4,
      'M_Muscle_0'=M_Muscle_0, 'M_Muscle_1'=M_Muscle_1, 'M_Muscle_2'=M_Muscle_2, 'M_Muscle_3'=M_Muscle_3, 'M_Muscle_4'=M_Muscle_4,
      'M_Skin_0'=M_Skin_0, 'M_Skin_1'=M_Skin_1, 'M_Skin_2'=M_Skin_2, 'M_Skin_3'=M_Skin_3, 'M_Skin_4'=M_Skin_4,
      'DeposRate'=DeposRate,
      'StratCornAdd'=StratCornAdd, "StratCornDecr"=StratCornDecr, "EvapReal"=EvapReal, "EvapSC"=EvapSC, "Sum_Skin_Air"=Sum_Skin_Air,"Sum_Skin_Liq"=Sum_Skin_Liq,
      'M_Marrow_0'=M_Marrow_0, 'M_Marrow_1'=M_Marrow_1, 'M_Marrow_2'=M_Marrow_2, 'M_Marrow_3'=M_Marrow_3, 'M_Marrow_4'=M_Marrow_4,
      'M_LungArt_0'=M_LungArt_0, 'M_LungArt_1'=M_LungArt_1, 'M_LungArt_2'=M_LungArt_2, 'M_LungArt_3'=M_LungArt_3, 'M_LungArt_4'=M_LungArt_4,
      'M_LungVen_0'=M_LungVen_0, 'M_LungVen_1'=M_LungVen_1, 'M_LungVen_2'=M_LungVen_2, 'M_LungVen_3'=M_LungVen_3, 'M_LungVen_4'=M_LungVen_4,
      'UrinExcr_0'=UrinExcr_0, 'UrinExcr_1'=UrinExcr_1, 'UrinExcr_2'=UrinExcr_2, 'UrinExcr_3'=UrinExcr_3, 'UrinExcr_4'=UrinExcr_4,
      'M_inhaled_0'=M_inhaled_0,
      'Μ_Exh_0'=Μ_Exh_0, 'Μ_Exh_1'=Μ_Exh_1, 'Μ_Exh_2'=Μ_Exh_2, 'Μ_Exh_3'=Μ_Exh_3, 'Μ_Exh_4'=Μ_Exh_4,
      'M_IntCav_0'=M_IntCav_0, 'M_IntCav_1'=M_IntCav_1, 'M_IntCav_2'=M_IntCav_2, 'M_IntCav_3'=M_IntCav_3, 'M_IntCav_4'=M_IntCav_4,
      'BolusDose'=BolusDose,
      'MassSurf'=MassSurf, 'StratCorn'=StratCorn,
      'UrinVol'=UrinVol,
      'Sum_Bolus'=Sum_Bolus
    ))
  })
}


#' ODE function for PBTK model
ode.func <- function(time, state, params, custom.func) {
  with(as.list(c(state, params)), {
    
    # Units
    # dM_i/dt: micromole/hour
    # C_i: micromole/litre
    
    C_Adip_0 = M_Adip_0/VolCmpAdip
    C_Adip_1 = M_Adip_1/VolCmpAdip
    C_Adip_2 = M_Adip_2/VolCmpAdip
    C_Adip_3 = M_Adip_3/VolCmpAdip
    C_Adip_4 = M_Adip_4/VolCmpAdip
    
    C_Bone_0 = M_Bone_0/VolCmpBone
    C_Bone_1 = M_Bone_1/VolCmpBone
    C_Bone_2 = M_Bone_2/VolCmpBone
    C_Bone_3 = M_Bone_3/VolCmpBone
    C_Bone_4 = M_Bone_4/VolCmpBone
    
    C_Brain_0 = M_Brain_0/VolCmpBrain
    C_Brain_1 = M_Brain_1/VolCmpBrain
    C_Brain_2 = M_Brain_2/VolCmpBrain
    C_Brain_3 = M_Brain_3/VolCmpBrain
    C_Brain_4 = M_Brain_4/VolCmpBrain
    
    C_Heart_0 = M_Heart_0/VolCmpHeart
    C_Heart_1 = M_Heart_1/VolCmpHeart
    C_Heart_2 = M_Heart_2/VolCmpHeart
    C_Heart_3 = M_Heart_3/VolCmpHeart
    C_Heart_4 = M_Heart_4/VolCmpHeart
    
    C_Muscle_0 = M_Muscle_0/VolCmpMuscle
    C_Muscle_1 = M_Muscle_1/VolCmpMuscle
    C_Muscle_2 = M_Muscle_2/VolCmpMuscle
    C_Muscle_3 = M_Muscle_3/VolCmpMuscle
    C_Muscle_4 = M_Muscle_4/VolCmpMuscle
    
    C_Marrow_0 = M_Marrow_0/VolCmpMarrow
    C_Marrow_1 = M_Marrow_1/VolCmpMarrow
    C_Marrow_2 = M_Marrow_2/VolCmpMarrow
    C_Marrow_3 = M_Marrow_3/VolCmpMarrow
    C_Marrow_4 = M_Marrow_4/VolCmpMarrow
    
    C_Kidney_0 = M_Kidney_0/VolCmpKidney
    C_Kidney_1 = M_Kidney_1/VolCmpKidney
    C_Kidney_2 = M_Kidney_2/VolCmpKidney
    C_Kidney_3 = M_Kidney_3/VolCmpKidney
    C_Kidney_4 = M_Kidney_4/VolCmpKidney
    
    C_Intestine_0 = M_Intestine_0/VolCmpIntestine
    C_Intestine_1 = M_Intestine_1/VolCmpIntestine
    C_Intestine_2 = M_Intestine_2/VolCmpIntestine
    C_Intestine_3 = M_Intestine_3/VolCmpIntestine
    C_Intestine_4 = M_Intestine_4/VolCmpIntestine
    
    C_Liver_0 = M_Liver_0/VolCmpLiver
    C_Liver_1 = M_Liver_1/VolCmpLiver
    C_Liver_2 = M_Liver_2/VolCmpLiver
    C_Liver_3 = M_Liver_3/VolCmpLiver
    C_Liver_4 = M_Liver_4/VolCmpLiver
    
    C_Skin_0 = M_Skin_0/VolCmpSkin
    C_Skin_1 = M_Skin_1/VolCmpSkin
    C_Skin_2 = M_Skin_2/VolCmpSkin
    C_Skin_3 = M_Skin_3/VolCmpSkin
    C_Skin_4 = M_Skin_4/VolCmpSkin
    
    C_Lung_0 = M_Lung_0/VolCmpLung
    C_Lung_1 = M_Lung_1/VolCmpLung
    C_Lung_2 = M_Lung_2/VolCmpLung
    C_Lung_3 = M_Lung_3/VolCmpLung
    C_Lung_4 = M_Lung_4/VolCmpLung
    
    C_BlungVen_0 = M_LungVen_0 / VolBlungVen # concentration in venous blood volume flowing to the lung
    C_BlungVen_1 = M_LungVen_1 / VolBlungVen # concentration in venous blood volume flowing to the lung
    C_BlungVen_2 = M_LungVen_2 / VolBlungVen # concentration in venous blood volume flowing to the lung
    C_BlungVen_3 = M_LungVen_3 / VolBlungVen # concentration in venous blood volume flowing to the lung
    C_BlungVen_4 = M_LungVen_4 / VolBlungVen # concentration in venous blood volume flowing to the lung
    
    dCinh_0 = 0
    
    C_BlungArt_0 = (CardOutp * C_BlungVen_0 + AlvVent * Cinh_0) / (CardOutp + AlvVent / RCba_0)   # micromole/litre
    C_BlungArt_1 = (CardOutp * C_BlungVen_1) / (CardOutp + AlvVent / RCba_1)   # micromole/litre
    C_BlungArt_2 = (CardOutp * C_BlungVen_2) / (CardOutp + AlvVent / RCba_2)   # micromole/litre
    C_BlungArt_3 = (CardOutp * C_BlungVen_3) / (CardOutp + AlvVent / RCba_3)   # micromole/litre
    C_BlungArt_4 = (CardOutp * C_BlungVen_4) / (CardOutp + AlvVent / RCba_4)   # micromole/litre
    
    Calv_0 = C_BlungArt_0 / RCba_0 # micromole/litre
    Calv_1 = C_BlungArt_1 / RCba_1 # micromole/litre
    Calv_2 = C_BlungArt_2 / RCba_2 # micromole/litre
    Calv_3 = C_BlungArt_3 / RCba_3 # micromole/litre
    Calv_4 = C_BlungArt_4 / RCba_4 # micromole/litre
    
    # concentration in arterial blood from heart
    Cblart_0 = M_LungArt_0/VolBlungArt 
    Cblart_1 = M_LungArt_1/VolBlungArt
    Cblart_2 = M_LungArt_2/VolBlungArt
    Cblart_3 = M_LungArt_3/VolBlungArt
    Cblart_4 = M_LungArt_4/VolBlungArt
    
    # Concentrations in Urine
    C_Urine_0 = UrinExcr_0 / UrinVol
    C_Urine_1 = UrinExcr_1 / UrinVol
    C_Urine_2 = UrinExcr_2 / UrinVol
    C_Urine_3 = UrinExcr_3 / UrinVol
    C_Urine_4 = UrinExcr_4 / UrinVol
    
    M_org_ven_0 = FlowOrgAdip * C_Adip_0/R_Adip_ven_0 + FlowOrgBone * C_Bone_0/R_Bone_ven_0 + FlowOrgBrain * C_Brain_0/R_Brain_ven_0 + FlowOrgHeart * C_Heart_0/R_Heart_ven_0 + FlowOrgKidney * C_Kidney_0/R_Kidney_ven_0 + (FlowOrgIntestine + FlowOrgLivArt) * C_Liver_0/R_Liver_ven_0 + FlowOrgMuscle * C_Muscle_0/R_Muscle_ven_0 + FlowOrgSkin * C_Skin_0/R_Skin_ven_0 + FlowOrgMarrow * C_Marrow_0/R_Marrow_ven_0 + FlowOrgLung * C_Lung_0/R_Lung_ven_0 
    M_org_ven_1 = FlowOrgAdip * C_Adip_1/R_Adip_ven_1 + FlowOrgBone * C_Bone_1/R_Bone_ven_1 + FlowOrgBrain * C_Brain_1/R_Brain_ven_1 + FlowOrgHeart * C_Heart_1/R_Heart_ven_1 + FlowOrgKidney * C_Kidney_1/R_Kidney_ven_1 + (FlowOrgIntestine + FlowOrgLivArt) * C_Liver_1/R_Liver_ven_1 + FlowOrgMuscle * C_Muscle_1/R_Muscle_ven_1 + FlowOrgSkin * C_Skin_1/R_Skin_ven_1 + FlowOrgMarrow * C_Marrow_1/R_Marrow_ven_1 + FlowOrgLung * C_Lung_1/R_Lung_ven_1 
    M_org_ven_2 = FlowOrgAdip * C_Adip_2/R_Adip_ven_2 + FlowOrgBone * C_Bone_2/R_Bone_ven_2 + FlowOrgBrain * C_Brain_2/R_Brain_ven_2 + FlowOrgHeart * C_Heart_2/R_Heart_ven_2 + FlowOrgKidney * C_Kidney_2/R_Kidney_ven_2 + (FlowOrgIntestine + FlowOrgLivArt) * C_Liver_2/R_Liver_ven_2 + FlowOrgMuscle * C_Muscle_2/R_Muscle_ven_2 + FlowOrgSkin * C_Skin_2/R_Skin_ven_2 + FlowOrgMarrow * C_Marrow_2/R_Marrow_ven_2 + FlowOrgLung * C_Lung_2/R_Lung_ven_2 
    M_org_ven_3 = FlowOrgAdip * C_Adip_3/R_Adip_ven_3 + FlowOrgBone * C_Bone_3/R_Bone_ven_3 + FlowOrgBrain * C_Brain_3/R_Brain_ven_3 + FlowOrgHeart * C_Heart_3/R_Heart_ven_3 + FlowOrgKidney * C_Kidney_3/R_Kidney_ven_3 + (FlowOrgIntestine + FlowOrgLivArt) * C_Liver_3/R_Liver_ven_3 + FlowOrgMuscle * C_Muscle_3/R_Muscle_ven_3 + FlowOrgSkin * C_Skin_3/R_Skin_ven_3 + FlowOrgMarrow * C_Marrow_3/R_Marrow_ven_3 + FlowOrgLung * C_Lung_3/R_Lung_ven_3 
    M_org_ven_4 = FlowOrgAdip * C_Adip_4/R_Adip_ven_4 + FlowOrgBone * C_Bone_4/R_Bone_ven_4 + FlowOrgBrain * C_Brain_4/R_Brain_ven_4 + FlowOrgHeart * C_Heart_4/R_Heart_ven_4 + FlowOrgKidney * C_Kidney_4/R_Kidney_ven_4 + (FlowOrgIntestine + FlowOrgLivArt) * C_Liver_4/R_Liver_ven_4 + FlowOrgMuscle * C_Muscle_4/R_Muscle_ven_4 + FlowOrgSkin * C_Skin_4/R_Skin_ven_4 + FlowOrgMarrow * C_Marrow_4/R_Marrow_ven_4 + FlowOrgLung * C_Lung_4/R_Lung_ven_4 
    
    Cblven_0 = M_org_ven_0 / CardOutp # micromole/litre in venous blood from all organs
    Cblven_1 = M_org_ven_1 / CardOutp # micromole/litre in venous blood from all organs
    Cblven_2 = M_org_ven_2 / CardOutp # micromole/litre in venous blood from all organs
    Cblven_3 = M_org_ven_3 / CardOutp # micromole/litre in venous blood from all organs
    Cblven_4 = M_org_ven_4 / CardOutp # micromole/litre in venous blood from all organs
    
    # dM_Ven_0 = 0#FlowOrgAdip * C_Adip_0/R_Adip_ven_0 + FlowOrgBone * C_Bone_0/R_Bone_ven_0 + FlowOrgBrain * C_Brain_0/R_Brain_ven_0 + FlowOrgHeart * C_Heart_0/R_Heart_ven_0 + FlowOrgKidney * C_Kidney_0/R_Kidney_ven_0 + (FlowOrgIntestine + FlowOrgLiver) * C_Liver_0/R_Liver_ven_0 + FlowOrgMuscle * C_Muscle_0/R_Muscle_ven_0 + FlowOrgSkin * C_Skin_0/R_Skin_ven_0 + FlowOrgMarrow * C_Marrow_0/R_Marrow_ven_0 - FlowOrgLung * C_Lung_0/R_Lung_ven_0 
    # dM_Ven_1 = 0#FlowOrgAdip * C_Adip_1/R_Adip_ven_1 + FlowOrgBone * C_Bone_1/R_Bone_ven_1 + FlowOrgBrain * C_Brain_1/R_Brain_ven_1 + FlowOrgHeart * C_Heart_1/R_Heart_ven_1 + FlowOrgKidney * C_Kidney_1/R_Kidney_ven_1 + (FlowOrgIntestine + FlowOrgLiver) * C_Liver_1/R_Liver_ven_1 + FlowOrgMuscle * C_Muscle_1/R_Muscle_ven_1 + FlowOrgSkin * C_Skin_1/R_Skin_ven_1 + FlowOrgMarrow * C_Marrow_1/R_Marrow_ven_1 - FlowOrgLung * C_Lung_1/R_Lung_ven_1 
    # dM_Ven_2 = 0#FlowOrgAdip * C_Adip_2/R_Adip_ven_2 + FlowOrgBone * C_Bone_2/R_Bone_ven_2 + FlowOrgBrain * C_Brain_2/R_Brain_ven_2 + FlowOrgHeart * C_Heart_2/R_Heart_ven_2 + FlowOrgKidney * C_Kidney_2/R_Kidney_ven_2 + (FlowOrgIntestine + FlowOrgLiver) * C_Liver_2/R_Liver_ven_2 + FlowOrgMuscle * C_Muscle_2/R_Muscle_ven_2 + FlowOrgSkin * C_Skin_2/R_Skin_ven_2 + FlowOrgMarrow * C_Marrow_2/R_Marrow_ven_2 - FlowOrgLung * C_Lung_2/R_Lung_ven_2 
    # dM_Ven_3 = 0#FlowOrgAdip * C_Adip_3/R_Adip_ven_3 + FlowOrgBone * C_Bone_3/R_Bone_ven_3 + FlowOrgBrain * C_Brain_3/R_Brain_ven_3 + FlowOrgHeart * C_Heart_3/R_Heart_ven_3 + FlowOrgKidney * C_Kidney_3/R_Kidney_ven_3 + (FlowOrgIntestine + FlowOrgLiver) * C_Liver_3/R_Liver_ven_3 + FlowOrgMuscle * C_Muscle_3/R_Muscle_ven_3 + FlowOrgSkin * C_Skin_3/R_Skin_ven_3 + FlowOrgMarrow * C_Marrow_3/R_Marrow_ven_3 - FlowOrgLung * C_Lung_3/R_Lung_ven_3 
    # dM_Ven_4 = 0#FlowOrgAdip * C_Adip_4/R_Adip_ven_4 + FlowOrgBone * C_Bone_4/R_Bone_ven_4 + FlowOrgBrain * C_Brain_4/R_Brain_ven_4 + FlowOrgHeart * C_Heart_4/R_Heart_ven_4 + FlowOrgKidney * C_Kidney_4/R_Kidney_ven_4 + (FlowOrgIntestine + FlowOrgLiver) * C_Liver_4/R_Liver_ven_4 + FlowOrgMuscle * C_Muscle_4/R_Muscle_ven_4 + FlowOrgSkin * C_Skin_4/R_Skin_ven_4 + FlowOrgMarrow * C_Marrow_4/R_Marrow_ven_4 - FlowOrgLung * C_Lung_4/R_Lung_ven_4 
    
    dM_Adip_0 = FlowOrgAdip * (Cblart_0 - C_Adip_0/R_Adip_ven_0) - (Vmax_Adip_0 * VolCmpAdip * C_Adip_0)/(Km_Adip_0 + C_Adip_0)
    dM_Adip_1 = FlowOrgAdip * (Cblart_1 - C_Adip_1/R_Adip_ven_1) - (Vmax_Adip_1 * VolCmpAdip * C_Adip_1)/(Km_Adip_1 + C_Adip_1) + (Vmax_p_Adip_0 * VolCmpAdip * C_Adip_0)/(Km_Adip_0 + C_Adip_0)
    dM_Adip_2 = FlowOrgAdip * (Cblart_2 - C_Adip_2/R_Adip_ven_2) - (Vmax_Adip_2 * VolCmpAdip * C_Adip_2)/(Km_Adip_2 + C_Adip_2) + (Vmax_p_Adip_1 * VolCmpAdip * C_Adip_1)/(Km_Adip_1 + C_Adip_1)
    dM_Adip_3 = FlowOrgAdip * (Cblart_3 - C_Adip_3/R_Adip_ven_3) - (Vmax_Adip_3 * VolCmpAdip * C_Adip_3)/(Km_Adip_3 + C_Adip_3) + (Vmax_p_Adip_2 * VolCmpAdip * C_Adip_2)/(Km_Adip_2 + C_Adip_2)
    dM_Adip_4 = FlowOrgAdip * (Cblart_4 - C_Adip_4/R_Adip_ven_4) - (Vmax_Adip_4 * VolCmpAdip * C_Adip_4)/(Km_Adip_4 + C_Adip_4) + (Vmax_p_Adip_3 * VolCmpAdip * C_Adip_3)/(Km_Adip_3 + C_Adip_3)
    
    dM_Bone_0 = FlowOrgBone * (Cblart_0 - C_Bone_0/R_Bone_ven_0) - (Vmax_Bone_0 * VolCmpBone * C_Bone_0)/(Km_Bone_0 + C_Bone_0)
    dM_Bone_1 = FlowOrgBone * (Cblart_1 - C_Bone_1/R_Bone_ven_1) - (Vmax_Bone_1 * VolCmpBone * C_Bone_1)/(Km_Bone_1 + C_Bone_1) + (Vmax_p_Bone_0 * VolCmpBone * C_Bone_0)/(Km_Bone_0 + C_Bone_0)
    dM_Bone_2 = FlowOrgBone * (Cblart_2 - C_Bone_2/R_Bone_ven_2) - (Vmax_Bone_2 * VolCmpBone * C_Bone_2)/(Km_Bone_2 + C_Bone_2) + (Vmax_p_Bone_1 * VolCmpBone * C_Bone_1)/(Km_Bone_1 + C_Bone_1)
    dM_Bone_3 = FlowOrgBone * (Cblart_3 - C_Bone_3/R_Bone_ven_3) - (Vmax_Bone_3 * VolCmpBone * C_Bone_3)/(Km_Bone_3 + C_Bone_3) + (Vmax_p_Bone_2 * VolCmpBone * C_Bone_2)/(Km_Bone_2 + C_Bone_2)
    dM_Bone_4 = FlowOrgBone * (Cblart_4 - C_Bone_4/R_Bone_ven_4) - (Vmax_Bone_4 * VolCmpBone * C_Bone_4)/(Km_Bone_4 + C_Bone_4) + (Vmax_p_Bone_3 * VolCmpBone * C_Bone_3)/(Km_Bone_3 + C_Bone_3)
    
    dM_Brain_0 = FlowOrgBrain * (Cblart_0 - C_Brain_0/R_Brain_ven_0) - (Vmax_Brain_0 * VolCmpBrain * C_Brain_0)/(Km_Brain_0 + C_Brain_0)
    dM_Brain_1 = FlowOrgBrain * (Cblart_1 - C_Brain_1/R_Brain_ven_1) - (Vmax_Brain_1 * VolCmpBrain * C_Brain_1)/(Km_Brain_1 + C_Brain_1) + (Vmax_p_Brain_0 * VolCmpBrain * C_Brain_0)/(Km_Brain_0 + C_Brain_0)
    dM_Brain_2 = FlowOrgBrain * (Cblart_2 - C_Brain_2/R_Brain_ven_2) - (Vmax_Brain_2 * VolCmpBrain * C_Brain_2)/(Km_Brain_2 + C_Brain_2) + (Vmax_p_Brain_1 * VolCmpBrain * C_Brain_1)/(Km_Brain_1 + C_Brain_1)
    dM_Brain_3 = FlowOrgBrain * (Cblart_3 - C_Brain_3/R_Brain_ven_3) - (Vmax_Brain_3 * VolCmpBrain * C_Brain_3)/(Km_Brain_3 + C_Brain_3) + (Vmax_p_Brain_2 * VolCmpBrain * C_Brain_2)/(Km_Brain_2 + C_Brain_2)
    dM_Brain_4 = FlowOrgBrain * (Cblart_4 - C_Brain_4/R_Brain_ven_4) - (Vmax_Brain_4 * VolCmpBrain * C_Brain_4)/(Km_Brain_4 + C_Brain_4) + (Vmax_p_Brain_3 * VolCmpBrain * C_Brain_3)/(Km_Brain_3 + C_Brain_3)
    
    dM_Heart_0 = FlowOrgHeart * (Cblart_0 - C_Heart_0/R_Heart_ven_0) - (Vmax_Heart_0 * VolCmpHeart * C_Heart_0)/(Km_Heart_0 + C_Heart_0)
    dM_Heart_1 = FlowOrgHeart * (Cblart_1 - C_Heart_1/R_Heart_ven_1) - (Vmax_Heart_1 * VolCmpHeart * C_Heart_1)/(Km_Heart_1 + C_Heart_1) + (Vmax_p_Heart_0 * VolCmpHeart * C_Heart_0)/(Km_Heart_0 + C_Heart_0)
    dM_Heart_2 = FlowOrgHeart * (Cblart_2 - C_Heart_2/R_Heart_ven_2) - (Vmax_Heart_2 * VolCmpHeart * C_Heart_2)/(Km_Heart_2 + C_Heart_2) + (Vmax_p_Heart_1 * VolCmpHeart * C_Heart_1)/(Km_Heart_1 + C_Heart_1)
    dM_Heart_3 = FlowOrgHeart * (Cblart_3 - C_Heart_3/R_Heart_ven_3) - (Vmax_Heart_3 * VolCmpHeart * C_Heart_3)/(Km_Heart_3 + C_Heart_3) + (Vmax_p_Heart_2 * VolCmpHeart * C_Heart_2)/(Km_Heart_2 + C_Heart_2)
    dM_Heart_4 = FlowOrgHeart * (Cblart_4 - C_Heart_4/R_Heart_ven_4) - (Vmax_Heart_4 * VolCmpHeart * C_Heart_4)/(Km_Heart_4 + C_Heart_4) + (Vmax_p_Heart_3 * VolCmpHeart * C_Heart_3)/(Km_Heart_3 + C_Heart_3)
    
    dM_Muscle_0 = FlowOrgMuscle * (Cblart_0 - C_Muscle_0/R_Muscle_ven_0) - (Vmax_Muscle_0 * VolCmpMuscle * C_Muscle_0)/(Km_Muscle_0 + C_Muscle_0)
    dM_Muscle_1 = FlowOrgMuscle * (Cblart_1 - C_Muscle_1/R_Muscle_ven_1) - (Vmax_Muscle_1 * VolCmpMuscle * C_Muscle_1)/(Km_Muscle_1 + C_Muscle_1) + (Vmax_p_Muscle_0 * VolCmpMuscle * C_Muscle_0)/(Km_Muscle_0 + C_Muscle_0)
    dM_Muscle_2 = FlowOrgMuscle * (Cblart_2 - C_Muscle_2/R_Muscle_ven_2) - (Vmax_Muscle_2 * VolCmpMuscle * C_Muscle_2)/(Km_Muscle_2 + C_Muscle_2) + (Vmax_p_Muscle_1 * VolCmpMuscle * C_Muscle_1)/(Km_Muscle_1 + C_Muscle_1)
    dM_Muscle_3 = FlowOrgMuscle * (Cblart_3 - C_Muscle_3/R_Muscle_ven_3) - (Vmax_Muscle_3 * VolCmpMuscle * C_Muscle_3)/(Km_Muscle_3 + C_Muscle_3) + (Vmax_p_Muscle_2 * VolCmpMuscle * C_Muscle_2)/(Km_Muscle_2 + C_Muscle_2)
    dM_Muscle_4 = FlowOrgMuscle * (Cblart_4 - C_Muscle_4/R_Muscle_ven_4) - (Vmax_Muscle_4 * VolCmpMuscle * C_Muscle_4)/(Km_Muscle_4 + C_Muscle_4) + (Vmax_p_Muscle_3 * VolCmpMuscle * C_Muscle_3)/(Km_Muscle_3 + C_Muscle_3)
    
    dM_Marrow_0 = FlowOrgMarrow * (Cblart_0 - C_Marrow_0/R_Marrow_ven_0) - (Vmax_Marrow_0 * VolCmpMarrow * C_Marrow_0)/(Km_Marrow_0 + C_Marrow_0)
    dM_Marrow_1 = FlowOrgMarrow * (Cblart_1 - C_Marrow_1/R_Marrow_ven_1) - (Vmax_Marrow_1 * VolCmpMarrow * C_Marrow_1)/(Km_Marrow_1 + C_Marrow_1) + (Vmax_p_Marrow_0 * VolCmpMarrow * C_Marrow_0)/(Km_Marrow_0 + C_Marrow_0)
    dM_Marrow_2 = FlowOrgMarrow * (Cblart_2 - C_Marrow_2/R_Marrow_ven_2) - (Vmax_Marrow_2 * VolCmpMarrow * C_Marrow_2)/(Km_Marrow_2 + C_Marrow_2) + (Vmax_p_Marrow_1 * VolCmpMarrow * C_Marrow_1)/(Km_Marrow_1 + C_Marrow_1)
    dM_Marrow_3 = FlowOrgMarrow * (Cblart_3 - C_Marrow_3/R_Marrow_ven_3) - (Vmax_Marrow_3 * VolCmpMarrow * C_Marrow_3)/(Km_Marrow_3 + C_Marrow_3) + (Vmax_p_Marrow_2 * VolCmpMarrow * C_Marrow_2)/(Km_Marrow_2 + C_Marrow_2)
    dM_Marrow_4 = FlowOrgMarrow * (Cblart_4 - C_Marrow_4/R_Marrow_ven_4) - (Vmax_Marrow_4 * VolCmpMarrow * C_Marrow_4)/(Km_Marrow_4 + C_Marrow_4) + (Vmax_p_Marrow_3 * VolCmpMarrow * C_Marrow_3)/(Km_Marrow_3 + C_Marrow_3)
    
    # Kidney 
    dM_Kidney_0 = FlowOrgKidney * (Cblart_0 - C_Kidney_0/R_Kidney_ven_0) - (Vmax_Kidney_0 * VolCmpKidney * C_Kidney_0)/(Km_Kidney_0 + C_Kidney_0) - GlomFiltr * RemovKdn_0 * FlowOrgKidney * Cblart_0 * FrWsol_0 # micromole/hour
    dM_Kidney_1 = FlowOrgKidney * (Cblart_1 - C_Kidney_1/R_Kidney_ven_1) - (Vmax_Kidney_1 * VolCmpKidney * C_Kidney_1)/(Km_Kidney_1 + C_Kidney_1) + (Vmax_p_Kidney_0 * VolCmpKidney * C_Kidney_0)/(Km_Kidney_0 + C_Kidney_0) - GlomFiltr * RemovKdn_1 * FlowOrgKidney * Cblart_1 * FrWsol_1 # micromole/hour 
    dM_Kidney_2 = FlowOrgKidney * (Cblart_2 - C_Kidney_2/R_Kidney_ven_2) - (Vmax_Kidney_2 * VolCmpKidney * C_Kidney_2)/(Km_Kidney_2 + C_Kidney_2) + (Vmax_p_Kidney_1 * VolCmpKidney * C_Kidney_1)/(Km_Kidney_1 + C_Kidney_1) - GlomFiltr * RemovKdn_2 * FlowOrgKidney * Cblart_2 * FrWsol_2 # micromole/hour 
    dM_Kidney_3 = FlowOrgKidney * (Cblart_3 - C_Kidney_3/R_Kidney_ven_3) - (Vmax_Kidney_3 * VolCmpKidney * C_Kidney_3)/(Km_Kidney_3 + C_Kidney_3) + (Vmax_p_Kidney_2 * VolCmpKidney * C_Kidney_2)/(Km_Kidney_2 + C_Kidney_2) - GlomFiltr * RemovKdn_3 * FlowOrgKidney * Cblart_3 * FrWsol_3 # micromole/hour 
    dM_Kidney_4 = FlowOrgKidney * (Cblart_4 - C_Kidney_4/R_Kidney_ven_4) - (Vmax_Kidney_4 * VolCmpKidney * C_Kidney_4)/(Km_Kidney_4 + C_Kidney_4) + (Vmax_p_Kidney_3 * VolCmpKidney * C_Kidney_3)/(Km_Kidney_3 + C_Kidney_3) - GlomFiltr * RemovKdn_4 * FlowOrgKidney * Cblart_4 * FrWsol_4 # micromole/hour 
    
    dUrinExcr_0 = GlomFiltr * RemovKdn_0 * FlowOrgKidney * Cblart_0 * FrWsol_0 # micromole/hour 
    dUrinExcr_1 = GlomFiltr * RemovKdn_1 * FlowOrgKidney * Cblart_1 * FrWsol_1 # micromole/hour
    dUrinExcr_2 = GlomFiltr * RemovKdn_2 * FlowOrgKidney * Cblart_2 * FrWsol_2 # micromole/hour
    dUrinExcr_3 = GlomFiltr * RemovKdn_3 * FlowOrgKidney * Cblart_3 * FrWsol_3 # micromole/hour 
    dUrinExcr_4 = GlomFiltr * RemovKdn_4 * FlowOrgKidney * Cblart_4 * FrWsol_4 # micromole/hour 
    
    dUrinVol = UrinFlow * BodyWt / 24
    
    # Intestine
    dBolusDose = - DecrBolusRt * BolusDose
    
    dM_IntCav_0 = EntHepRt_0 * M_Liver_0 - 0.3 * M_IntCav_0 #micromole/hour
    dM_IntCav_1 = EntHepRt_1 * M_Liver_1 - 0.3 * M_IntCav_1 #micromole/hour
    dM_IntCav_2 = EntHepRt_2 * M_Liver_2 - 0.3 * M_IntCav_2 #micromole/hour
    dM_IntCav_3 = EntHepRt_3 * M_Liver_3 - 0.3 * M_IntCav_3 #micromole/hour
    dM_IntCav_4 = EntHepRt_4 * M_Liver_4 - 0.3 * M_IntCav_4 #micromole/hour
    
    dM_Intestine_0 = FlowOrgIntestine * (Cblart_0 - C_Intestine_0/R_Intestine_ven_0) - (Vmax_Intestine_0 * VolCmpIntestine * C_Intestine_0)/(Km_Intestine_0 + C_Intestine_0) + 0.3 * M_IntCav_0 + DecrBolusRt * BolusDose
    dM_Intestine_1 = FlowOrgIntestine * (Cblart_1 - C_Intestine_1/R_Intestine_ven_1) - (Vmax_Intestine_1 * VolCmpIntestine * C_Intestine_1)/(Km_Intestine_1 + C_Intestine_1) + (Vmax_p_Intestine_0 * VolCmpIntestine * C_Intestine_0)/(Km_Intestine_0 + C_Intestine_0) + 0.3 * M_IntCav_1
    dM_Intestine_2 = FlowOrgIntestine * (Cblart_2 - C_Intestine_2/R_Intestine_ven_2) - (Vmax_Intestine_2 * VolCmpIntestine * C_Intestine_2)/(Km_Intestine_2 + C_Intestine_2) + (Vmax_p_Intestine_1 * VolCmpIntestine * C_Intestine_1)/(Km_Intestine_1 + C_Intestine_1) + 0.3 * M_IntCav_2
    dM_Intestine_3 = FlowOrgIntestine * (Cblart_3 - C_Intestine_3/R_Intestine_ven_3) - (Vmax_Intestine_3 * VolCmpIntestine * C_Intestine_3)/(Km_Intestine_3 + C_Intestine_3) + (Vmax_p_Intestine_2 * VolCmpIntestine * C_Intestine_2)/(Km_Intestine_2 + C_Intestine_2) + 0.3 * M_IntCav_3
    dM_Intestine_4 = FlowOrgIntestine * (Cblart_4 - C_Intestine_4/R_Intestine_ven_4) - (Vmax_Intestine_4 * VolCmpIntestine * C_Intestine_4)/(Km_Intestine_4 + C_Intestine_4) + (Vmax_p_Intestine_3 * VolCmpIntestine * C_Intestine_3)/(Km_Intestine_3 + C_Intestine_3) + 0.3 * M_IntCav_4
    
    # Liver
    dM_Liver_0 = FlowOrgLivArt * (Cblart_0 - C_Liver_0/R_Liver_ven_0) - (Vmax_Liver_0 * VolCmpLiver * C_Liver_0)/(Km_Liver_0 + C_Liver_0) + FlowOrgIntestine * C_Intestine_0/R_Intestine_ven_0 - FlowOrgIntestine * C_Liver_0 / R_Liver_ven_0 - EntHepRt_0 * M_Liver_0
    dM_Liver_1 = FlowOrgLivArt * (Cblart_1 - C_Liver_1/R_Liver_ven_1) - (Vmax_Liver_1 * VolCmpLiver * C_Liver_1)/(Km_Liver_1 + C_Liver_1) + (Vmax_p_Liver_0 * VolCmpLiver * C_Liver_0)/(Km_Liver_0 + C_Liver_0) + FlowOrgIntestine * C_Intestine_1 / R_Intestine_ven_1 - FlowOrgIntestine * C_Liver_1 / R_Liver_ven_1 - EntHepRt_1 * M_Liver_1
    dM_Liver_2 = FlowOrgLivArt * (Cblart_2 - C_Liver_2/R_Liver_ven_2) - (Vmax_Liver_2 * VolCmpLiver * C_Liver_2)/(Km_Liver_2 + C_Liver_2) + (Vmax_p_Liver_1 * VolCmpLiver * C_Liver_1)/(Km_Liver_1 + C_Liver_1) + FlowOrgIntestine * C_Intestine_2 / R_Intestine_ven_2 - FlowOrgIntestine * C_Liver_2 / R_Liver_ven_2 - EntHepRt_2 * M_Liver_2
    dM_Liver_3 = FlowOrgLivArt * (Cblart_3 - C_Liver_3/R_Liver_ven_3) - (Vmax_Liver_3 * VolCmpLiver * C_Liver_3)/(Km_Liver_3 + C_Liver_3) + (Vmax_p_Liver_2 * VolCmpLiver * C_Liver_2)/(Km_Liver_2 + C_Liver_2) + FlowOrgIntestine * C_Intestine_3 / R_Intestine_ven_3 - FlowOrgIntestine * C_Liver_3 / R_Liver_ven_3 - EntHepRt_3 * M_Liver_3
    dM_Liver_4 = FlowOrgLivArt * (Cblart_4 - C_Liver_4/R_Liver_ven_4) - (Vmax_Liver_4 * VolCmpLiver * C_Liver_4)/(Km_Liver_4 + C_Liver_4) + (Vmax_p_Liver_3 * VolCmpLiver * C_Liver_3)/(Km_Liver_3 + C_Liver_3) + FlowOrgIntestine * C_Intestine_4 / R_Intestine_ven_4 - FlowOrgIntestine * C_Liver_4 / R_Liver_ven_4 - EntHepRt_4 * M_Liver_4
    
    # Skin
    # Dermal absorption
    DermFact = 1 
    dDeposRate = 0 
    # Step 1: Calculate potential absorption rate
    MassInitAbs = 2 * dens_0 * Kpw0 * (MassLoad - StratCorn) / MassLoad / Fpart
    # Step 2: Check for quick depletion of surface material
    if (MassInitAbs + EvapL > DeposRate) {
      MassSurf = 0
    }
    RtScMc = min(1, 2 * StratCorn / MaxCap)
    if(MassSurf > 0){
      # Chemical present on the skin surface
      if (MassInitAbs + EvapL > MassSurf) {
        # Not enough material - divide proportionally
        dStratCornAdd = MassSurf * MassInitAbs / (EvapL + MassInitAbs)
        dEvapReal = MassSurf * EvapL / (EvapL + MassInitAbs)
      } else {
        # Enough material for full rates
        dStratCornAdd = MassInitAbs
        dEvapReal = EvapL
      }
      # Movement from stratum corneum to viable skin
      dStratCornDecr = RtScMc * Kpw0 * wsol_0 / 1000
    }else {
      if(DeposRate > 0){
        # Active deposition
        dStratCornAdd = DeposRate * MassInitAbs / (EvapL + MassInitAbs)
        dEvapReal = DeposRate * EvapL / (EvapL + MassInitAbs)
        
        # Evaporation from stratum corneum
        dEvapSC = Kevapw * RtScMc * wsol_0 / 1000
        
        if (dEvapReal < dEvapSC) {
          dEvapReal = dEvapSC
          dStratCornAdd = DeposRate - dEvapReal
        }
        
        dStratCornDecr = RtScMc * Kpw0 * wsol_0 / 1000
      }else {
        # No deposition, only clearance
        dEvapSC = Kevapw * RtScMc * wsol_0 / 1000
        dEvapReal = dEvapSC
        dStratCornAdd = 0  
        dStratCornDecr = RtScMc * (Kevapw + Kpw0) * wsol_0 / 1000
      }
    }
    
    # Stratum Conrneum balance
    dStratCorn = dStratCornAdd - dStratCornDecr
    
    # Calculate surface mass change
    dMassSurf = DeposRate - dEvapReal - dStratCornAdd
    
    # Calculate absorption into viable skin
    dAmtAbs = RtScMc * wsol_0 * Kpw0 / 1000  # mg/cm²/hour
    
    # Convert to flux for PBPK model
    SkinFlux_0 = 1000 * (RtScMc * wsol_0 * Kpw0 / 1000) * SkinArea / mw_0  # µmoles/hour
    
    # Air-to-skin transfer
    AmntDermAir_0 = Cexp / mw_0 * Kpa0 * TotSkin / DermProt / 1000  # µmoles/hour
    
    # Dermal absorption
    dSum_Skin_Liq = SkinFlux_0 / BodyWt
    dSum_Skin_Air = DermFact * AmntDermAir_0 / BodyWt
    dM_Skin_0 = FlowOrgSkin * (Cblart_0 - C_Skin_0/R_Skin_ven_0) + SkinFlux_0 + DermFact * AmntDermAir_0 - (Vmax_Skin_0 * VolCmpSkin * C_Skin_0)/(Km_Skin_0 + C_Skin_0)
    dM_Skin_1 = FlowOrgSkin * (Cblart_1 - C_Skin_1/R_Skin_ven_1) - (Vmax_Skin_1 * VolCmpSkin * C_Skin_1)/(Km_Skin_1 + C_Skin_1) + (Vmax_p_Skin_0 * VolCmpSkin * C_Skin_0)/(Km_Skin_0 + C_Skin_0)
    dM_Skin_2 = FlowOrgSkin * (Cblart_2 - C_Skin_2/R_Skin_ven_2) - (Vmax_Skin_2 * VolCmpSkin * C_Skin_2)/(Km_Skin_2 + C_Skin_2) + (Vmax_p_Skin_1 * VolCmpSkin * C_Skin_1)/(Km_Skin_1 + C_Skin_1)
    dM_Skin_3 = FlowOrgSkin * (Cblart_3 - C_Skin_3/R_Skin_ven_3) - (Vmax_Skin_3 * VolCmpSkin * C_Skin_3)/(Km_Skin_3 + C_Skin_3) + (Vmax_p_Skin_2 * VolCmpSkin * C_Skin_2)/(Km_Skin_2 + C_Skin_2)
    dM_Skin_4 = FlowOrgSkin * (Cblart_4 - C_Skin_4/R_Skin_ven_4) - (Vmax_Skin_4 * VolCmpSkin * C_Skin_4)/(Km_Skin_4 + C_Skin_4) + (Vmax_p_Skin_3 * VolCmpSkin * C_Skin_3)/(Km_Skin_3 + C_Skin_3)
    
    dM_Lung_0 = FlowOrgLung * (Cblart_0 - C_Lung_0/R_Lung_ven_0) - (Vmax_Lung_0 * VolCmpLung * C_Lung_0)/(Km_Lung_0 + C_Lung_0)
    dM_Lung_1 = FlowOrgLung * (Cblart_1 - C_Lung_1/R_Lung_ven_1) - (Vmax_Lung_1 * VolCmpLung * C_Lung_1)/(Km_Lung_1 + C_Lung_1) + (Vmax_p_Lung_0 * VolCmpLung * C_Lung_0)/(Km_Lung_0 + C_Lung_0)
    dM_Lung_2 = FlowOrgLung * (Cblart_2 - C_Lung_2/R_Lung_ven_2) - (Vmax_Lung_2 * VolCmpLung * C_Lung_2)/(Km_Lung_2 + C_Lung_2) + (Vmax_p_Lung_1 * VolCmpLung * C_Lung_1)/(Km_Lung_1 + C_Lung_1)
    dM_Lung_3 = FlowOrgLung * (Cblart_3 - C_Lung_3/R_Lung_ven_3) - (Vmax_Lung_3 * VolCmpLung * C_Lung_3)/(Km_Lung_3 + C_Lung_3) + (Vmax_p_Lung_2 * VolCmpLung * C_Lung_2)/(Km_Lung_2 + C_Lung_2)
    dM_Lung_4 = FlowOrgLung * (Cblart_4 - C_Lung_4/R_Lung_ven_4) - (Vmax_Lung_4 * VolCmpLung * C_Lung_4)/(Km_Lung_4 + C_Lung_4) + (Vmax_p_Lung_3 * VolCmpLung * C_Lung_3)/(Km_Lung_3 + C_Lung_3)
    
    dM_LungArt_0 = CardOutp * (C_BlungArt_0 - Cblart_0)
    dM_LungArt_1 = CardOutp * (C_BlungArt_1 - Cblart_1)
    dM_LungArt_2 = CardOutp * (C_BlungArt_2 - Cblart_2)
    dM_LungArt_3 = CardOutp * (C_BlungArt_3 - Cblart_3)
    dM_LungArt_4 = CardOutp * (C_BlungArt_4 - Cblart_4)
    
    dM_LungVen_0 = CardOutp * (Cblven_0 - C_BlungVen_0) # amount in venous blood volume
    dM_LungVen_1 = CardOutp * (Cblven_1 - C_BlungVen_1) # amount in venous blood volume
    dM_LungVen_2 = CardOutp * (Cblven_2 - C_BlungVen_2) # amount in venous blood volume
    dM_LungVen_3 = CardOutp * (Cblven_3 - C_BlungVen_3) # amount in venous blood volume
    dM_LungVen_4 = CardOutp * (Cblven_4 - C_BlungVen_4) # amount in venous blood volume
    
    dΜ_Exh_0 = AlvVent * Calv_0 # micromoles
    dΜ_Exh_1 = AlvVent * Calv_1 # micromoles
    dΜ_Exh_2 = AlvVent * Calv_2 # micromoles
    dΜ_Exh_3 = AlvVent * Calv_3 # micromoles
    dΜ_Exh_4 = AlvVent * Calv_4 # micromoles
    
    dM_inhaled_0 = Cinh_0 * AlvVent # Total inhaled amount of substance in µmol
    
    dSum_Bolus = DecrBolusRt * BolusDose
    SumTissues = (M_Adip_0 + M_Adip_1 + M_Adip_2 + M_Adip_3 + M_Adip_4 +
                    M_Bone_0 + M_Bone_1 + M_Bone_2 + M_Bone_3 + M_Bone_4 +
                    M_Brain_0 + M_Brain_1 + M_Brain_2 + M_Brain_3 + M_Brain_4 +
                    M_Heart_0 + M_Heart_1 + M_Heart_2 + M_Heart_3 + M_Heart_4 +
                    M_Kidney_0 + M_Kidney_1 + M_Kidney_2 + M_Kidney_3 + M_Kidney_4 +
                    M_Intestine_0 + M_Intestine_1 + M_Intestine_2 + M_Intestine_3 + M_Intestine_4 +
                    M_Liver_0 + M_Liver_1 + M_Liver_2 + M_Liver_3 + M_Liver_4 +
                    M_Lung_0 + M_Lung_1 + M_Lung_2 + M_Lung_3 + M_Lung_4 +
                    M_Muscle_0 + M_Muscle_1 + M_Muscle_2 + M_Muscle_3 + M_Muscle_4 +
                    M_Skin_0 + M_Skin_1 + M_Skin_2 + M_Skin_3 + M_Skin_4 +
                    M_Marrow_0 + M_Marrow_1 + M_Marrow_2 + M_Marrow_3 + M_Marrow_4)/BodyWt
    
    SumExh = (Μ_Exh_0 + Μ_Exh_1 + Μ_Exh_2 + Μ_Exh_3 + Μ_Exh_4)/BodyWt
    SumUrin = (UrinExcr_0 + UrinExcr_1 + UrinExcr_2 + UrinExcr_3 + UrinExcr_4)/BodyWt
    SumCav = (M_IntCav_0 + M_IntCav_1 + M_IntCav_2 + M_IntCav_3 + M_IntCav_4)/BodyWt
    SumBlood = (M_LungArt_0 + M_LungArt_1 + M_LungArt_2 + M_LungArt_3 + M_LungArt_4 + M_LungVen_0 + M_LungVen_1 + M_LungVen_2 + M_LungVen_3 + M_LungVen_4)/BodyWt
    dSum_Skin_Air = DermFact * AmntDermAir_0 / BodyWt
    
    # Return derivatives
    return(list(c(
      # 'dM_Ven_0'=dM_Ven_0, 'dM_Ven_1'=dM_Ven_1, 'dM_Ven_2'=dM_Ven_2, 'dM_Ven_3'=dM_Ven_3, 'dM_Ven_4'=dM_Ven_4,
      "dM_Adip_0"=dM_Adip_0, "dM_Adip_1"=dM_Adip_1, "dM_Adip_2"=dM_Adip_2, "dM_Adip_3"=dM_Adip_3, "dM_Adip_4"=dM_Adip_4,
      "dM_Bone_0"=dM_Bone_0, "dM_Bone_1"=dM_Bone_1, "dM_Bone_2"=dM_Bone_2, "dM_Bone_3"=dM_Bone_3, "dM_Bone_4"=dM_Bone_4,
      "dM_Brain_0"=dM_Brain_0, "dM_Brain_1"=dM_Brain_1, "dM_Brain_2"=dM_Brain_2, "dM_Brain_3"=dM_Brain_3, "dM_Brain_4"=dM_Brain_4,
      "dM_Heart_0"=dM_Heart_0, "dM_Heart_1"=dM_Heart_1, "dM_Heart_2"=dM_Heart_2, "dM_Heart_3"=dM_Heart_3, "dM_Heart_4"=dM_Heart_4,
      "dM_Kidney_0"=dM_Kidney_0, "dM_Kidney_1"=dM_Kidney_1, "dM_Kidney_2"=dM_Kidney_2, "dM_Kidney_3"=dM_Kidney_3, "dM_Kidney_4"=dM_Kidney_4,
      "dM_Intestine_0"=dM_Intestine_0, "dM_Intestine_1"=dM_Intestine_1, "dM_Intestine_2"=dM_Intestine_2, "dM_Intestine_3"=dM_Intestine_3, "dM_Intestine_4"=dM_Intestine_4,
      "dM_Liver_0"=dM_Liver_0, "dM_Liver_1"=dM_Liver_1, "dM_Liver_2"=dM_Liver_2, "dM_Liver_3"=dM_Liver_3, "dM_Liver_4"=dM_Liver_4,
      'dCinh_0'=dCinh_0, "dM_Lung_0"=dM_Lung_0, "dM_Lung_1"=dM_Lung_1, "dM_Lung_2"=dM_Lung_2, "dM_Lung_3"=dM_Lung_3, "dM_Lung_4"=dM_Lung_4,
      "dM_Muscle_0"=dM_Muscle_0, "dM_Muscle_1"=dM_Muscle_1, "dM_Muscle_2"=dM_Muscle_2, "dM_Muscle_3"=dM_Muscle_3, "dM_Muscle_4"=dM_Muscle_4,
      "dM_Skin_0"=dM_Skin_0, "dM_Skin_1"=dM_Skin_1, "dM_Skin_2"=dM_Skin_2, "dM_Skin_3"=dM_Skin_3, "dM_Skin_4"=dM_Skin_4,
      "dDeposRate"=dDeposRate,
      "dStratCornAdd"=dStratCornAdd, "dStratCornDecr"=dStratCornDecr, "dEvapReal"=dEvapReal, "dEvapSC"=dEvapSC, "dSum_Skin_Air"=dSum_Skin_Air,"dSum_Skin_Liq"=dSum_Skin_Liq,
      "dM_Marrow_0"=dM_Marrow_0, "dM_Marrow_1"=dM_Marrow_1, "dM_Marrow_2"=dM_Marrow_2, "dM_Marrow_3"=dM_Marrow_3, "dM_Marrow_4"=dM_Marrow_4,
      "dM_LungArt_0"=dM_LungArt_0, "dM_LungArt_1"=dM_LungArt_1, "dM_LungArt_2"=dM_LungArt_2, "dM_LungArt_3"=dM_LungArt_3, "dM_LungArt_4"=dM_LungArt_4,
      "dM_LungVen_0"=dM_LungVen_0, "dM_LungVen_1"=dM_LungVen_1, "dM_LungVen_2"=dM_LungVen_2, "dM_LungVen_3"=dM_LungVen_3, "dM_LungVen_4"=dM_LungVen_4,
      "dUrinExcr_0"=dUrinExcr_0, "dUrinExcr_1"=dUrinExcr_1, "dUrinExcr_2"=dUrinExcr_2, "dUrinExcr_3"=dUrinExcr_3, "dUrinExcr_4"=dUrinExcr_4,
      "dM_inhaled_0"=dM_inhaled_0,
      "dΜ_Exh_0"=dΜ_Exh_0, "dΜ_Exh_1"=dΜ_Exh_1, "dΜ_Exh_2"=dΜ_Exh_2, "dΜ_Exh_3"=dΜ_Exh_3, "dΜ_Exh_4"=dΜ_Exh_4,
      "dM_IntCav_0"=dM_IntCav_0, "dM_IntCav_1"=dM_IntCav_1, "dM_IntCav_2"=dM_IntCav_2, "dM_IntCav_3"=dM_IntCav_3, "dM_IntCav_4"=dM_IntCav_4,
      "dBolusDose"=dBolusDose,
      "dMassSurf"=dMassSurf, "dStratCorn"=dStratCorn,
      "dUrinVol"=dUrinVol, 
      "dSum_Bolus"=dSum_Bolus
    ),
    
    # Tissue concentrations
    C_Adip_0=C_Adip_0, C_Adip_1=C_Adip_1, C_Adip_2=C_Adip_2, C_Adip_3=C_Adip_3, C_Adip_4=C_Adip_4,
    C_Bone_0=C_Bone_0, C_Bone_1=C_Bone_1, C_Bone_2=C_Bone_2, C_Bone_3=C_Bone_3, C_Bone_4=C_Bone_4,
    C_Brain_0=C_Brain_0, C_Brain_1=C_Brain_1, C_Brain_2=C_Brain_2, C_Brain_3=C_Brain_3, C_Brain_4=C_Brain_4,
    C_Heart_0=C_Heart_0, C_Heart_1=C_Heart_1, C_Heart_2=C_Heart_2, C_Heart_3=C_Heart_3, C_Heart_4=C_Heart_4,
    C_Kidney_0=C_Kidney_0, C_Kidney_1=C_Kidney_1, C_Kidney_2=C_Kidney_2, C_Kidney_3=C_Kidney_3, C_Kidney_4=C_Kidney_4,
    C_Intestine_0=C_Intestine_0, C_Intestine_1=C_Intestine_1, C_Intestine_2=C_Intestine_2, C_Intestine_3=C_Intestine_3, C_Intestine_4=C_Intestine_4,
    C_Liver_0=C_Liver_0, C_Liver_1=C_Liver_1, C_Liver_2=C_Liver_2, C_Liver_3=C_Liver_3, C_Liver_4=C_Liver_4,
    C_Lung_0=C_Lung_0, C_Lung_1=C_Lung_1, C_Lung_2=C_Lung_2, C_Lung_3=C_Lung_3, C_Lung_4=C_Lung_4,
    C_Muscle_0=C_Muscle_0, C_Muscle_1=C_Muscle_1, C_Muscle_2=C_Muscle_2, C_Muscle_3=C_Muscle_3, C_Muscle_4=C_Muscle_4,
    C_Skin_0=C_Skin_0, C_Skin_1=C_Skin_1, C_Skin_2=C_Skin_2, C_Skin_3=C_Skin_3, C_Skin_4=C_Skin_4,
    C_Marrow_0=C_Marrow_0, C_Marrow_1=C_Marrow_1, C_Marrow_2=C_Marrow_2, C_Marrow_3=C_Marrow_3, C_Marrow_4=C_Marrow_4,
    
    # Blood concentrations
    C_BlungVen_0=C_BlungVen_0, C_BlungVen_1=C_BlungVen_1, C_BlungVen_2=C_BlungVen_2, C_BlungVen_3=C_BlungVen_3, C_BlungVen_4=C_BlungVen_4,
    C_BlungArt_0=C_BlungArt_0, C_BlungArt_1=C_BlungArt_1, C_BlungArt_2=C_BlungArt_2, C_BlungArt_3=C_BlungArt_3, C_BlungArt_4=C_BlungArt_4,
    Cblart_0=Cblart_0, Cblart_1=Cblart_1, Cblart_2=Cblart_2, Cblart_3=Cblart_3, Cblart_4=Cblart_4,
    Cblven_0=Cblven_0, Cblven_1=Cblven_1, Cblven_2=Cblven_2, Cblven_3=Cblven_3, Cblven_4=Cblven_4,
    
    # Alveolar air concentrations
    Calv_0=Calv_0, Calv_1=Calv_1, Calv_2=Calv_2, Calv_3=Calv_3, Calv_4=Calv_4,
    
    # Urine concentrations
    C_Urine_0=C_Urine_0, C_Urine_1=C_Urine_1, 
    C_Urine_2=C_Urine_2, C_Urine_3=C_Urine_3, C_Urine_4=C_Urine_4,
    SumTissues=SumTissues, SumExh=SumExh, SumUrin=SumUrin, SumCav=SumCav, SumBlood=SumBlood,
    Sum_Oral_Abs=Sum_Bolus/BodyWt,
    Sum_Inhaled=M_inhaled_0/BodyWt,
    Sum_Skin_Air=Sum_Skin_Air, Sum_Skin_Liq=Sum_Skin_Liq
    ))
  })
}

create.events <- function(params){
  with(as.list(params),{
    # Initialize an empty data frame for all events
    all_events <- data.frame(var = character(),
                             time = numeric(),
                             value = numeric(),
                             method = character(),
                             stringsAsFactors = FALSE)
    
    # Oral exposure events
    if(!is.null(params$OralDose)){
      bolus_amount <- 1000 * OralDose * BodyWt / mw_0  
      bolus_events <- data.frame(var = rep("BolusDose", length(OralDose_times)),
                                 time = OralDose_times,
                                 value = bolus_amount,
                                 method = "add")
      
      # Add to all events
      all_events <- rbind(all_events, bolus_events)
    }
    
    # Inhalation exposure events
    if(!is.null(Cexp) && !is.null(Cexp_times)){
      Cinh_0_values <- Cexp / mw_0 / RespProt
      inhalation_events <- data.frame(var = rep("Cinh_0", length(Cexp_times)),
                                      time = Cexp_times,
                                      value = Cinh_0_values,
                                      method = "rep")
      
      # Add to all events
      all_events <- rbind(all_events, inhalation_events)
    }
    
    # Deposition exposure events
    if(!is.null(DeposRate) && !is.null(DeposRate_times)){
      depos_rate_values <- DeposRate
      depos_rate_events <- data.frame(var = rep("DeposRate", length(DeposRate_times)),
                                      time = DeposRate_times,
                                      value = depos_rate_values,
                                      method = "rep")
      
      # Add to all events
      all_events <- rbind(all_events, depos_rate_events)
    }
    
    # Sort all events by time
    if(nrow(all_events) > 0){
      all_events <- all_events[order(all_events$time), ]
    }
    
    return(list(data = all_events))
  })
}

custom.func <- function(){
  return()
}

# Create parameters for normal man at rest
user_input = list(
  
  # Parent Substance
  dens_0 = 740,
  mw_0 = 88.15,
  vpPa_0 = 33300,
  lkow_0 = 0.94,
  DermLKow = 0.94,
  wsol_0 = 51000,
  Resorpt_0 = -100,
  FracHpRt_0 = 0,
  
  # 1st Metabolite
  dens_1 = 781,
  mw_1 = 74.12,
  vpPa_1 = 5430,
  lkow_1 = 0.35,
  wsol_1 = 1000000,
  Resorpt_1 = -100,
  FracHpRt_1 = 0,
  
  # 2nd Metabolite
  dens_2 = 1000,
  mw_2 = 90.12,
  vpPa_2 = 30,
  lkow_2 = -0.65,
  wsol_2 = 1000000,
  Resorpt_2 = -100,
  FracHpRt_2 = 0,
  
  # 3rd Metabolite  
  dens_3 = 1000,
  mw_3 = 104.11,
  vpPa_3 = 0.01,
  lkow_3 = -4.09,
  wsol_3 = 1000000,
  Resorpt_3 = -100,
  FracHpRt_3 = 0,
  
  # 4th Metabolite  
  dens_4 = 0,
  mw_4 = 0,
  vpPa_4 = 0,
  lkow_4 = 0,
  wsol_4 = 0,
  Resorpt_4 = -100,
  FracHpRt_4 = 0,
  
  scenario = 1, 
  body_weight = 70, 
  Temp = 25,
  SkinArea=200, 
  RespProt = 1,
  DermProt = 1,
  DecrBolusRt = 3,
  N_subs = 4,
  Cexp = c(100),         # mg/m³
  Cexp_times = c(0),
  DeposRate = c(10),
  DeposRate_times = c(0),
  OralDose = c(0),        # mg/kg
  OralDose_times = c(0),
  
  #Metabolism Parameters
  # Vmax parameters
  Vmax_Adip_0 = 0, Vmax_Adip_1 = 0, Vmax_Adip_2 = 0, Vmax_Adip_3 = 0, Vmax_Adip_4 = 0,
  Vmax_Bone_0 = 0, Vmax_Bone_1 = 0, Vmax_Bone_2 = 0, Vmax_Bone_3 = 0, Vmax_Bone_4 = 0,
  Vmax_Brain_0 = 0, Vmax_Brain_1 = 0, Vmax_Brain_2 = 0, Vmax_Brain_3 = 0, Vmax_Brain_4 = 0,
  Vmax_Heart_0 = 0, Vmax_Heart_1 = 0, Vmax_Heart_2 = 0, Vmax_Heart_3 = 0, Vmax_Heart_4 = 0,
  Vmax_Kidney_0 = 0, Vmax_Kidney_1 = 0, Vmax_Kidney_2 = 0, Vmax_Kidney_3 = 0, Vmax_Kidney_4 = 0,
  Vmax_Intestine_0 = 0, Vmax_Intestine_1 = 0, Vmax_Intestine_2 = 0, Vmax_Intestine_3 = 0, Vmax_Intestine_4 = 0,
  Vmax_Liver_0 = 500, Vmax_Liver_1 = 300, Vmax_Liver_2 = 300, Vmax_Liver_3 = 0, Vmax_Liver_4 = 0,
  Vmax_Lung_0 = 0, Vmax_Lung_1 = 0, Vmax_Lung_2 = 0, Vmax_Lung_3 = 0, Vmax_Lung_4 = 0,
  Vmax_Muscle_0 = 0, Vmax_Muscle_1 = 0, Vmax_Muscle_2 = 0, Vmax_Muscle_3 = 0, Vmax_Muscle_4 = 0,
  Vmax_Skin_0 = 0, Vmax_Skin_1 = 0, Vmax_Skin_2 = 0, Vmax_Skin_3 = 0, Vmax_Skin_4 = 0,
  Vmax_Marrow_0 = 0, Vmax_Marrow_1 = 0, Vmax_Marrow_2 = 0, Vmax_Marrow_3 = 0, Vmax_Marrow_4 = 0,
  
  Vmax_p_Adip_0 = 0, Vmax_p_Adip_1 = 0, Vmax_p_Adip_2 = 0, Vmax_p_Adip_3 = 0, Vmax_p_Adip_4 = 0,
  Vmax_p_Bone_0 = 0, Vmax_p_Bone_1 = 0, Vmax_p_Bone_2 = 0, Vmax_p_Bone_3 = 0, Vmax_p_Bone_4 = 0,
  Vmax_p_Brain_0 = 0, Vmax_p_Brain_1 = 0, Vmax_p_Brain_2 = 0, Vmax_p_Brain_3 = 0, Vmax_p_Brain_4 = 0,
  Vmax_p_Heart_0 = 0, Vmax_p_Heart_1 = 0, Vmax_p_Heart_2 = 0, Vmax_p_Heart_3 = 0, Vmax_p_Heart_4 = 0,
  Vmax_p_Kidney_0 = 0, Vmax_p_Kidney_1 = 0, Vmax_p_Kidney_2 = 0, Vmax_p_Kidney_3 = 0, Vmax_p_Kidney_4 = 0,
  Vmax_p_Intestine_0 = 0, Vmax_p_Intestine_1 = 0, Vmax_p_Intestine_2 = 0, Vmax_p_Intestine_3 = 0, Vmax_p_Intestine_4 = 0,
  Vmax_p_Liver_0 = 500, Vmax_p_Liver_1 = 0, Vmax_p_Liver_2 = 0, Vmax_p_Liver_3 = 0, Vmax_p_Liver_4 = 0,
  Vmax_p_Lung_0 = 0, Vmax_p_Lung_1 = 0, Vmax_p_Lung_2 = 0, Vmax_p_Lung_3 = 0, Vmax_p_Lung_4 = 0,
  Vmax_p_Muscle_0 = 0, Vmax_p_Muscle_1 = 0, Vmax_p_Muscle_2 = 0, Vmax_p_Muscle_3 = 0, Vmax_p_Muscle_4 = 0,
  Vmax_p_Skin_0 = 0, Vmax_p_Skin_1 = 0, Vmax_p_Skin_2 = 0, Vmax_p_Skin_3 = 0, Vmax_p_Skin_4 = 0,
  Vmax_p_Marrow_0 = 0, Vmax_p_Marrow_1 = 0, Vmax_p_Marrow_2 = 0, Vmax_p_Marrow_3 = 0, Vmax_p_Marrow_4 = 0,
  
  # Km parameters
  Km_Adip_0 = 1, Km_Adip_1 = 1, Km_Adip_2 = 1, Km_Adip_3 = 1, Km_Adip_4 = 1,
  Km_Bone_0 = 1, Km_Bone_1 = 1, Km_Bone_2 = 1, Km_Bone_3 = 1, Km_Bone_4 = 1,
  Km_Brain_0 = 1, Km_Brain_1 = 1, Km_Brain_2 = 1, Km_Brain_3 = 1, Km_Brain_4 = 1,
  Km_Heart_0 = 1, Km_Heart_1 = 1, Km_Heart_2 = 1, Km_Heart_3 = 1, Km_Heart_4 = 1,
  Km_Kidney_0 = 1, Km_Kidney_1 = 1, Km_Kidney_2 = 1, Km_Kidney_3 = 1, Km_Kidney_4 = 1,
  Km_Intestine_0 = 1, Km_Intestine_1 = 1, Km_Intestine_2 = 1, Km_Intestine_3 = 1, Km_Intestine_4 = 1,
  Km_Liver_0 = 50, Km_Liver_1 = 1, Km_Liver_2 = 1, Km_Liver_3 = 1, Km_Liver_4 = 1,
  Km_Lung_0 = 1, Km_Lung_1 = 1, Km_Lung_2 = 1, Km_Lung_3 = 1, Km_Lung_4 = 1,
  Km_Muscle_0 = 1, Km_Muscle_1 = 1, Km_Muscle_2 = 1, Km_Muscle_3 = 1, Km_Muscle_4 = 1,
  Km_Skin_0 = 1, Km_Skin_1 = 1, Km_Skin_2 = 1, Km_Skin_3 = 1, Km_Skin_4 = 1,
  Km_Marrow_0 = 1, Km_Marrow_1 = 1, Km_Marrow_2 = 1, Km_Marrow_3 = 1, Km_Marrow_4 = 1
)
params <- create.params(user_input = user_input)
initial_state = create.inits(params)
events <- create.events(params)
times = seq(0, 96, by=1)

# Run the ODE solver with events
out <- data.frame(ode(times = times,
                      func = ode.func, y = initial_state, parms = params,
                      events = events,
                      method="lsodes",
                      rtol = 1e-05,
                      atol = 1e-05))
print(tail(out,2))
#====================
#7. Upload on Jaqpot 
#===================


# Subset of features to be displayed on the user interface
predicted.feats <- c(
  "M_Adip_0", "M_Adip_1", "M_Adip_2", "M_Adip_3", "M_Adip_4",
  "M_Bone_0", "M_Bone_1", "M_Bone_2", "M_Bone_3", "M_Bone_4",
  "M_Brain_0", "M_Brain_1", "M_Brain_2", "M_Brain_3", "M_Brain_4",
  "M_Heart_0", "M_Heart_1", "M_Heart_2", "M_Heart_3", "M_Heart_4",
  "M_Kidney_0", "M_Kidney_1", "M_Kidney_2", "M_Kidney_3", "M_Kidney_4",
  "M_Intestine_0", "M_Intestine_1", "M_Intestine_2", "M_Intestine_3", "M_Intestine_4",
  "M_Liver_0", "M_Liver_1", "M_Liver_2", "M_Liver_3", "M_Liver_4",
  "Cinh_0", "M_Lung_0", "M_Lung_1", "M_Lung_2", "M_Lung_3", "M_Lung_4",
  "M_Muscle_0", "M_Muscle_1", "M_Muscle_2", "M_Muscle_3", "M_Muscle_4",
  "M_Skin_0", "M_Skin_1", "M_Skin_2", "M_Skin_3", "M_Skin_4",
  "DeposRate",
  "StratCornAdd", "StratCornDecr", "EvapReal", "EvapSC", "Sum_Skin_Air", "Sum_Skin_Liq",
  "M_Marrow_0", "M_Marrow_1", "M_Marrow_2", "M_Marrow_3", "M_Marrow_4",
  "M_LungArt_0", "M_LungArt_1", "M_LungArt_2", "M_LungArt_3", "M_LungArt_4",
  "M_LungVen_0", "M_LungVen_1", "M_LungVen_2", "M_LungVen_3", "M_LungVen_4",
  "UrinExcr_0", "UrinExcr_1", "UrinExcr_2", "UrinExcr_3", "UrinExcr_4",
  "M_inhaled_0",
  "Μ_Exh_0", "Μ_Exh_1", "Μ_Exh_2", "Μ_Exh_3", "Μ_Exh_4",
  "M_IntCav_0", "M_IntCav_1", "M_IntCav_2", "M_IntCav_3", "M_IntCav_4",
  "BolusDose",
  "MassSurf", "StratCorn",
  "UrinVol",
  "Sum_Bolus",
  
  "C_Adip_0", "C_Adip_1", "C_Adip_2", "C_Adip_3", "C_Adip_4",
  "C_Bone_0", "C_Bone_1", "C_Bone_2", "C_Bone_3", "C_Bone_4",
  "C_Brain_0", "C_Brain_1", "C_Brain_2", "C_Brain_3", "C_Brain_4",
  "C_Heart_0", "C_Heart_1", "C_Heart_2", "C_Heart_3", "C_Heart_4",
  "C_Kidney_0", "C_Kidney_1", "C_Kidney_2", "C_Kidney_3", "C_Kidney_4",
  "C_Intestine_0", "C_Intestine_1", "C_Intestine_2", "C_Intestine_3", "C_Intestine_4",
  "C_Liver_0", "C_Liver_1", "C_Liver_2", "C_Liver_3", "C_Liver_4",
  "C_Lung_0", "C_Lung_1", "C_Lung_2", "C_Lung_3", "C_Lung_4",
  "C_Muscle_0", "C_Muscle_1", "C_Muscle_2", "C_Muscle_3", "C_Muscle_4",
  "C_Skin_0", "C_Skin_1", "C_Skin_2", "C_Skin_3", "C_Skin_4",
  "C_Marrow_0", "C_Marrow_1", "C_Marrow_2", "C_Marrow_3", "C_Marrow_4",
  
  "C_BlungVen_0", "C_BlungVen_1", "C_BlungVen_2", "C_BlungVen_3", "C_BlungVen_4",
  "C_BlungArt_0", "C_BlungArt_1", "C_BlungArt_2", "C_BlungArt_3", "C_BlungArt_4",
  "Cblart_0", "Cblart_1", "Cblart_2", "Cblart_3", "Cblart_4",
  "Cblven_0", "Cblven_1", "Cblven_2", "Cblven_3", "Cblven_4",
  
  "Calv_0", "Calv_1", "Calv_2", "Calv_3", "Calv_4",
  
  "C_Urine_0", "C_Urine_1", "C_Urine_2", "C_Urine_3", "C_Urine_4",
  "SumTissues", "SumExh", "SumUrin", "SumCav", "SumBlood",
  "Sum_Oral_Abs", "Sum_Inhaled",
  "Sum_Skin_Air", "Sum_Skin_Liq"
)

# Deploy the model on the Jaqpot server to create a web service
jaqpotr::deploy.pbpk(user.input = user_input,out.vars = predicted.feats,
                     create.params = create.params,  create.inits = create.inits,
                     create.events = create.events, custom.func = custom.func,
                     envFile = "/Users/vassilis/Documents/GitHub/jaqpotpy/.env")