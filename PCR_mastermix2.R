RNA_samps <- c(577.03, 123.04, 228.23, 241.94, 155.21, 74.65, 114.82,
               114.48, 529.03, 246.07, 441.38)


names(RNA_samps) <- c("eyGAL4,Dl;yw",
                      "eyGAL4;dop-PDZ",
                      "eyGAL4;dop-DUF",
                      "eyGAL4;dop-RNAi-v35100",
                      "eyGAL4;dop-GFP",
                      "eyGAL4;dop-RNAi-BL44547",
                      "eyGAL4;dop-kin",
                      "eyGAL4;dop-HA",
                      "eyGAL4;CG10147-RNAi-v31183",
                      "eyGAL4;dop-RNAi-BL27698",
                      "eyGAL4;CG10147-RNAi-v18787")


## labels ("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
##                         "L", "M")



cDNA_synthesis <- function(x, vol_ul, per_reaction_ng = 20, amt_cDNA_ng, mix_5X_ul 
                         = 4,enzyme_10X_ul = 2, exp_fin_vol = 20){
   
    # x is the dataset, vol_ul: the volume of RNA soln.,per_reaction: amount of
    # RNA used for each qPCR reaction
    # amt_cDNA_ng: amount of cDNA after RT,
    # mix_5X_ul & enzyme_10X_ul: according to SuperScript VILO cDNA protocol,
    # exp_fin_vol: final vol of reaction

    total_amount_ng <- round(x * vol_ul, 2)
    no_rxns <- round(total_amount_ng/per_reaction_ng, 2)
    total_amount_ug <- round(total_amount_ng / 1000, 2)
    rem_RNA_amt <- total_amount_ng - amt_cDNA_ng
    vol_cDNA <- round(amt_cDNA_ng/x, 2)
    H2O_vol <- 20 - (vol_cDNA + mix_5X_ul + enzyme_10X_ul)
    obs_fin_vol <- vol_cDNA + mix_5X_ul + enzyme_10X_ul + H2O_vol
    suff_RNA <- (total_amount_ng >= amt_cDNA_ng)

     final_output <- data.frame(x, total_amount_ug, no_rxns, rem_RNA_amt,
                                vol_cDNA, mix_5X_ul, enzyme_10X_ul, H2O_vol, 
                                obs_fin_vol, suff_RNA)
    colnames(final_output) <- c("RNA_conc_ng/ul", "total_amount_ug",
                                "number_rxns", "amt_RNA_left_ng", "vol_cDNA_ul", 
                                "5X_mix_ul", "10X_enzyme_ul", "H2O_vol_ul", 
                                "final_vol", "sufficient RNA?")
   return(final_output)
}

