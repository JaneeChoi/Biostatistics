predictor_multiple = c('ASSIIDELFQDR', 'TLLSNLEEAK', 'NADYSYSVWK', 'GGSASTWLTAFALR', 'DLLLPQPDLR', 'VAAGAFQGLR', 'AGFSWIEVTFK', 'LALDNGGLAR', 'ILSGDPYCEK', 'LSSGLVTAALYGR',
                       'NIQSLEVIGK', 'ICLDPDAPR', 'IEDGFSLK', 'VAEGTQVLELPFK', 'GLTSVINQK', 'IVVVTAGVR', 'GEWVALNPLR', 'TGESVEFVCK', 'LSLEIEQLELQR', 'TWYPEVPK', 
                       'EFGNTLEDK', 'EWFSETFQK', 'GFLLLASLR', 'TIVTTLQDSIR', 'ALAQCAPPPAVCAELVR', 'YGQPLPGYTTK')
predictor_multiple_gene = c('CLU', 'C5', 'LRG1', 'ITIH4', 'BTD', 'PPBP', 'SERPINC1', 'LDHB', 'CFH', 'C4BPA', 'APOC1', 'THBS1', 'IGFBP3')
predictor_gene = c(features[!features %in% predictor_multiple], predictor_multiple_gene)

length(predictor_multiple_gene)
peptide = c('AADDTWEPFASGK', 'DSVTGTLPK', 'LIQGAPTIR', 'NNLELSTPLK', 'VFSLQWGEVK', 'DYFIATCK', 'ALLAFQESK', 'VTGVVLFR', 'FQASVATPR', 'IQPSGGTNINEALLR',
            'DGYLFQLLR', 'VSTLPAITLK', 'VLFYVDSEK', 'TLNICEVGTIR', 'VCPFAGILENGAVR', 'DPTFIPAPIQAK', 'GLIDLTLDK', 'GDIGETGVPGAEGPR', 'ELLALIQLER', 'TNFDNDIALVR',
            'ASCLYGQLPK', 'CDLISIPK', 'ILLAELEQLK', 'DTEVLLVGLEPGTR', 'CDENILWLDYK', 'FAVLQENVAWGNGR', 'LSNNALSGLPQGVFGK', 'GLPGEVLGAQPGPR', 'GFQQLLQELNQPR', 'CINQLLCK',
            'LLGIETPLPK')
gene = c('TTR', 'KLKB1', 'IGFBP2', 'PROS1', 'CFI', 'C1R', 'C4BPB', 'SOD3', 'MBL2', 'ITIH2',
         'HRG', 'CTSD', 'C7', 'C6', 'APOH', 'AGT', 'IFRD1', 'ADIPOQ', 'ECM1', 'C1S',
         'GSTP1', 'CORO1C', 'VIM', 'PTPRJ', 'PKM2', 'FCGBP', 'CPN2', 'COL4A2', 'SERPINA5', 'SEPP1',
         'ICAM1')

length(gene)
length(peptide)

predictor_gene_only_gene = sapply(predictor_gene, 
                                  FUN = function(x) {
                                    if (x %in% peptide) return(gene[which(x == peptide)])
                                    return(x)
                                  })
names(predictor_gene_only_gene) = NULL

pca_CLU      = prcomp(AACR_transformed_tr_stdtogether[c('ASSIIDELFQDR', 'TLLSNLEEAK')], center = FALSE, scale = FALSE)
pca_C5       = prcomp(AACR_transformed_tr_stdtogether[c('NADYSYSVWK', 'GGSASTWLTAFALR')], center = FALSE, scale = FALSE)
pca_LRG1     = prcomp(AACR_transformed_tr_stdtogether[c('DLLLPQPDLR', 'VAAGAFQGLR')], center = FALSE, scale = FALSE)
pca_ITIH4    = prcomp(AACR_transformed_tr_stdtogether[c('AGFSWIEVTFK', 'LALDNGGLAR')], center = FALSE, scale = FALSE)
pca_BTD      = prcomp(AACR_transformed_tr_stdtogether[c('ILSGDPYCEK', 'LSSGLVTAALYGR')], center = FALSE, scale = FALSE)
pca_PPBP     = prcomp(AACR_transformed_tr_stdtogether[c('NIQSLEVIGK', 'ICLDPDAPR')], center = FALSE, scale = FALSE)
pca_SERPINC1 = prcomp(AACR_transformed_tr_stdtogether[c('IEDGFSLK', 'VAEGTQVLELPFK')], center = FALSE, scale = FALSE)
pca_LDHB     = prcomp(AACR_transformed_tr_stdtogether[c('GLTSVINQK', 'IVVVTAGVR')], center = FALSE, scale = FALSE)
pca_CFH      = prcomp(AACR_transformed_tr_stdtogether[c('GEWVALNPLR', 'TGESVEFVCK')], center = FALSE, scale = FALSE)
pca_C4BPA    = prcomp(AACR_transformed_tr_stdtogether[c('LSLEIEQLELQR', 'TWYPEVPK')], center = FALSE, scale = FALSE)
pca_APOC1    = prcomp(AACR_transformed_tr_stdtogether[c('EFGNTLEDK', 'EWFSETFQK')], center = FALSE, scale = FALSE)
pca_THBS1    = prcomp(AACR_transformed_tr_stdtogether[c('GFLLLASLR', 'TIVTTLQDSIR')], center = FALSE, scale = FALSE)
pca_IGFBP3   = prcomp(AACR_transformed_tr_stdtogether[c('ALAQCAPPPAVCAELVR', 'YGQPLPGYTTK')], center = FALSE, scale = FALSE)

AACR_transformed_tr_stdtogether_tr_multiple = AACR_transformed_tr_stdtogether_tr
AACR_transformed_tr_stdtogether_tr_multiple[['CLU']]      = as.matrix(AACR_transformed_tr_stdtogether_tr[c('ASSIIDELFQDR', 'TLLSNLEEAK')]) %*% pca_CLU[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['C5']]       = as.matrix(AACR_transformed_tr_stdtogether_tr[c('NADYSYSVWK', 'GGSASTWLTAFALR')]) %*% pca_C5[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['LRG1']]     = as.matrix(AACR_transformed_tr_stdtogether_tr[c('DLLLPQPDLR', 'VAAGAFQGLR')]) %*% pca_LRG1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['ITIH4']]    = as.matrix(AACR_transformed_tr_stdtogether_tr[c('AGFSWIEVTFK', 'LALDNGGLAR')]) %*% pca_ITIH4[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['BTD']]      = as.matrix(AACR_transformed_tr_stdtogether_tr[c('ILSGDPYCEK', 'LSSGLVTAALYGR')]) %*% pca_BTD[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['PPBP']]     = as.matrix(AACR_transformed_tr_stdtogether_tr[c('NIQSLEVIGK', 'ICLDPDAPR')]) %*% pca_PPBP[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['SERPINC1']] = as.matrix(AACR_transformed_tr_stdtogether_tr[c('IEDGFSLK', 'VAEGTQVLELPFK')]) %*% pca_SERPINC1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['LDHB']]     = as.matrix(AACR_transformed_tr_stdtogether_tr[c('GLTSVINQK', 'IVVVTAGVR')]) %*% pca_LDHB[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['CFH']]      = as.matrix(AACR_transformed_tr_stdtogether_tr[c('GEWVALNPLR', 'TGESVEFVCK')]) %*% pca_CFH[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['C4BPA']]    = as.matrix(AACR_transformed_tr_stdtogether_tr[c('LSLEIEQLELQR', 'TWYPEVPK')]) %*% pca_C4BPA[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['APOC1']]    = as.matrix(AACR_transformed_tr_stdtogether_tr[c('EFGNTLEDK', 'EWFSETFQK')]) %*% pca_APOC1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['THBS1']]    = as.matrix(AACR_transformed_tr_stdtogether_tr[c('GFLLLASLR', 'TIVTTLQDSIR')]) %*% pca_THBS1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_tr_multiple[['IGFBP3']]   = as.matrix(AACR_transformed_tr_stdtogether_tr[c('ALAQCAPPPAVCAELVR', 'YGQPLPGYTTK')]) %*% pca_IGFBP3[['rotation']][,1, drop = FALSE]

AACR_transformed_tr_stdtogether_validation_multiple = AACR_transformed_tr_stdtogether_validation
AACR_transformed_tr_stdtogether_validation_multiple[['CLU']]      = as.matrix(AACR_transformed_tr_stdtogether_validation[c('ASSIIDELFQDR', 'TLLSNLEEAK')]) %*% pca_CLU[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['C5']]       = as.matrix(AACR_transformed_tr_stdtogether_validation[c('NADYSYSVWK', 'GGSASTWLTAFALR')]) %*% pca_C5[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['LRG1']]     = as.matrix(AACR_transformed_tr_stdtogether_validation[c('DLLLPQPDLR', 'VAAGAFQGLR')]) %*% pca_LRG1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['ITIH4']]    = as.matrix(AACR_transformed_tr_stdtogether_validation[c('AGFSWIEVTFK', 'LALDNGGLAR')]) %*% pca_ITIH4[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['BTD']]      = as.matrix(AACR_transformed_tr_stdtogether_validation[c('ILSGDPYCEK', 'LSSGLVTAALYGR')]) %*% pca_BTD[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['PPBP']]     = as.matrix(AACR_transformed_tr_stdtogether_validation[c('NIQSLEVIGK', 'ICLDPDAPR')]) %*% pca_PPBP[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['SERPINC1']] = as.matrix(AACR_transformed_tr_stdtogether_validation[c('IEDGFSLK', 'VAEGTQVLELPFK')]) %*% pca_SERPINC1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['LDHB']]     = as.matrix(AACR_transformed_tr_stdtogether_validation[c('GLTSVINQK', 'IVVVTAGVR')]) %*% pca_LDHB[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['CFH']]      = as.matrix(AACR_transformed_tr_stdtogether_validation[c('GEWVALNPLR', 'TGESVEFVCK')]) %*% pca_CFH[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['C4BPA']]    = as.matrix(AACR_transformed_tr_stdtogether_validation[c('LSLEIEQLELQR', 'TWYPEVPK')]) %*% pca_C4BPA[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['APOC1']]    = as.matrix(AACR_transformed_tr_stdtogether_validation[c('EFGNTLEDK', 'EWFSETFQK')]) %*% pca_APOC1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['THBS1']]    = as.matrix(AACR_transformed_tr_stdtogether_validation[c('GFLLLASLR', 'TIVTTLQDSIR')]) %*% pca_THBS1[['rotation']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_validation_multiple[['IGFBP3']]   = as.matrix(AACR_transformed_tr_stdtogether_validation[c('ALAQCAPPPAVCAELVR', 'YGQPLPGYTTK')]) %*% pca_IGFBP3[['rotation']][,1, drop = FALSE]

AACR_transformed_te_stdtogether_multiple = AACR_transformed_te_stdtogether
AACR_transformed_te_stdtogether_multiple[['CLU']]      = as.matrix(AACR_transformed_te_stdtogether[c('ASSIIDELFQDR', 'TLLSNLEEAK')]) %*% pca_CLU[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['C5']]       = as.matrix(AACR_transformed_te_stdtogether[c('NADYSYSVWK', 'GGSASTWLTAFALR')]) %*% pca_C5[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['LRG1']]     = as.matrix(AACR_transformed_te_stdtogether[c('DLLLPQPDLR', 'VAAGAFQGLR')]) %*% pca_LRG1[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['ITIH4']]    = as.matrix(AACR_transformed_te_stdtogether[c('AGFSWIEVTFK', 'LALDNGGLAR')]) %*% pca_ITIH4[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['BTD']]      = as.matrix(AACR_transformed_te_stdtogether[c('ILSGDPYCEK', 'LSSGLVTAALYGR')]) %*% pca_BTD[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['PPBP']]     = as.matrix(AACR_transformed_te_stdtogether[c('NIQSLEVIGK', 'ICLDPDAPR')]) %*% pca_PPBP[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['SERPINC1']] = as.matrix(AACR_transformed_te_stdtogether[c('IEDGFSLK', 'VAEGTQVLELPFK')]) %*% pca_SERPINC1[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['LDHB']]     = as.matrix(AACR_transformed_te_stdtogether[c('GLTSVINQK', 'IVVVTAGVR')]) %*% pca_LDHB[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['CFH']]      = as.matrix(AACR_transformed_te_stdtogether[c('GEWVALNPLR', 'TGESVEFVCK')]) %*% pca_CFH[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['C4BPA']]    = as.matrix(AACR_transformed_te_stdtogether[c('LSLEIEQLELQR', 'TWYPEVPK')]) %*% pca_C4BPA[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['APOC1']]    = as.matrix(AACR_transformed_te_stdtogether[c('EFGNTLEDK', 'EWFSETFQK')]) %*% pca_APOC1[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['THBS1']]    = as.matrix(AACR_transformed_te_stdtogether[c('GFLLLASLR', 'TIVTTLQDSIR')]) %*% pca_THBS1[['rotation']][,1, drop = FALSE]
AACR_transformed_te_stdtogether_multiple[['IGFBP3']]   = as.matrix(AACR_transformed_te_stdtogether[c('ALAQCAPPPAVCAELVR', 'YGQPLPGYTTK')]) %*% pca_IGFBP3[['rotation']][,1, drop = FALSE]

AACR_transformed_tr_stdtogether_multiple = AACR_transformed_tr_stdtogether
AACR_transformed_tr_stdtogether_multiple[['CLU']]      = pca_CLU[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['C5']]       = pca_C5[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['LRG1']]     = pca_LRG1[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['ITIH4']]    = pca_ITIH4[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['BTD']]      = pca_BTD[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['PPBP']]     = pca_PPBP[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['SERPINC1']] = pca_SERPINC1[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['LDHB']]     = pca_LDHB[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['CFH']]      = pca_CFH[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['C4BPA']]    = pca_C4BPA[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['APOC1']]    = pca_APOC1[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['THBS1']]    = pca_THBS1[['x']][,1, drop = FALSE]
AACR_transformed_tr_stdtogether_multiple[['IGFBP3']]   = pca_IGFBP3[['x']][,1, drop = FALSE]


a = sapply(colnames(AACR_transformed_tr_stdtogether_tr_multiple), 
           FUN = function(x) {
             if (x %in% peptide) return(gene[which(x == peptide)])
             return(x)
           })
names(a) = NULL

colnames(AACR_transformed_tr_stdtogether_tr_multiple) = a
colnames(AACR_transformed_tr_stdtogether_validation_multiple) = a
colnames(AACR_transformed_te_stdtogether_multiple) = a
colnames(AACR_transformed_tr_stdtogether_multiple) = a
rm(a)