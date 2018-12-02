/*
RUN kalkyle_mva_korr.p (ArtBas.ArtikkelNr).
*/

DEF INPUT        PARAMETER lArtikkelNr  AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntArtPris  AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntPrisKo   AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntBestHode AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iAntVpiPris  AS INT NO-UNDO.

FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
    RETURN.
ELSE 
KORREKSJON:
DO:
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VarGr THEN 
        LEAVE KORREKSJON.
    FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Moms THEN 
        LEAVE KORREKSJON.

    FOR EACH artpris OF artbas:
        IF ArtBas.OPris = TRUE THEN DO:
            /* Vi rättar upp eventuella fel i kalkylen samt sätter riktig moms och nytt pris */
            ASSIGN ArtPris.DB%[1]          = 100 - VarGr.Kost_Proc
                   ArtPris.DBKr[1]         = 100 - VarGr.Kost_Proc
                   ArtPris.EuroManuel      = FALSE
                   ArtPris.Mva%[1]         = Moms.MomsProc
                   ArtPris.MvaKr[1]        = Moms.MomsProc
                   ArtPris.Pris[1]         = 100 + Moms.MomsProc
                   ArtPris.ValPris[1]      = VarGr.Kost_Proc
                   ArtPris.InnkjopsPris[1] = VarGr.Kost_Proc
                   ArtPris.VareKost[1]     = VarGr.Kost_Proc
                   iAntArtPris = iAntArtPris + 1.
        END.
        ELSE DO:
            IF ArtPris.Mva%[1] <> Moms.MomsProc THEN
                ASSIGN ArtPris.MvaKr[1] = ArtPris.Pris[1] * Moms.MomsProc / (100 + Moms.MomsProc)
                       ArtPris.Mva%[1]  = Moms.MomsProc
                       ArtPris.DBKr[1]  = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
                       ArtPris.DB%[1]   = ROUND(ArtPris.DBKr[1] / (ArtPris.Pris[1] - ArtPris.MvaKr[1]) * 100,2)
                       iAntArtPris = iAntArtPris + 1.
                .
            IF ArtPris.Pris[2] <> 0 AND ArtPris.Mva%[2] <> Moms.MomsProc THEN
                ASSIGN ArtPris.MvaKr[2] = ArtPris.Pris[2] * Moms.MomsProc / (100 + Moms.MomsProc)
                       ArtPris.Mva%[2]  = Moms.MomsProc
                       ArtPris.DBKr[2]  = ArtPris.Pris[2] - ArtPris.MvaKr[2] - ArtPris.VareKost[2]
                       ArtPris.DB%[2]   = ROUND(ArtPris.DBKr[2] / (ArtPris.Pris[2] - ArtPris.MvaKr[2]) * 100,2)
                       iAntArtPris = iAntArtPris + 1.
        END.
    END.
    FOR EACH PrisKo OF artbas WHERE PrisKo.Mva% <> Moms.MomsProc:
        ASSIGN PrisKo.MvaKr = PrisKo.Pris * Moms.MomsProc / (100 + Moms.MomsProc)
               PrisKo.Mva%  = Moms.MomsProc
               PrisKo.DBKr  = PrisKo.Pris - PrisKo.MvaKr - PrisKo.VareKost
               PrisKo.DB%   = ROUND(PrisKo.DBKr / (PrisKo.Pris - PrisKo.MvaKr) * 100,2).
        ASSIGN iAntPrisKo = iAntPrisKo + 1.
    END.
    FOR EACH BestHode OF artbas WHERE beststat < 6 NO-LOCK:
        FOR EACH BestPris OF Besthode WHERE BestPris.Mva% <> Moms.MomsProc:
            ASSIGN BestPris.MvaKr = BestPris.Pris * Moms.MomsProc / (100 + Moms.MomsProc)
                   BestPris.Mva%  = Moms.MomsProc
                   BestPris.DBKr  = BestPris.Pris - BestPris.MvaKr - BestPris.VareKost
                   BestPris.DB%   = ROUND(BestPris.DBKr / (BestPris.Pris - BestPris.MvaKr) * 100,2).
            ASSIGN iAntBestHode = iAntBestHode + 1.
        END.
    END.
    
    FOR EACH VPIArtBas NO-LOCK WHERE 
        VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr:
        FOR EACH VPIArtPris OF VPIArtBas:
            IF VPIArtBas.OPris = TRUE THEN DO:
                /* Vi rättar upp eventuella fel i kalkylen samt sätter riktig moms och nytt pris */
                ASSIGN VPIArtPris.DB%[1]          = 100 - VarGr.Kost_Proc
                       VPIArtPris.DBKr[1]         = 100 - VarGr.Kost_Proc
                       VPIArtPris.EuroManuel      = FALSE
                       VPIArtPris.Mva%[1]         = Moms.MomsProc
                       VPIArtPris.MvaKr[1]        = Moms.MomsProc
                       VPIArtPris.Pris[1]         = 100 + Moms.MomsProc
                       VPIArtPris.ValPris[1]      = VarGr.Kost_Proc
                       VPIArtPris.InnkjopsPris[1] = VarGr.Kost_Proc
                       VPIArtPris.VareKost[1]     = VarGr.Kost_Proc.
            END.
            ELSE DO:
                IF VPIArtPris.Mva%[1] <> Moms.MomsProc THEN
                    ASSIGN VPIArtPris.MvaKr[1] = VPIArtPris.Pris[1] * Moms.MomsProc / (100 + Moms.MomsProc)
                           VPIArtPris.Mva%[1]  = Moms.MomsProc
                           VPIArtPris.DBKr[1]  = VPIArtPris.Pris[1] - VPIArtPris.MvaKr[1] - VPIArtPris.VareKost[1]
                           VPIArtPris.DB%[1]   = ROUND(VPIArtPris.DBKr[1] / (VPIArtPris.Pris[1] - VPIArtPris.MvaKr[1]) * 100,2).
                IF VPIArtPris.Pris[2] <> 0 AND VPIArtPris.Mva%[2] <> Moms.MomsProc THEN
                    ASSIGN VPIArtPris.MvaKr[2] = VPIArtPris.Pris[2] * Moms.MomsProc / (100 + Moms.MomsProc)
                           VPIArtPris.Mva%[2]  = Moms.MomsProc
                           VPIArtPris.DBKr[2]  = VPIArtPris.Pris[2] - VPIArtPris.MvaKr[2] - VPIArtPris.VareKost[2]
                           VPIArtPris.DB%[2]   = ROUND(VPIArtPris.DBKr[2] / (VPIArtPris.Pris[2] - VPIArtPris.MvaKr[2]) * 100,2).
            END.
            ASSIGN iAntVpiPris = iAntVpiPris + 1.
        END.
    END.
END. /* KORREKSJON */
