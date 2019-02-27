DEFINE VARIABLE dDB%     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dDBKr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMva%    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaKr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPris    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dValpris AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dInpris  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dVarekost AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPrisUMoms AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMomsMarg AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMomsen AS DECIMAL    NO-UNDO.
FOR EACH tt_vare BREAK BY Modell.
    IF FIRST-OF(modell) THEN DO:
        ASSIGN dMvaKr     = ROUND((TT_Vare.Mva / (100 + TT_Vare.Mva)) * TT_Vare.utprisn,2)
               dDBKr      = TT_Vare.utprisn - dMvaKr - TT_Vare.nettokr
               dDB%       = round(100 * dDBKr / (TT_Vare.utprisn - dMvaKr),2)
               dMva%      = TT_Vare.Mva
               dPris      = TT_Vare.utprisn
               dValpris   = TT_Vare.nettokr
               dInpris    = TT_Vare.nettokr
               dVarekost  = TT_Vare.nettokr
               dPrisUMoms = TT_Vare.utprisn - dMvaKr.
            .
        CREATE ArtPris.
        ASSIGN  /* nyckelfält */
            ArtPris.ArtikkelNr = TT_Vare.modell
            ArtPris.ProfilNr   = 1.
        ASSIGN
            ArtPris.AktivFraDato    = TODAY
            ArtPris.DB%[1]          = dDB%
            ArtPris.DBKr[1]         = dDBKr
            ArtPris.EuroManuel      = TRUE
            ArtPris.LevNr           = TT_Vare.Levnr
            ArtPris.Mva%[1]         = TT_Vare.Mva
            ArtPris.MvaKr[1]        = dMvaKr
            ArtPris.Pris[1]         = TT_Vare.utprisn
            ArtPris.ValPris[1]      = TT_Vare.nettokr
            ArtPris.InnkjopsPris[1] = TT_Vare.nettokr
            ArtPris.VareKost[1]     = TT_Vare.nettokr.
    END.
END.
