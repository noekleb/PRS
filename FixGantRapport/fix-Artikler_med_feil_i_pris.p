DEF VAR iAnt AS INT NO-UNDO.
DEF VAR iLagant AS INT NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ON WRITE  OF ArtPris OVERRIDE DO: END.  

DEF STREAM UtFil.

ASSIGN 
    cUtFil = 'konv\VarerMedFeilPrisOgHarLager.csv'
    .

OUTPUT STREAM UtFil TO VALUE(cUtFil).

PUT STREAM UtFil
    'ProfilNr;'
    'ArtikkelNr;'
    'Beskr;'
    'LevKod;'
    'LevFargKod;'
    'Sasong;'
    'VmId;'
    'Beskrivelse;'
    'InnkjopsPris[1];'
    'VareKost[1];'
    'Db%[1];'
    'Pris[1];'
    'Siste prisendring;'
    'Artikkel RegistrertDato;'
    'Artikkel EndretDato;'
    'Har trans;'
    'Ant på lager'
    SKIP.

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ProfilNr <> 14 AND 
    (
    ArtPris.Pris[1] = 0 OR
    ArtPris.InnkjopsPris[1] = 0 OR
    ArtPris.VareKost[1] = 0 OR
    ArtPris.Pris[1] = ? OR
    ArtPris.InnkjopsPris[1] = ? OR
    ArtPris.VareKost[1] = ?
    ),
    FIRST ArtBas OF ArtPris NO-LOCK:

    FIND VareMerke OF ArtBas  NO-LOCK NO-ERROR.

    iAnt = iAnt + 1.
    
    iLagant = 0.
    FOR EACH ArtLag NO-LOCK WHERE 
        ArtLag.ArtikkelNr = ArtPris.ArtikkelNr AND 
        ArtLag.Lagant > 0:
        iLagant = iLagAnt + ArtLag.Lagant.
    END.
    
    DO:

        /*
        ASSIGN
            ArtPris.Rab1Kr          = ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1])/ 100,2) 
            ArtPris.Rab1Kr          = if ArtPris.Rab1Kr = ? then 0 else ArtPris.Rab1Kr 
            ArtPris.VareKost[1]     = ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]
            ArtPris.MvaKr[1]        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
            ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
            ArtPris.DB%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.VareKost[1] + ArtPris.DbKr[1]),2)
            ArtPris.DB%[1]          = IF ArtPris.DB%[1] = ? THEN 0 ELSE ArtPris.DB%[1]
        .
        */

        /*
        DISPLAY
            ArtPris.ProfilNr
            ArtPris.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LevKod
            ArtBas.LevFargKod
            ArtBas.Sasong
            ArtBas.VmId
            (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '')
            ArtPris.InnkjopsPris[1]
            ArtPris.VareKost[1]
            ArtPris.Db%[1]
            ArtPris.Pris[1]
            ArtPris.EDato COLUMN-LABEL 'Siste prisendring'
            ArtBas.RegistrertDato
            ArtBas.EDato
            CAN-FIND(FIRST TransLogg WHERE 
                     TransLogg.ArtikkelNr = ArtPris.ArtikkelNr) COLUMN-LABEL 'Har trans'
            iLagant COLUMN-LABEL 'Ant på lager'
        WITH WIDTH 350.
        */
    
        PUT STREAM UtFil UNFORMATTED
            ArtPris.ProfilNr ';'
            ArtPris.ArtikkelNr ';'
            ArtBas.Beskr ';'
            ArtBas.LevKod ';'
            ArtBas.LevFargKod ';'
            ArtBas.Sasong ';'
            ArtBas.VmId ';'
            (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '') ';'
            ArtPris.InnkjopsPris[1] ';'
            ArtPris.VareKost[1] ';'
            ArtPris.Db%[1] ';'
            ArtPris.Pris[1] ';'
            ArtPris.EDato ';'
            ArtBas.RegistrertDato ';'
            ArtBas.EDato ';'
            CAN-FIND(FIRST TransLogg WHERE 
                     TransLogg.ArtikkelNr = ArtPris.ArtikkelNr) ';'
            iLagant
            SKIP.
    END.
    
END.
OUTPUT STREAM UtFil CLOSE.

MESSAGE 'iAnt' iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
