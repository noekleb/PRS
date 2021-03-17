DEF INPUT PARAM  icRowId       AS CHAR NO-UNDO.
DEF INPUT PARAM  icValueFields AS CHAR NO-UNDO.
DEF INPUT PARAM  icValues      AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError       AS CHAR NO-UNDO.

IF ENTRY(LOOKUP("Vg",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST VarGr WHERE 
                        VarGr.Vg = INT(ENTRY(LOOKUP("Vg",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig varegruppe er angitt.".
END.
ELSE 
    ocError = "Varegruppe er ikke angitt.".

IF ENTRY(LOOKUP("LevNr",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST LevBas WHERE 
                        LevBas.LevNr = INT(ENTRY(LOOKUP("LevNr",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig leverandr er angitt.".
END.
ELSE 
    ocError = "Leverandr er ikke angitt.".

IF ENTRY(LOOKUP("ProdNr",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST Produsent WHERE 
                        Produsent.ProdNr = INT(ENTRY(LOOKUP("ProdNr",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig produsent er angitt.".
END.
ELSE 
    ocError = "Produsent er ikke angitt.".

IF ENTRY(LOOKUP("Sasong",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST Sasong WHERE 
                        Sasong.Sasong = INT(ENTRY(LOOKUP("Sasong",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig sesong er angitt.".
END.
ELSE 
    ocError = "Sesong er ikke angitt.".

IF ENTRY(LOOKUP("VmId",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST VareMerke WHERE 
                        Varemerke.VmId = INT(ENTRY(LOOKUP("VmId",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig varemerke er angitt.".
END.
ELSE 
    ocError = "Varemerke er ikke angitt.".

IF ENTRY(LOOKUP("StrTypeId",icValueFields),icValues,"|") <> '' THEN DO:
  IF NOT CAN-FIND(FIRST StrType WHERE 
                        StrType.StrTypeId = INT(ENTRY(LOOKUP("StrTypeId",icValueFields),icValues,"|"))) THEN
    ocError = "Ugyldig størrelsestype er angitt.".
END.
ELSE 
    ocError = "Størrelsestype er ikke angitt.".

IF DECIMAL(ENTRY(LOOKUP("LinkVareNr",icValueFields),icValues,"|")) <> 0 THEN DO:
  IF NOT CAN-FIND(FIRST ArtBas WHERE 
                        ArtBas.ArtikkelNr = DEC(ENTRY(LOOKUP("LinkVareNr",icValueFields),icValues,"|")) AND
                        ArtBas.Pant = TRUE) THEN
    ocError = "Ugyldig pantartikkel er angitt.".
END.
    
IF ocError = "" THEN DO:
  DEFINE VARIABLE fInnkjPris  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE fRab1       AS DECIMAL NO-UNDO.
  DEFINE VARIABLE fPris1      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iProfilnr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE fPrisExMVA  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE fVarekost   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE fDb%        AS DECIMAL NO-UNDO.

  DEFINE VARIABLE KatalogPris#1 AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ForhRab%#1    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE Varekost      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE suppRab%#1    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE suppVareKost  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE AnbefaltPris  AS DECIMAL NO-UNDO.

  FIND VPIArtBas NO-LOCK
       WHERE ROWID(VPIArtBas) = TO-ROWID(icRowId)
       NO-ERROR.
  IF AVAIL VPIArtBas THEN DO:

    FIND CURRENT VPIArtBas EXCLUSIVE-LOCK NO-ERROR.
    
    ASSIGN fInnkjPris    = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Innkjopspris#1"))
           fRab1         = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Rab1%#1"))
           fVarekost     = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Varekost#1"))
           fDb%          = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Db%#1"))
           fPris1        = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Pris#1"))
           iProfilnr     = INTEGER(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Profilnr"))
           KatalogPris#1 = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KatalogPris#1"))
           ForhRab%#1    = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"ForhRab%#1"))
           Varekost      = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Varekost"))
           suppRab%#1    = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"suppRab%#1"))
           suppVareKost  = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"suppVareKost"))
           AnbefaltPris  = DECIMAL(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"AnbefaltPris"))
           .
    FIND FIRST VPIArtPris EXCLUSIVE-LOCK
         WHERE VPIArtPris.ArtikkelNr = VPIArtBas.ArtikkelNr
           AND VPIArtPris.ProfilNr   = iProfilnr
         NO-WAIT NO-ERROR.
    IF LOCKED VPIArtPris THEN
      ocError = "VPIArtPris ikke tilgjengelig for oppdatering (lst av annen bruker): " + PROGRAM-NAME(1).
    ELSE IF AVAIL VPIArtPris THEN 
      ASSIGN VPIArtPris.InnkjopsPris[1] = fInnkjPris
             VPIArtPris.Rab1%[1]        = fRab1
             VPIArtPris.VareKost[1]     = fVarekost
             VPIArtPris.Pris[1]         = fPris1
             fPrisExMVA                 = fPris1 / (1 + VPIArtPris.Mva%[1] / 100)
             VPIArtPris.DBKr[1]         = fPrisExMVA - VPIArtPris.VareKost[1]
             VPIArtPris.DB%[1]          = fDb%
             VPIArtBas.KatalogPris      = KatalogPris#1 
             VPIArtBas.ForhRab%         = ForhRab%#1    
             VPIArtBas.suppRab%         = suppRab%#1    
             VPIArtBas.AnbefaltPris     = AnbefaltPris  
             .               
    ELSE ocError = "VPIArtPris eksisterer ikke for_ artikkel: " + STRING(VPIArtBas.ArtikkelNr) + ", profilnr: " + STRING(iProfilnr) + "  " + PROGRAM-NAME(1).
  END.
  IF AVAILABLE VPIArtBAs THEN 
    FIND CURRENT VPIArtBas NO-LOCK NO-ERROR.
END.
