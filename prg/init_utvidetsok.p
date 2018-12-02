DEFINE INPUT PARAMETER lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.

disable triggers for load of ArtBas. 
disable triggers for dump of ArtBas. 

DEF BUFFER trgVarGr     FOR VarGr.
DEF BUFFER trgHuvGr     FOR HuvGr.
DEF BUFFER trgAvdeling  FOR Avdeling.
DEF BUFFER trgLevBas    FOR LevBas.
DEF BUFFER trgVaremerke FOR Varemerke.
DEF BUFFER trgProdusent FOR Produsent.
DEF BUFFER trgFarg      FOR Farg.
DEF BUFFER trgSasong    FOR Sasong.
DEF BUFFER trgStrekkode FOR Strekkode.
DEF BUFFER trgStrKonv   FOR StrKonv.

DEFINE VARIABLE trgcLopNr   AS CHAR NO-UNDO. 
DEFINE VARIABLE trgcStrLst  AS CHAR NO-UNDO.
DEFINE VARIABLE trgcKodeLst AS CHAR NO-UNDO.
DEFINE VARIABLE trgERPNrLst AS CHAR NO-UNDO.
DEFINE VARIABLE trgTekst    AS CHAR NO-UNDO.

DEFINE VARIABLE iCnt AS INTEGER NO-UNDO. 

DO TRANSACTION:
FIND ArtBas EXCLUSIVE-LOCK WHERE
  ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
IF AVAILABLE ArtBas THEN 
DO:
    FIND trgVargr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE trgVarGr THEN
        FIND trgHuvGr OF trgVarGr NO-LOCK NO-ERROR.
    IF AVAILABLE trgHuvGr THEN
        FIND trgAvdeling OF trgHuvGr NO-LOCK NO-ERROR.
    FIND trgLevBas    OF ArtBas NO-LOCK NO-ERROR.
    FIND trgVaremerke OF ArtBas NO-LOCK NO-ERROR.
    FIND trgProdusent OF ArtBas NO-LOCK NO-ERROR.
    FIND trgFarg      OF ArtBas NO-LOCK NO-ERROR.
    FIND trgSasong    OF ArtBas NO-LOCK NO-ERROR.
    
    ASSIGN
      trgcStrLst  = " "
      trgcKodeLst = " "
      trgERPNrLst = "".
    /* Henter størrelser */    
    FOR EACH TrgStrekkode NO-LOCK WHERE
            trgStrekkode.ArtikkelNr = ArtBas.ArtikkelNr:
        FIND trgStrKonv NO-LOCK WHERE
            trgStrKonv.StrKode = trgStrekkode.StrKode NO-ERROR.
        IF AVAILABLE trgStrKonv THEN
            ASSIGN
            trgcStrLst = trgcStrLst + trim(trgStrKonv.Storl) + " ".
        ASSIGN
        trgcKodeLst = trgcKodeLst + trim(trgStrekkode.Bestillingsnummer) + " "
        trgERPNrLst = trgERPNrLst + trim(trgStrekkode.ERPNr) + " "
        NO-ERROR.
    END.
    
    ASSIGN 
    trgTekst  = "".
    
    ASSIGN
        trgTekst = SUBSTRING(
            STRING(ArtBas.ArtikkelNr) + " " + 
            ArtBas.LevKod + " " + 
            ArtBas.Beskr + " " + 
            ArtBas.LevFargKod + " " + 
            (IF AVAILABLE trgSasong 
               THEN trgSasong.SasBeskr
               ELSE "") + " " +                          
            (IF AVAILABLE trgVarGr 
               THEN trgVarGr.VgBeskr
               ELSE "") + " " +                          
            (IF AVAILABLE trgLevBas 
               THEN trgLevBas.LevNamn
               ELSE "") + " " +
            (IF AVAILABLE trgVaremerke 
               THEN trgVaremerke.Kortnavn
               ELSE "") + " " +
            (IF AVAILABLE trgProdusent 
               THEN trgProdusent.Beskrivelse
               ELSE "") + " " +
            (IF AVAILABLE trgFarg 
               THEN trgFarg.FarBeskr
               ELSE "") + " " +
            trgcStrLst  + " " +
            trgcKodeLst + " " +
            trgERPNrLst
            ,1,3100) NO-ERROR    .
    
    IF ERROR-STATUS:ERROR = FALSE THEN 
       icnt = icnt + 1. 
    /*   
    if icnt mod 1000 = 0 then disp  icnt trgtekst format "x(40)". 
    pause 0.    
    */
       
    ASSIGN
       ArtBas.UtvidetSok = SUBSTRING(trgTekst,1,3500)
       NO-ERROR.
END.
END. /* TRANSACTION */