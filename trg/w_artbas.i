DEF BUFFER trgVarGr     FOR VarGr.
DEF BUFFER trgHuvGr     FOR HuvGr.
DEF BUFFER trgAvdeling  FOR Avdeling.
DEF BUFFER trgLevBas    FOR LevBas.
DEF BUFFER trgVaremerke FOR Varemerke.
DEF BUFFER trgProdusent FOR Produsent.
DEF BUFFER trgFarg      FOR Farg.
DEF BUFFER trgSasong    FOR Sasong.
DEF BUFFER trgStrekkode FOR {&vpi}Strekkode.
DEF BUFFER trgStrKonv   FOR StrKonv.

DEF VAR trgcLopNr   AS CHAR NO-UNDO. 
DEF VAR trgcStrLst  AS CHAR NO-UNDO.
DEF VAR trgcKodeLst AS CHAR NO-UNDO.
DEFINE VARIABLE trgERPNrLst AS CHARACTER NO-UNDO.

FIND trgVargr OF {&vpi}ArtBas NO-LOCK NO-ERROR.
IF AVAILABLE trgVarGr THEN
    FIND trgHuvGr OF trgVarGr NO-LOCK NO-ERROR.
IF AVAILABLE trgHuvGr THEN
    FIND trgAvdeling OF trgHuvGr NO-LOCK NO-ERROR.
FIND trgLevBas OF {&vpi}ArtBas NO-LOCK NO-ERROR.
FIND trgVaremerke OF {&vpi}ArtBas NO-LOCK NO-ERROR.
FIND trgProdusent OF {&vpi}ArtBas NO-LOCK NO-ERROR.
FIND trgFarg OF {&vpi}ArtBas NO-LOCK NO-ERROR.
FIND trgSasong OF {&vpi}ArtBas NO-LOCK NO-ERROR.

DEF VAR trgTekst AS CHAR NO-UNDO.

ASSIGN
  trgcStrLst  = " "
  trgcKodeLst = " ".
/* Henter størrelser */
FOR EACH TrgStrekkode NO-LOCK WHERE
    /* Parameters Definitions ---                                           */
    &IF '{&vpi}' = 'VPI' &THEN          
        trgStrekkode.VareNr = STRING({&vpi}ArtBas.ArtikkelNr):
    &ELSE
        trgStrekkode.ArtikkelNr = {&vpi}ArtBas.ArtikkelNr:
    &ENDIF

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
IF ERROR-STATUS:ERROR = FALSE THEN 
DO:
  ASSIGN
    trgTekst = SUBSTRING(
        STRING({&vpi}ArtBas.ArtikkelNr) + " " + 
        {&vpi}ArtBas.LevKod + " " + 
        {&vpi}ArtBas.Beskr + " " + 
        {&vpi}ArtBas.LevFargKod + " " + 
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
        ,1,3100) NO-ERROR
    .
  IF ERROR-STATUS:ERROR = FALSE THEN 
  ASSIGN
    {&vpi}ArtBas.UtvidetSok = SUBSTRING(trgTekst,1,3500)
    NO-ERROR.
END.

