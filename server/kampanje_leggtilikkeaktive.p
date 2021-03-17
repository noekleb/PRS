/* kampanje_leggtilikkeaktive.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cHKatLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bAktive AS LOG NO-UNDO.

ASSIGN 
  iKampanjeId = INT(ENTRY(1,icParam,'|'))
  NO-ERROR.
IF NUM-ENTRIES(icParam,'|') >= 3 THEN 
  ASSIGN
    cHKatLst = ENTRY(2,icParam,'|')
    cVmLst   = ENTRY(3,icParam,'|') 
    .
    
IF NUM-ENTRIES(icParam,'|') >= 4 THEN 
  ASSIGN
    bAktive  = IF CAN-DO('YES,TRUE',ENTRY(4,icParam,'|')) THEN TRUE ELSE FALSE
    .  
IF ERROR-STATUS:ERROR THEN 
DO:
  obOk = FALSE.
  ocReturn = '**Feil ved innsendt kampanjeid (' + icParam + ').'.
END.

FIND KampanjeHode NO-LOCK WHERE 
  KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.
IF NOT AVAILABLE KampanjeHode THEN 
DO:
  obOk = FALSE.
  ocReturn = '**Ukjent kampanjehode - sjekk kampanjeid (' + icParam + ').'.
END.  

MESSAGE
'icParam:' icParam SKIP  
'cHKatLst:'cHKatLst SKIP
'cVmLst:' cVmLst SKIP 
'bAktive:' bAktive
VIEW-AS ALERT-BOX.

iAnt = 0.
PASSIVEBLOKKEN:
FOR EACH ArtPris NO-LOCK WHERE 
  ArtPris.ProfilNr = 16 AND 
  (IF (bAktive AND KampanjeHode.aktivert = FALSE) THEN TRUE ELSE ArtPris.tilbud = FALSE):
  /* Er kampanjen allerede aktivert, skal ikke aktive artikler trekkes inn uansett hva som er valgt. */
  
  IF CAN-FIND(FIRST Lager NO-LOCK WHERE 
              Lager.ArtikkelNr = ArtPris.ArtikkelNr AND 
              Lager.butik      = 16 AND 
              Lager.Lagant > 0) THEN
  AKTIVERBLOKK:
  DO: 
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:
      IF (cHKatLst <> '' AND NOT CAN-DO(cHKatLst,STRING(ArtBas.HovedKatNr))) THEN 
        LEAVE AKTIVERBLOKK.
      IF (cVmLst <> '' AND NOT CAN-DO(cVmLst,STRING(ArtBas.VmId))) THEN 
        LEAVE AKTIVERBLOKK.
    END.

    IF AVAILABLE ArtBas THEN 
      MESSAGE '  Artikkel utvalg:' + ArtBas.Beskr + ' ' + ArtBas.LevKod + ' ' + STRING(ArtBas.HovedKatNr) + ' ' + STRING(ArtPris.Tilbud)
      VIEW-AS ALERT-BOX.
      
    
    RUN art_to_kampanje.p(STRING(KampanjeHode.KampanjeId) + ',ARTNR,' + STRING(ArtPris.ArtikkelNr) + ',TRUE',
                          ?,
                          '',
                          OUTPUT cReturn,
                          OUTPUT bOk
                         ).
    IF cReturn = '1' THEN                      
      iAnt = iAnt + 1.
  END. /* AKTIVERBLOKK */
END. /*PASSIVEBLOKKEN */

obOK     = YES.
ocReturn = IF iAnt > 0 THEN STRING(iAnt) + ' artikler lagt til kampanjen.' ELSE 'Ingen artikler lagt til.'.

RETURN ocReturn.


