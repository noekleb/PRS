/* Hent liste med profiler hvor det ikke ligger pris på artikkelen.
   Parameter:  
   Opprettet:               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE bok AS LOG NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE cRowIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProfilLst AS CHARACTER NO-UNDO.

ASSIGN 
    cLogg = 'modell_profillst' + REPLACE(STRING(TODAY),'/','')
    cProfilLst = '1,2,16,100'
    .
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parameter:' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    icParam: ' + icParam
    ).

IF icParam = '' THEN 
  RETURN 'Mangler artikkelnr. for å kunne gjøre oppslag.'.
                                  
ASSIGN
  lArtikkelNr = DEC(ENTRY(1,icParam,'|'))
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  RETURN 'Ugyldig artikkelnr. mottatt (' + ENTRY(1,icParam,'|') + ').'.  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lArtikkelNr: ' + STRING(lArtikkelNr)
    ).

ocReturn = ''.
FOR EACH PrisProfil NO-LOCK WHERE 
  LOOKUP(STRING(PrisProfil.ProfilNr),cProfilLst) <> 0:
  IF NOT CAN-FIND(ArtPris WHERE 
                  ArtPris.ArtikkelNr = lArtikkelNr AND 
                  ArtPris.ProfilNr   = PrisProfil.ProfilNr) THEN
    ASSIGN  
      cRowIdLst = cRowIdLst + 
                  (IF cRowIdLst = '' THEN '' ELSE ',') + 
                  STRING(ROWID(PrisProfil))
      cIdLst = cIdLst + 
               (IF cIdLst = '' THEN '' ELSE ',') + 
               STRING(PrisProfil.ProfilNr)
      .
END.
IF cRowIdLst <> '' THEN 
  ocReturn = cRowIdLst + '|' + cIdLst.

obOk = NOT TRIM(ocReturn) = '|'.
IF obOk = FALSE THEN 
  RETURN 'Det ligger pris på alle profiler på denne artikkelen.'.
ELSE 
  RETURN ''.

/* **********************  Internal Procedures  *********************** */

