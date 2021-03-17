/* Kodrehode_pakkeliste.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iButNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttKOrdrePostPakke NO-UNDO
  FIELD KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" 
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS Blob
  .

ASSIGN 
  iButNr   = INT(ENTRY(1,icParam,'|'))
  ocReturn = '.'
  cLogg    = 'Kodrehode_pakkeliste.p' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

RUN skrivpakkeliste.p (iButNr, 2, OUTPUT cfilNavn).

obOk = ocReturn = "".
obOk = TRUE.

/* **********************  Internal Procedures  *********************** */
