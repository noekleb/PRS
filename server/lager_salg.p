/* artpris_kampanje.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iLagerBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iSalgBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.

DEF TEMP-TABLE Lager
    FIELD Varetekst AS CHARACTER
    FIELD LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Pris AS DECIMAL
    FIELD Solgt% AS DECIMAL
    FIELD Solgt AS DECIMAL
    FIELD VerdiSolgt AS DECIMAL
    FIELD Lagant AS DECIMAL
    FIELD VerdiLager AS DECIMAL
    FIELD Sasong AS CHARACTER
    FIELD Varegruppe AS CHARACTER
    FIELD Hovedgruppe AS CHARACTER
    FIELD Produsent AS CHARACTER
    FIELD Varemerke AS CHARACTER
    FIELD VVarekost AS DECIMAL
    FIELD Butik AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .

/*
• If you define a temp-table with the same name as a database table and 
  then you define a buffer for that name, the buffer will be associated 
  with the database table, not with the temp-table.
*/
DEFINE BUFFER bufLager FOR Lager.
DEFINE BUFFER buf2Lager FOR Lager.
    
RUN opprettLagerTbl.

ihBuffer:COPY-TEMP-TABLE (BUFFER Lager:HANDLE,NO,NO,YES).

obOK = YES.
obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettLagerTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
  ASSIGN 
    iLagerBut = 16
    iSalgBut  = 15
    iProfilNr = 16
    .
  {syspara.i 150 1 2 iSalgBut INT}
  {syspara.i 150 1 3 iLagerBut INT}
  
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iSalgBut NO-ERROR.
  IF AVAILABLE Butiker THEN 
    iProfilNr = Butiker.Profilnr.
  ELSE 
    iProfilNr = 1.
  
  EMPTY TEMP-TABLE Lager.

  LAGERLOOP:
  FOR EACH bufLager NO-LOCK WHERE 
    bufLager.Butik = iLagerBut, 
    FIRST ArtBas OF bufLager NO-LOCK WHERE 
          ArtBas.WebButikkArtikkel = TRUE:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.  
      
    CREATE Lager.
    BUFFER-COPY bufLager
      TO Lager NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN 
    DO:
      NEXT.
    END. 
      
    ASSIGN
      Lager.Varetekst = ArtBas.Beskr
      Lager.LevKod    = ArtBas.LevKod
      Lager.LevFargKod = ArtBas.LevFargKod
      Lager.VerdiLager = IF Lager.VerdiLager = ? THEN 0 ELSE Lager.VerdiLager
      Lager.Lagant = bufLager.Lagant
      Lager.VVareKost = bufLager.VVareKost
      Lager.VVareKost = IF Lager.VVareKost = ? THEN 0 ELSE Lager.VVareKost
      Lager.VerdiLager = Lager.Lagant * Lager.VVareKost
      .
    IF AVAILABLE HuvGr THEN 
      RELEASE HuvGr.
    FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
    FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
    FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN 
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
    ASSIGN 
      Lager.Varemerke = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ''
      Lager.Produsent = IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE ''
      Lager.Sasong = IF AVAILABLE Sasong THEN STRING(SaSong.Sasong) ELSE ''
      Lager.Varegruppe = IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ''
      Lager.Hovedgruppe = IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE ''
      Lager.Pris = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
      .
      
    FIND FIRST buf2Lager NO-LOCK WHERE 
      buf2Lager.Butik = iSalgBut AND 
      buf2Lager.ArtikkelNr = bufLager.ArtikkelNr NO-ERROR.
    IF AVAILABLE buf2Lager THEN 
      ASSIGN
        Lager.Solgt = buf2Lager.AntSolgt
        Lager.VerdiSolgt = buf2Lager.VerdiSolgt 
        .  
    ELSE  
      ASSIGN
        Lager.Solgt = 0
        Lager.VerdiSolgt = 0
        . 
    
    ASSIGN 
      Lager.Solgt% = (Lager.Solgt * 100) / (Lager.Solgt + Lager.Lagant)
      Lager.Solgt% = IF Lager.Solgt% = ? THEN 0 ELSE Lager.Solgt%
      .
  END. /* LAGERLOOP */
    
END PROCEDURE.
