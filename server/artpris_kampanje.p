/* artpris_kampanje.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLagerBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iSalgBut AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTTIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER NO-UNDO.

DEF TEMP-TABLE ArtPris
    FIELD ProfilNr AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD artpris_Beskr AS CHARACTER
    FIELD artpris_LevKod AS CHARACTER
    FIELD artpris_LevFargKod AS CHARACTER
    FIELD artpris_TilbudFraDato AS DATETIME
    FIELD artpris_TilbudTilDato AS DATETIME
    FIELD Pris_1 AS DECIMAL
    FIELD jbextent_1_Pris AS DECIMAL /* placeholder for calculation */
    FIELD Rab1%_2 AS DECIMAL
    FIELD jbextent_2_Rab1% AS DECIMAL /* placeholder for calculation */
    FIELD Pris_2 AS DECIMAL
    FIELD jbextent_2_Pris AS DECIMAL /* placeholder for calculation */
    FIELD artpris_Solgt% AS DECIMAL
    FIELD artpris_AntSolgt AS INTEGER
    FIELD artpris_VerdiSolgt AS DECIMAL
    FIELD artpris_LagAnt AS DECIMAL
    FIELD artpris_LagVerdi AS DECIMAL
    FIELD artpris_Varemerke AS CHARACTER
    FIELD artpris_Produsent AS CHARACTER
    FIELD artpris_Sesong AS CHARACTER
    FIELD artpris_Varegruppe AS CHARACTER
    FIELD artpris_Hovedgruppe AS CHARACTER
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
DEFINE BUFFER bufArtPris FOR ArtPris.
    
RUN opprettArtPrisTbl.

ihBuffer:COPY-TEMP-TABLE (BUFFER ArtPris:HANDLE,NO,NO,YES).

obOK = YES.
obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettArtPrisTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  
  ASSIGN 
    iProfilNr = 16
    cButLst   = '15,16'
    iLagerBut = 16
    iSalgBut  = 15
    cTTIdLst  = '1,3,10'
    .
  
  EMPTY TEMP-TABLE ArtPris.

  FOR EACH bufArtPris NO-LOCK WHERE 
    bufArtPris.ProfilNr = iProfilNr AND 
    bufArtPris.Tilbud   = TRUE:
      
    CREATE ArtPris.
    BUFFER-COPY bufArtPris
      TO ArtPris.  
      
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:
      ASSIGN
        ArtPris.artpris_Beskr = ArtBas.Beskr
        ArtPris.artpris_LevKod = ArtBas.LevKod
        ArtPris.artpris_LevFargKod = ArtBas.LevFargKod
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
        ArtPris.artpris_Varemerke   = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ''
        ArtPris.artpris_Produsent   = IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE ''
        ArtPris.artpris_Sesong      = IF AVAILABLE Sasong THEN SaSong.SasBeskr ELSE ''
        ArtPris.artpris_Varegruppe  = IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ''
        ArtPris.artpris_Hovedgruppe = IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE ''
        .
      
    END.    
    ASSIGN  
        ArtPris.artpris_TilbudFraDato = DATETIME(bufArtPris.TilbudFraDato, bufArtPris.TilbudFraTid * 1000)
        ArtPris.artpris_TilbudTilDato = DATETIME(bufArtPris.TilbudTilDato, bufArtPris.TilbudTilTid * 1000)
        ArtPris.Pris_1  = bufArtPris.Pris[1]
        ArtPris.Rab1%_2 = ROUND(((bufArtPris.Pris[1] - bufArtPris.Pris[2]) * 100) / bufArtPris.Pris[1],2) 
        ArtPris.Pris_2  = bufArtPris.Pris[2]
        .
    FIND FIRST Lager NO-LOCK WHERE 
      Lager.Butik = iLagerBut AND 
      Lager.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
    IF AVAILABLE Lager THEN 
      ASSIGN
        ArtPris.artpris_LagAnt = Lager.Lagant
        ArtPris.artpris_LagVerdi = Lager.LagAnt * Lager.VVareKost 
        .  
    IF bufArtPris.TilbudFraDato <> ? AND 
       bufArtPris.tilbudTilDato <> ? THEN 
      DO:
        DO dDato = bufArtPris.TilbudFraDato TO bufArtPris.TilbudTilDato:
          FOR EACH TransLogg NO-LOCK WHERE
            Translogg.ArtikkelNr = bufArtPris.ArtikkelNr AND 
            TransLogg.Dato  = dDato AND
            TransLogg.Tid  >= 0 AND 
            TransLogg.Butik = iSalgBut:
            CASE TransLogg.TTId:
              WHEN 1 THEN
                ASSIGN
                  ArtPris.ArtPris_antSolgt    = ArtPris.ArtPris_antSolgt   + TransLogg.Antall
                  ArtPris.ArtPris_VerdiSolgt  = ArtPris.ArtPris_VerdiSolgt + (TransLogg.Pris * Translogg.antall)
                  .
              WHEN 3 THEN
                ASSIGN
                  ArtPris.ArtPris_antSolgt   = ArtPris.ArtPris_antSolgt   + TransLogg.Antall
                  ArtPris.artpris_VerdiSolgt = ArtPris.artpris_VerdiSolgt + ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall)
                  .
              WHEN 10 THEN
                ASSIGN
                  ArtPris.ArtPris_antSolgt   = ArtPris.ArtPris_antSolgt   + TransLogg.Antall
                  ArtPris.artpris_VerdiSolgt = ArtPris.artpris_VerdiSolgt + (TransLogg.Pris * TransLogg.Antall)
                  .
            END CASE.
          END.
        END.
         
        ASSIGN 
          ArtPris.artpris_Solgt% = (artpris_AntSolgt * 100) / (artpris_AntSolgt + ArtPris.artpris_LagAnt)
          ArtPris.artpris_Solgt% = IF ArtPris.artpris_Solgt% = ? THEN 0 ELSE ArtPris.artpris_Solgt%
          .
      END.
      ELSE 
        ASSIGN 
          ArtPris.artpris_AntSolgt = 0
          ArtPris.artpris_VerdiSolgt = 0
          ArtPris.artpris_Solgt% = 0
         .
     
      

  END.
    
END PROCEDURE.
