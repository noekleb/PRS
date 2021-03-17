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
DEFINE VARIABLE cHovedKatLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE iNetbut AS INTEGER NO-UNDO.
DEFINE VARIABLE lRab% AS DECIMAL NO-UNDO.
DEFINE VARIABLE lPris AS DECIMAL NO-UNDO.
DEFINE VARIABLE bTilbud AS LOG NO-UNDO.
DEFINE VARIABLE ltilbPris AS DECIMAL NO-UNDO.
DEFINE VARIABLE lReservert AS DECIMAL NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE lVareKost AS DECIMAL NO-UNDO.
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.
DEFINE VARIABLE lKampRab% AS DECIMAL NO-UNDO.

DEF TEMP-TABLE ArtLag
    FIELD butik AS INTEGER
    FIELD ArtLag_VmId AS INTEGER
    FIELD ArtLag_Varemerke AS CHARACTER
    FIELD ArtLag_NOS AS CHARACTER
    FIELD ArtLag_LevNr AS INTEGER
    FIELD ArtLag_Beskr AS CHARACTER
    FIELD ArtLag_LevKod AS CHARACTER
    FIELD ArtLag_LevFargKod AS CHARACTER
    FIELD storl AS CHARACTER
    FIELD lagant AS DECIMAL
    FIELD ArtLag_Sasong AS INTEGER
    FIELD ArtLag_Pris AS DECIMAL
    FIELD ArtLag_Tilbud AS CHARACTER
    FIELD ArtLag_TilbPris AS DECIMAL
    FIELD ArtLag_KampRab% AS DECIMAL
    FIELD ArtLag_Kode AS CHARACTER
    FIELD ArtLag_Reservert AS DECIMAL
    FIELD ArtLag_Varekost AS DECIMAL
    FIELD ArtLag_Rab% AS DECIMAL
    FIELD ArtLag_VVareKost AS DECIMAL
    FIELD ArtBas_WebButikkArtikkel AS LOGICAL
    FIELD ArtBas_PubliserINettButikk AS LOGICAL
    FIELD EndretDatoTid AS DATETIME
    FIELD ArtikkelNr AS DECIMAL
    FIELD StrKode AS INTEGER
    FIELD ArtLag_Recid AS CHARACTER
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
DEFINE BUFFER bufArtLag FOR ArtLag.
DEFINE BUFFER bufNetButiker FOR Butiker.
DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 150 1 2 iNetBut INT}
FIND bufNetbutiker NO-LOCK WHERE 
  bufNetbutiker.butik = iNetBut NO-ERROR.

{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 150 1 3 iLagerBut INT}

EMPTY TEMP-TABLE ArtLag.
RUN opprettArtLagTbl.

ihBuffer:COPY-TEMP-TABLE (BUFFER ArtLag:HANDLE,NO,NO,YES).

obOK = YES.
obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettArtLagTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iSalgBut NO-ERROR.
  IF AVAILABLE Butiker THEN 
    iProfilNr = Butiker.Profilnr.
  ELSE 
    iProfilNr = 1.
  
  EMPTY TEMP-TABLE ArtLag.

  HOVEDLOOP:
  FOR EACH ArtBas NO-LOCK,
    EACH Strekkode OF ArtBas NO-LOCK:

    FIND bufArtLag NO-LOCK WHERE
      bufArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
      bufArtLag.butik      = iLagerBut AND
      bufArtLag.StrKode    = Strekkode.StrKode NO-ERROR.
    IF NOT AVAILABLE bufArtLag THEN
    DO:
      FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
      CREATE ArtLag.
      ASSIGN
        ArtLag.ArtikkelNr    = ArtBas.ArtikkelNr
        ArtLag.butik         = iLagerBut
        ArtLag.StrKode       = Strekkode.StrKode
        ArtLag.storl         = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
        ArtLag.lagant        = 0
        ArtLag.StrKode       = Strekkode.StrKode
        ArtLag.EndretDatoTid = NOW
        .
    END.
    ELSE DO:
      CREATE ArtLag.
      BUFFER-COPY bufArtLag 
        TO ArtLag
        .
    END.
    
    RUN getArtLag_KampRab% ( OUTPUT lRab%, 
                             OUTPUT lPris,
                             OUTPUT bTilbud,
                             OUTPUT lTilbPris,
                             OUTPUT lReservert,
                             OUTPUT lVareKost,
                             OUTPUT lVVareKost,
                             OUTPUT lKampRab%  
                            ).
    /* Supplerer recorden */
    ASSIGN 
      ArtLag.ArtLag_VmId        = ArtBas.VmId
      ArtLag.ArtLag_Varemerke   = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ''
      ArtLag.ArtLag_NOS         = ArtBas.Lagerkoder
      ArtLag.ArtLag_LevNr       = ArtBas.LevNr
      ArtLag.ArtLag_Beskr       = ArtBas.Beskr
      ArtLag.ArtLag_LevKod      = ArtBas.LevKod
      ArtLag.ArtLag_LevFargKod  = ArtBas.LevFargKod
      ArtLag.ArtLag_Sasong      = ArtBas.Sasong
      ArtLag.ArtLag_Pris        = lPris
      ArtLag.ArtLag_Tilbud      = IF bTilbud THEN '*' ELSE ''
      ArtLag.ArtLag_TilbPris    = lTilbPris
      ArtLag.ArtLag_KampRab%    = lKampRab%
      ArtLag.ArtLag_Kode        = Strekkode.Kode
      ArtLag.ArtLag_Reservert   = lReservert
      ArtLag.ArtLag_Varekost    = lVarekost
      ArtLag.ArtLag_Rab%        = lRab%
      ArtLag.ArtLag_VVareKost   = lVVareKost
      ArtLag.ArtBas_WebButikkArtikkel = ArtBas.WebButikkArtikkel
      ArtLag.ArtBas_PubliserINettButikk = ArtBas.PubliserINettButikk
      ArtLag.ArtLag_Recid       = IF AVAILABLE ArtLag THEN STRING(RECID(ArtLag)) ELSE ''
      .
    
  END. /* HOVEDLOOP */
    
END PROCEDURE.

PROCEDURE getArtLag_KampRab%:
  DEFINE OUTPUT PARAMETER plRab% AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER plPris AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER pbTilbud AS LOG NO-UNDO.
  DEFINE OUTPUT PARAMETER plTilbPris AS DECIMAL NO-UNDO.   
  DEFINE OUTPUT PARAMETER plReservert AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER plVareKost AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER plVVareKost AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER plKampRab% AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE plRabKr AS DECIMAL NO-UNDO.

  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = (IF AVAILABLE bufNetButiker THEN bufNetButiker.ProfilNr ELSE clbutiker.ProfilNr) NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris THEN 
    DO: 
      /* På tilbud. */
      IF ArtPris.Tilbud = TRUE THEN 
        ASSIGN
          pltilbPris  = ArtPris.Pris[2]
          pbTilbud    = TRUE
          plRabKr     = ArtPris.Pris[1] - ArtPris.Pris[2]
          plKampRab%  = ROUND((plRabKr * 100) / ArtPris.Pris[1],1)    
          .
      /* Ordinær pris */
      ELSE 
        ASSIGN 
          pltilbPris = 0
          pbTilbud   = FALSE
          plRabKr    = 0
          plKampRab% = 0 
          .
      /* Generelt */    
      ASSIGN 
        plPris      = ArtPris.Pris[1]
        plVareKost  = ArtPris.VareKost[1]
        plRab%      = ArtPris.Rab1%[IF ArtPris.tilbud THEN 2 ELSE 1]
        .
    END.

    FIND bufArtLag NO-LOCK WHERE
      bufArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
      bufArtLag.Butik      = iNetBut AND
      bufArtLag.StrKode    = Strekkode.StrKode NO-ERROR.
    IF AVAILABLE bufArtLag THEN
      plReservert = bufArtLag.LagAnt.
    ELSE
      plReservert = 0.
      
    FIND Lager NO-LOCK WHERE 
      Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
      Lager.butik      = iLagerBut NO-ERROR.
    IF AVAILABLE Lager THEN 
      ASSIGN 
        plVVareKost = Lager.VVareKost
        .
  END.
  
END PROCEDURE. 

