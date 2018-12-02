/*
  Leser liste med artikler fra en fil. Motposterer og posterer korrigert
  transaksjon for translogg poster som har avvik i vvarekost som er større
  enn 15%.
*/

CURRENT-WINDOW:WIDTH = 300.

DEFINE VARIABLE wBatchNr    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSeqNr      AS INTEGER   NO-UNDO.
DEFINE VARIABLE dArtikkelNr AS DECIMAL   FORMAT "->>>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE cFilNavn    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dAvvik%     AS DECIMAL   NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

FUNCTION hentSeqNr RETURNS INTEGER 
    (  ) FORWARD.

DEF BUFFER bArtPris FOR ArtPris.
DEF BUFFER bTransLogg FOR TransLogg.

DEFINE STREAM Inn.

ASSIGN
  dDato    = 05/01/2011
  cFilNavn = 'Artikkel_liste230811_093832.txt'.
IF SEARCH(cFilNavn) = ? THEN 
DO:
  MESSAGE 'Finner ikke angitt fil: ' cFilNavn
  VIEW-AS ALERT-BOX.
  RETURN.
END.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
LESFRAFIL:
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.
  
  ASSIGN
    dArtikkelNr = DECIMAL(ENTRY(1,cLinje,';'))
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
      MESSAGE 'Feil i fil ' cFilNavn
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      NEXT LESFRAFIL.
  END.

ARTIKKELLOOP:
FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = dArtikkelNr:

    /* Avvik skal måles mot sentralagerets kalkyle. */
    FIND bArtPris NO-LOCK WHERE
        bArtPris.ArtikkelNr = dArtikkelNr AND
        bArtPris.ProfilNr   = 1 NO-ERROR.
    IF NOT AVAILABLE bArtPris THEN 
      NEXT LESFRAFIL.

    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK:
      TRANSLOGG:
      FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
          TransLogg.ArtikkelNr =  ArtBas.ArtikkelNr AND 
          TransLogg.Dato       >= dDato AND 
          TransLogg.Tid        >= 0 AND 
          TransLogg.Butik      = Butiker.Butik:
           
          /* Skal ikke ta med nedskrivning. */
          IF TransLogg.TTId = 6 THEN 
            NEXT TRANSLOGG.
          
          /* Nye transaksjoner som er lest inn, skal ikke tas med. */
          IF TransLogg.Postert = FALSE THEN 
            NEXT TRANSLOGG.
          
          /* Avvik i varekost uttrykt i %. */
          dAvvik% = ABS((bArtPris.VareKost[1] - ABS(TransLogg.VVareKost)) * 100) / bArtPris.VareKost[1]. 
          
          /* Bare transaksjoner med feil vvarekost skal korrigeres. */
          IF TransLogg.VVarekost = ? OR 
             TransLogg.VVareKost = 0 OR 
             dAvvik% > 15 THEN. /* Gjør ingenting */
          ELSE NEXT TRANSLOGG.

          FIND Lager NO-LOCK WHERE
              Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
              Lager.Butik      = TransLogg.Butik NO-ERROR.

          /*
          DISPLAY
            TransLogg.Butik
            TransLogg.Dato
            Translogg.ttid
            ArtBas.ArtikkelNr
            ArtBas.LevKod
            ArtBas.Beskr
            dAvvik%
            bArtPris.InnkjopsPris[1]
            TransLogg.VVareKost
            bArtPris.Pris[1]
          WITH WIDTH 300.
          */

          IF wBatchNr = 0 THEN
          DO:
              RUN batchlogg.p (PROGRAM-NAME(1), 'KORR av innpris ' + STRING(TODAY), OUTPUT wBatchNr).
              RUN batchstatus.p (wBatchNr, 1).
          END.

          /* Motposterer. */
          iSeqNr = hentSeqNr().
          CREATE bTransLogg.
          BUFFER-COPY TransLogg
              EXCEPT SeqNr Antall BatchNr
              TO bTransLogg
              ASSIGN
                bTransLogg.SeqNr       = iSeqNr
                bTransLogg.Antall      = TransLogg.Antall * -1
                bTransLogg.BatchNr     = wBatchNr
                bTransLogg.Postert     = FALSE
                bTransLogg.PostertDato = ?
                .

          /* Setter korrekt varekost i transaksjonen. */
          iSeqNr = hentSeqNr().
          CREATE bTransLogg.
          BUFFER-COPY TransLogg 
              EXCEPT SeqNr VVareKost BatchNr
              TO bTransLogg
              ASSIGN
                bTransLogg.SeqNr     = iSeqNr
                bTransLogg.VVareKost = bArtPris.VareKost[1]
                bTransLogg.BatchNr   = wBatchNr
                bTransLogg.Postert   = FALSE
                bTransLogg.PostertDato = ?
                .
          /* Korreksjon av varekjøp */
          IF CAN-DO('005',STRING(Translogg.TTId,"999")) THEN
          DO:
            ASSIGN
              bTransLogg.Pris          = bArtPris.VareKost[1]
              bTransLogg.RabKr         = 0
              bTransLogg.SubtotalRab   = 0
              bTransLogg.Mva           = 0
              bTransLogg.Mva%          = 0
              .
          END.
          /* Korreksjon av brekkasje, overføringer og internt forbruk. */
          IF AVAILABLE Lager AND Lager.VVarekost <> 0 AND Lager.VVareKost <> ? AND Lager.VVAreKost > 0 AND
             CAN-DO('002,006,011',STRING(Translogg.TTId,"999")) THEN
          DO:
            ASSIGN
              bTransLogg.Pris          = Lager.VVareKost
              bTransLogg.RabKr         = 0
              bTransLogg.SubtotalRab   = 0
              bTransLogg.Mva           = 0
              bTransLogg.Mva%          = 0
              .
          END.
          
      END. /* TRANSLOGG */
    END. /* BUTIKKLOOP */
END. /* ARTIKKELLOOP */

END. /* LESFRAFIL*/
INPUT STREAM Inn CLOSE.

/* ************************  Function Implementations ***************** */

FUNCTION hentSeqNr RETURNS INTEGER 
	    (  ):
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/	

  DEFINE VARIABLE piSeqNr AS INTEGER NO-UNDO.
            
          piSeqNr = 0.
          FOR EACH bTransLogg NO-LOCK WHERE
              bTransLogg.Butik    = TransLogg.Butik AND
              bTransLogg.TransNr  = TransLogg.TransNr
              BY bTransLogg.Butik
              BY bTransLogg.TransNr
              BY bTransLogg.SeqNr:
              piSeqNr = bTransLogg.SeqNr + 1.
          END.

		RETURN piSeqNr.


		
END FUNCTION.
