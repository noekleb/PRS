/*
  Programmet skaper en liste over de artikler som har translogg poster
  hvor TransLogg.VVareKost avviker mer en 15% fra kalkulert varekost
  i prisprofil 1 (Hk).
  Listen benyttes siden som innput for korreksjonsprogrammet som 
  utfører korreksjonen av transaksjonene.
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

DEF TEMP-TABLE tmpArtBas 
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    .

DEFINE STREAM Ut.

ASSIGN
    dDato  = 05/01/2011
    cFilNavn = 'Artikkel_liste' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,'HH:MM:SS'),':','') +  '.txt'.

ARTIKKELLOOP:
FOR EACH ArtBas NO-LOCK WHERE 
    CAN-FIND(FIRST TransLogg WHERE
             TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
             TransLogg.Dato       >= dDato):

    /* Avvik skal måles mot sentralagerets kalkyle. */
    FIND bArtPris NO-LOCK WHERE
        bArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
        bArtPris.ProfilNr   = 1 NO-ERROR.
    IF NOT AVAILABLE bArtPris THEN 
      NEXT ARTIKKELLOOP.

    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK:
      TRANSLOGG:
      FOR EACH TransLogg NO-LOCK WHERE
          TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
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
          IF NOT CAN-FIND(tmpArtBas WHERE tmpArtBas.ArtikkelNr = ArtBas.ArtikkelNr) THEN
          DO:
              CREATE tmpArtBas.
              ASSIGN
                  tmpArtBas.ArtikkelNr = ArtBas.ArtikkelNr.
          END.
      END. /* TRANSLOGG */
    END. /* BUTIKKLOOP */
END. /* ARTIKKELLOOP */

IF CAN-FIND(FIRST tmpArtBAs) THEN 
OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.
FOR EACH tmpArtBas:
    PUT STREAM Ut UNFORMATTED tmpArtBas.ArtikkelNr SKIP.
END.
OUTPUT STREAM Ut CLOSE.

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

