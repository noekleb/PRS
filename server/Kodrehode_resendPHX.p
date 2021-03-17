/* kampanje_leggtilikkeaktive.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE cTabellNavn AS CHARACTER NO-UNDO.

ASSIGN 
  lKOrdre_Id = DEC(ENTRY(1,icParam,'|'))
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
DO:
  obOk = FALSE.
  ocReturn = '**Feil ved innsendt kundeordreid (' + icParam + ').'.
END.

DO TRANSACTION:
  FIND KOrdreHode EXCLUSIVE-LOCK WHERE 
    KOrdrEHode.KOrdre_Id = lKOrdre_Id NO-ERROR NO-WAIT.
  IF NOT AVAILABLE KOrdreHode OR LOCKED KOrdreHode THEN 
  DO:
    obOk = FALSE.
    ocReturn = '**Ukjent eller låst KOrdrEHode - sjekk id (' + icParam + ').'.
  END.  
  ELSE DO:
      ASSIGN 
/*        KOrdreHode.WebKode            = '1'*/
        obOk = TRUE
        KOrdreHode.ShipmentSend = ?
        NO-ERROR.
        
        
    /* Skaper eLogg post for å trigge utlegg på nytt. */
    /* Varebytte skal IKKE ha ELoggs poster.          */
    IF NOT KordreHode.EkstOrdreNr MATCHES '*BYTTE*' THEN 
    DO:
      FIND FIRST EkstEDBSystem WHERE 
          EkstEDBSystem.DataType = "WEBBUT" AND 
          EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE EkstEDBSystem THEN
      WEBBUTIKK:
      DO FOR ELogg:
          cTabellNavn = IF KordreHode.EkstOrdreNr MATCHES '*RETUR*'
                                THEN "RETURKOrdreHode"
                                ELSE "KOrdreHode".
          FIND ELogg EXCLUSIVE-LOCK WHERE 
               ELogg.TabellNavn     = cTabellNavn AND
               ELogg.EksterntSystem = "WEBBUT"    AND
               ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id) NO-ERROR NO-WAIT.
          IF LOCKED ELogg THEN 
              LEAVE WEBBUTIKK.
          ELSE IF NOT AVAIL Elogg THEN 
          DO:
              CREATE Elogg.
              ASSIGN ELogg.TabellNavn     = cTabellNavn
                     ELogg.EksterntSystem = "WEBBUT"   
                     ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id)
                     KOrdreHode.ShipmentSendt = NOW /* Flagger at shipment melding er sendt. */
                     .
              RELEASE ELogg.
          END.
          ELSE DO:
              ASSIGN ELogg.EndringsType = 1 
                     ELogg.Behandlet    = FALSE.
              RELEASE ELogg.
          END. 
      END. /* WEBBUTIKK */
    END.      
        
  END.
END. /* TRANSACTION */

