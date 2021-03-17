TRIGGER PROCEDURE FOR WRITE OF KOrdreHode  OLD BUFFER Old_KOrdreHode.

DEF VAR trgcTabellNavn AS CHAR NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNettbutikstyp AS CHARACTER   NO-UNDO.
DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.

{trg\c_w_trg.i &Fil=SkoTex.KOrdreHode &Type="W"}

{syspara.i 150 1 20 cNettbutikstyp}

IF KOrdreHode.Opphav = 0 THEN
  KOrdreHode.Opphav = 1.
  
ASSIGN 
    cLogg = 'trgKOrdreHode' + REPLACE(STRING(TODAY),'/','')
    KOrdreHode.DatoTidEndret = NOW
    .
  
/* Fyller ut sendingsnr hvis det ikke er utfyllt. */
IF KordreHode.EkstOrdreNr MATCHES '*RETUR*' AND 
    KOrdreHode.SendingsNr = '' THEN
    ASSIGN KOrdreHode.SendingsNr = 'RETUR'.       

/* Fyller ut sendingsnr hvis det er makulering. */
IF KOrdreHode.LevStatus = '60' AND 
    KOrdreHode.SendingsNr = '' THEN
    ASSIGN KOrdreHode.SendingsNr = 'MAKULERT'.       
IF KOrdreHode.LevStatus = '60' AND KordreHode.Butik > 0 THEN DO:
    ASSIGN KOrdreHode.SendingsNr = 'MAKULERT'.       
    for each kordrelinje of kordrehode where kordrelinje.plukkbutikk = kordrehode.butik:
        if kordrelinje.plockstatus = 1 then
            kordrelinje.plockstatus = 0.
    end.
END.
/*/* Utleveres i butikk - NB: Flyttes til innlesning fra PHX. */              */
/*IF KOrdreHode.LevFNr = 8 AND                                                */
/*    KOrdreHode.SendingsNr = '' THEN                                         */
/*    DO:                                                                     */
/*        FIND LeveringsForm OF KOrdreHode NO-LOCK NO-ERROR.                  */
/*        IF AVAILABLE LeveringsForm THEN                                     */
/*            ASSIGN KOrdreHode.SendingsNr = LeveringsForm.LevFormBeskrivelse.*/
/*    END.                                                                    */

/* Logger for eksport til Nettbutikk. */  
IF KOrdreHode.Opphav = 10 AND INTEGER(KOrdreHode.LevStatus) > 30 THEN 
  NETTBUTIKK:
  DO:
    IF cNettbutikstyp <> "2" AND INTEGER(KOrdreHode.LevStatus) < 50 THEN
        LEAVE.
    RUN Bibl_LoggDbFri.p(cLogg,'KOrdreId/EkstOrdreNr: ' + STRING(KOrdreHode.KORdre_Id) + '/' + STRING(KOrdreHode.EkstOrdreNr) + 
                         ' Opphav: ' + STRING(KOrdreHode.Opphav) + 
                         ' FraLevStatus: ' + Old_KOrdreHode.LevStatus + 
                         ' TilLevStatus: ' + KOrdreHode.LevStatus + 
                         ' SendingsNr: ' + KOrdreHode.SendingsNr + 
                         ' Er ShipmentSendt tidligere: ' + (IF KOrdreHode.ShipmentSendt = ? THEN '?' ELSE STRING(KOrdreHode.ShipmentSendt)) + 
                         ' NOW: ' + STRING(NOW)
                         ).

    /* Er ordren makulert, står det 'MAKULERT30' i sendingsnr og status 60. */
    /* Da skal det sendes shippingmelding med 0 i antall.                   */
    /* Står det 'MAKULERT50', skal det IKKE sendes shippingordre. */
    IF KOrdreHode.LevStatus = '60' AND KOrdreHode.SendingsNr = 'MAKULERT50' THEN 
        LEAVE NETTBUTIKK.                
          
    /* Shipment melding er sendt tidligere, og skal ikke sendes på nytt. */      
    IF KOrdreHode.ShipmentSendt <> ? THEN
        LEAVE NETTBUTIKK.
          
    /* Varebytte skal IKKE ha ELoggs poster. */
    IF NOT KordreHode.EkstOrdreNr MATCHES '*BYTTE*' THEN 
    DO:
      FIND FIRST trgEkstEDBSystem WHERE 
          trgEkstEDBSystem.DataType = "WEBBUT" AND 
          trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE trgEkstEDBSystem THEN
      WEBBUTIKK:
      DO:
          trgcTabellNavn = IF KordreHode.EkstOrdreNr MATCHES '*RETUR*'
                                THEN "RETURKOrdreHode"
                                ELSE "KOrdreHode".
          FIND ELogg EXCLUSIVE-LOCK WHERE 
               ELogg.TabellNavn     = trgcTabellNavn AND
               ELogg.EksterntSystem = "WEBBUT"    AND
               ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id) NO-ERROR NO-WAIT.
          IF LOCKED ELogg THEN 
              LEAVE WEBBUTIKK.
          ELSE IF NOT AVAIL Elogg THEN 
          DO:
              CREATE Elogg.
              ASSIGN ELogg.TabellNavn     = trgcTabellNavn
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
  END. /* NETTBUTIKK */  
  



