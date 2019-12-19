TRIGGER PROCEDURE FOR WRITE OF KOrdreLinje.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgKOrdreHode    FOR KordreHode.
DEFINE BUFFER trgKOrdreLinje   FOR KOrdreLinje.
DEFINE BUFFER bufKOrdreLinje   FOR KOrdreLinje.

DEF VAR trgcTabellNavn AS CHAR NO-UNDO.
DEF VAR lTotal AS DEC NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.KOrdreLinje &Type="W"}

FIND trgKORdreHode OF KOrdreLinje EXCLUSIVE-LOCK NO-ERROR.

/* Hvis ikke dette er gjort ved import av ordre, gjøres det her. */
IF KOrdreLinje.Linjesum <> KOrdreLinje.OrgLinjeSum AND KOrdreLinje.OrgLinjeSum = 0 THEN 
  KOrdreLinje.OrgLinjeSum = KOrdreLinje.LinjeSum. 

/* Logger for eksport til Nettbutikk. */  
IF AVAILABLE trgKordreHode AND trgKOrdreHode.Opphav = 10 AND
  INTEGER(trgKOrdreHode.LevStatus) >= 50 THEN 
  NETTBUTIKK:
  DO:
    /* Shipment melding er sendt tidligere, og skal ikke sendes på nytt. */      
    IF trgKOrdreHode.ShipmentSendt <> ? THEN 
        LEAVE NETTBUTIKK.      

    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAILABLE trgEkstEDBSystem THEN
    WEBBUTIKK:
    DO:
        trgcTabellNavn = IF trgKordreHode.EkstOrdreNr MATCHES '*RETUR*'
                              THEN "RETURKOrdreHode"
                              ELSE "KOrdreHode".
        FIND ELogg EXCLUSIVE-LOCK WHERE 
             ELogg.TabellNavn     = trgcTabellNavn AND
             ELogg.EksterntSystem = "WEBBUT"    AND
             ELogg.Verdier        = STRING(trgKOrdreHode.KOrdre_Id) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN 
            LEAVE WEBBUTIKK.
        ELSE IF NOT AVAIL Elogg THEN 
        DO:
            ASSIGN trgKOrdrEHode.ShipmentSendt = NOW.
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = trgcTabellNavn
                   ELogg.EksterntSystem = "WEBBUT"   
                   ELogg.Verdier        = STRING(trgKOrdreHode.KOrdre_Id).
            RELEASE ELogg.
        END.
        ELSE DO:
            ASSIGN ELogg.EndringsType = 1 
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END.
    END. /* WEBBUTIKK */
  END. /* NETTBUTIKK */  

FOR EACH trgKOrdreLinje OF trgKOrdreHode NO-LOCK:
    /* På vanlige ordre skal bare aktive linjer summeres. */
    IF trgKOrdreHode.SendingsNr <> 'RETUR' THEN
    DO:
      IF trgKOrdreLinje.Aktiv = FALSE THEN
        NEXT.
    END.
    /* På returordre, skal linjer hvor vare er endret på returordre, men ikke på opprinnelig ordre med. */
    ELSE IF trgKOrdreHode.SendingsNr = 'RETUR' AND trgKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
    DO:
      FIND bufKOrdreLinje NO-LOCK WHERE
        bufKOrdreLinje.KOrdre_Id = trgKOrdreHode.RefKOrdre_Id AND
        bufKOrdreLinje.KOrdreLinjeNr = trgKOrdreLinje.KOrdreLinjeNr NO-ERROR.
      IF AVAILABLE bufKORdreLinje AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN
       NEXT.
    END.
    ELSE IF trgKOrdreHode.SendingsNr = 'RETUR' AND trgKOrdreLinje.Aktiv AND trgKORdreLinje.ByttetKOrdreLinjeNr > 0 THEN 
    DO:
      /* VArebytte skal tas med i sum. Blir alltid 0. */
    END.
    ELSE IF trgKOrdreHode.SendingsNr = 'RETUR' AND trgKOrdreLinje.KopiKOrdreLinjeNr = 0 AND trgKOrdreLinje.aktiv = TRUE THEN
    DO:
      /* Skal være med.  Dvs. ikke noe Next her. :) */.
    END.
    /* Skal ikke med. Er nå passive linjer på returordre hvor vare ikke er byttet. */
    ELSE DO:
      NEXT.
    END.
   
    IF trgKOrdreLinje.VareNr = 'BETALT' THEN NEXT.
    lTotal = lTotal + trgKOrdreLinje.LinjeSum.
END.


FIND CURRENT trgKORdreHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAILABLE trgKOrdreHode THEN
    ASSIGN
        trgKOrdreHode.Totalt = lTotal.
IF AVAILABLE trgKOrdreHode THEN RELEASE trgKOrdreHode.

