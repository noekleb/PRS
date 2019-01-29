TRIGGER PROCEDURE FOR WRITE OF KOrdreLinje.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgKOrdreHode    FOR KordreHode.
DEFINE BUFFER trgKOrdreLinje   FOR KOrdreLinje.

DEF VAR trgcTabellNavn AS CHAR NO-UNDO.
DEF VAR lTotal AS DEC NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.KOrdreLinje &Type="W"}

FIND trgKORdreHode OF KOrdreLinje NO-LOCK NO-ERROR.

/* Logger for eksport til Nettbutikk. */  
IF AVAILABLE trgKordreHode AND trgKOrdreHode.Opphav = 10 AND
  INTEGER(trgKOrdreHode.LevStatus) > 30 THEN 
  NETTBUTIKK:
  DO:
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
    IF trgKOrdreLinje.VareNr = 'BETALT' THEN NEXT.
    lTotal = lTotal + trgKOrdreLinje.LinjeSum.
END.
FIND CURRENT trgKORdreHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF AVAILABLE trgKOrdreHode THEN
    ASSIGN
        trgKOrdreHode.Totalt = lTotal.
IF AVAILABLE trgKOrdreHode THEN RELEASE trgKOrdreHode.

