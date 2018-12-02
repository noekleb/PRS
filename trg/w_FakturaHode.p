TRIGGER PROCEDURE FOR WRITE OF FakturaHode.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.

{trg\c_w_trg.i &Fil=SkoTex.FakturaHode &Type="W"}

FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "FAKTAUTO" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    /* Endring som skal til ERP system */
    ERPUT:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Fakturahode" AND
             ELogg.EksterntSystem = "FAKTAUTO"    AND
             ELogg.Verdier        = STRING(FakturaHode.Faktura_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Fakturahode"
                   ELogg.EksterntSystem = "FAKTAUTO"   
                   ELogg.Verdier        = STRING(FakturaHode.Faktura_Id).
        END.
        ASSIGN ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
    END. /* ERPUT */

END. /* WEBBUTIKK */


