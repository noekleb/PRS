TRIGGER PROCEDURE FOR WRITE OF PKSDLHode  OLD BUFFER oldPKSDLHode.

{trg/c_w_trg.i &Type="W" &Fil="SkoTex.PKSDLHode"}

/* Logger sending av ordre til ERP system */
IF oldPKSDLHode.PkSdlStatus = 5 AND /* Leses inn */
   PKSDLHode.PkSdlStatus = 6 THEN   /* Sendt leverandør */
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "PKSDLHode" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(PKSDLHode.PkSdlId) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "PKSDLHode"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(PKSDLHode.PkSdlId).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

/* Gjøres det varemottak, sjekk om det skal sendes eMail. Send eMail. */
IF  oldPKSDLHode.PkSdlStatus = 10 AND PKSDLHode.PkSdlStatus = 20 THEN 
    RUN send_pksdl_email.p (PkSdlHode.PkSdlId).
    /*
    RUN pksdl_varsling.p (PkSdlHode.PkSdlId).
    */



