/* sendFakturaEMail.p 

Sende mail med faktura ved:
    * Varemottak i Outlett av pakksedler som kommer fra 20 (Fra overføringer). Opphav = 4.
    * Varemottak i 16 av pakksedler hvor pakkseddel er tatt fra outlet's pakkseddel liste. Opphav = 7.
    * Varemottak i 20 hvor varer er overført fra 16.

*/

DEFINE INPUT PARAMETER lFaktura_Id AS DECIMAL NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMotbutNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE piAktiv AS INTEGER NO-UNDO.
DEFINE VARIABLE cMottagerLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSenderLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/* Er mailsending aktiv? - hvis ikke - avslutt. */
{syspar2.i 50 50 34 piAktiv INT}
/* Gyldige sndere og mottagere */
{syspara.i 50 50 35 cMottagerLst}
{syspar2.i 50 50 35 cSenderLst}

SUBSCRIBE 'fakturaFilNavn' ANYWHERE.

ASSIGN 
    bTest = TRUE 
    cLogg = 'SendEMail' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'sendFakturaEMail:' 
    ).

HANDLING:
DO:
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'SendeMail Start.' 
            ).

    IF piAktiv <> 1 THEN 
    DO:
        ocReturn = 'Sending av fakturamail er ikke aktiver. Se syspara 50 50 34.'.
        LEAVE HANDLING.
    END.

    IF cMottagerLst = '' OR cSenderLst = '' THEN 
    DO:
        ocReturn = 'Gyldige sendere og mottagere er ikke satt opp. Se syspara 50 50 35.'.
        LEAVE HANDLING.
    END.
    
    FIND FakturaHode NO-LOCK WHERE 
        FakturaHode.Faktura_Id = lFaktura_Id NO-ERROR.
    IF NOT AVAILABLE FakturaHode THEN
        DO:
            ocReturn = 'Ukjent fakura id (' + STRING(lFaktura_Id) + ').'.
            LEAVE HANDLING.
        END. 
    FIND Butiker NO-LOCK WHERE 
        Butiker.butik = FakturaHode.butikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        DO:
            ocReturn = 'Ukjent butikk angitt på faktura (' + STRING(FakturaHode.butikkNr) + ').'.
            LEAVE HANDLING.
        END. 
    
    FIND LAST PkSdlHode NO-LOCK WHERE 
        PkSdlHode.FakturaNr = FakturaHode.FakturaNr NO-ERROR.
    IF NOT AVAILABLE PkSdlHode THEN 
        DO:
            ocReturn =  'Ingen pakkseddel koblet til fakturen (' + STRING(FakturaHode.FakturaNr) + ').'.
            LEAVE HANDLING.
        END. 
    
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
    IF AVAILABLE PkSdlLinje THEN 
        iMotButNr = PkSdlLinje.ButikkNr.
    ELSE 
        iMotButNr = 0. 

    /* Sjekk av gylidige sender og mottagere */
    IF NOT CAN-DO(cSenderLst,STRING(FakturaHode.ButikkNr)) OR NOT CAN-DO(cMottagerLst,STRING(iMotButNr)) THEN 
        DO:
            IF bTest THEN 
                ocReturn =  'Ugyldig sender/mottager (' + STRING(FakturaHode.ButikkNr) +  '/' + STRING(iMotButNr) + ').'.
            LEAVE HANDLING.
        END. 
    
    /* Generer bare faktura pdf fil. Sender filnavn tilbake med publsih når kode BAREFIL sendes inn.        */
    /* NB: Legges koden inn i første paremter, virker det ikke selv om allt ser ok ut på mottagersiden :( . */
    RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",TRUE,Butiker.RapPrinter,1,"BAREFIL",1).
END. /* HANDLING */

IF ocReturn <> '' THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + ocReturn 
        ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'sendFakturaEMail ferdig' 
    ).

RETURN ocReturn.

/* Her mottas filnavn og mail sendes. */
PROCEDURE fakturaFilNavn:
    DEF INPUT PARAMETER pcFilNavn AS CHAR NO-UNDO.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Fil: ' + pcFilNavn 
        ).

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Overført fra: ' + STRING(FakturaHode.butikkNr) 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Overført til: ' + STRING(iMotButNr) 
        ).

    rSendEMail:parMailType        = 'FakturaMail'.
    rSendEMail:parSUBJECT         = 'Faktura fra overf. fra ' + STRING(FakturaHode.butikkNr) + ' til ' + STRING(iMotbutNr) + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMESSAGE         =  'Vedrørende pakkseddel: ' + PkSdlHode.PkSdlNr + '.' + CHR(10) +   
                                      PkSdlHode.Merknad.
    rSendEMail:parFILE            = pcFilNavn.  
    rSendEMail:send( ).
END PROCEDURE. 

