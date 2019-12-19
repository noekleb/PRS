/* send_pksdl_email.p */ 

DEFINE INPUT PARAMETER lPkSdlId LIKE PksdlHode.PkSdlId NO-UNDO.

DEFINE VARIABLE cPdfFil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX         AS INTEGER   NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

SUBSCRIBE 'SendPakkseddel' ANYWHERE .
SUBSCRIBE 'GetSendPakkseddel' ANYWHERE.

ASSIGN 
    cLogg = 'send_pksdl_email' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).

/* Er det ikke satt opp noen mottager, skal det ikke sendes noe. */
{syspara.i 50 50 28 cTekst}
IF cTekst = '' THEN
DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Mangler mottager for eMail syspara 50 50 28.' 
        ).
    RETURN.
END.

/* Liste over butikker som skal ha varsel. */
{syspar2.i 50 50 33 cSendLst}
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Varslingsliste: ' + cSendLst
    ).

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId NO-ERROR. 
IF AVAILABLE PkSdlHode THEN
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF AVAILABLE PkSdlLinje THEN
    FIND Butiker NO-LOCK WHERE 
        butiker.butik = PkSdlLinje.ButikkNr NO-ERROR.

IF NOT AVAILABLE PkSdlLinje OR 
   NOT AVAILABLE Butiker OR
   NOT CAN-DO(cSendLst,STRING(Butiker.butik)) THEN
   DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ' + STRING(lPkSdlId) + '. Den mangler pksdllinje eller har et ukjent butikknr. Eller den skal ikke ha varsel.' 
            ).
       RETURN.
   END.
ELSE  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Sender og skriver Butikk/Pakkseddel/PkSdlId: ' +
                    STRING(PkSdlLinje.ButikkNr) + '/' +
                    PkSdlHode.PkSdlNr + '/' +
                    STRING(PkSdlHode.PkSdlId) + '.'
    ).

/* Sending av pakkseddel gjøres fra utskriftsrutinen etter at pdf. er laget. */
RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|",TRUE,Butiker.RAPPrinter + '|NO',1,"",1).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).

PROCEDURE SendPakkseddel:
    DEF INPUT PARAMETER icPdfFil AS CHAR NO-UNDO.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  SendPakkseddel Fil: ' + icPdfFil 
        ).

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Varemottak i: ' + STRING(Butiker.Butik) 
        ).

    rSendEMail:parMailType        = 'PAKKSEDDEL'.
    rSendEMail:parSUBJECT         = "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + ' i butikk ' + STRING(Butiker.butik) +
                                    " " + Butiker.ButNamn + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMESSAGE         =  "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + " foretatt i butikk " +
                                     STRING(Butiker.butik) +
                                     " " + Butiker.ButNamn + ".  " +
                                     REPLACE(PkSdlHode.Merknad,CHR(10),' ') + '  ' +
                                     REPLACE(PkSdlHode.MeldingFraLev,CHR(10),' ').
    rSendEMail:parFILE            = icPdfFil.  
    rSendEMail:send( ).

END PROCEDURE.

PROCEDURE GetSendPakkseddel:
    DEF OUTPUT PARAMETER obSend AS LOG NO-UNDO.

    obSend = TRUE.
END PROCEDURE.

