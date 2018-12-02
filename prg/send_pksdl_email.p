/* send_pksdl_email.p */ 

DEFINE INPUT PARAMETER lPkSdlId LIKE PksdlHode.PkSdlId NO-UNDO.

DEFINE VARIABLE cPdfFil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX         AS INTEGER   NO-UNDO.

/* Er det ikke satt opp noen mottager, skal det ikke sendes noe. */
{syspara.i 50 50 28 cTekst}
IF cTekst = '' THEN 
    RETURN.

/* Liste over butikker som skal ha varsel. */
{syspar2.i 50 50 33 cSendLst}

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId NO-ERROR. 
IF AVAILABLE PkSdlHode THEN
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF AVAILABLE PkSdlLinje THEN
    FIND Butiker NO-LOCK WHERE 
        butiker.butik = PkSdlLinje.ButikkNr NO-ERROR.

ASSIGN 
    cLogg = 'send_pksdl_email' + REPLACE(STRING(TODAY),'/','')
    .

IF NOT AVAILABLE PkSdlLinje OR 
   NOT AVAILABLE Butiker OR
   NOT CAN-DO(cSendLst,STRING(Butiker.butik)) THEN
   DO: 
/*        RUN bibl_loggDbFri.p (cLogg,'** Kan ikke sende pakkseddel med PkSdlId: ' +                                                */
/*                        STRING(lPkSdlId) + '. Den mangler pksdllinje eller har et ukjent butikknr. Eller den skal ikke ha varsel.'*/
/*                        ).                                                                                                        */
       RETURN.
   END.
ELSE  
/*    RUN bibl_loggDbFri.p (cLogg,'Sender og skriver Butikk/Pakkseddel/PkSdlId: ' +*/
/*                    STRING(PkSdlLinje.ButikkNr) + '/' +                          */
/*                    PkSdlHode.PkSdlNr + '/' +                                    */
/*                    STRING(PkSdlHode.PkSdlId) + '.'                              */
/*                    ).                                                           */

SUBSCRIBE 'SendPakkseddel' ANYWHERE .
SUBSCRIBE 'GetSendPakkseddel' ANYWHERE.

/* Sending av pakkseddel gjøres fra utskriftsrutinen etter at pdf. er laget. */
RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|",TRUE,Butiker.RAPPrinter,1,"",1).

PROCEDURE SendPakkseddel:
    DEF INPUT PARAMETER icPdfFil AS CHAR NO-UNDO.
    
    FILE-INFO:FILE-NAME = icPdfFil.

    RUN sendmail_tsl.p ("PAKKSEDDEL",
                        "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + ' i butikk ' + STRING(Butiker.butik) + 
                            " " + Butiker.ButNamn + ".",
                        FILE-INFO:FULL-PATHNAME,
                        "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + " foretatt i butikk " + 
                            STRING(Butiker.butik) + 
                            " " + Butiker.ButNamn + ".  " + 
                            REPLACE(PkSdlHode.Merknad,CHR(10),' ') + '  ' + 
                            REPLACE(PkSdlHode.MeldingFraLev,CHR(10),' '),                        "",
                        "") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        DO:
            RUN bibl_loggDbFri.p (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                RUN bibl_loggDbFri.p (cLogg, '          ' 
                    + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                    ).
            END.            
        END.
/*    ELSE                                                                                                */
/*        RUN bibl_loggDbFri.p (cLogg,'    OK. eMail sendt med vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').*/
    
END PROCEDURE.

PROCEDURE GetSendPakkseddel:
    DEF OUTPUT PARAMETER obSend AS LOG NO-UNDO.

    obSend = TRUE.
END PROCEDURE.

