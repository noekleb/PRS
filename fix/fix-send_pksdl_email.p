/* fix-send_pksdl_email.p */ 

DEFINE VAR cPdfFil     AS CHAR       NO-UNDO.

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = 138037 NO-ERROR. /* 138037 */
IF AVAILABLE PkSdlHode THEN
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF AVAILABLE PkSdlLinje THEN
    FIND Butiker NO-LOCK WHERE 
        butiker.butik = PkSdlLinje.ButikkNr NO-ERROR.

IF NOT AVAILABLE PkSdlLinje THEN
    RETURN.

SUBSCRIBE 'SendPakkseddel' ANYWHERE .
SUBSCRIBE 'GetSendPakkseddel' ANYWHERE.

RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|",FALSE,'dummy',1,"",1).

PROCEDURE SendPakkseddel:
    DEF INPUT PARAMETER icPdfFil AS CHAR NO-UNDO.

    RUN sendmail_tsl.p ("PAKKSEDDEL",
                        "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + ' i butikk ' + STRING(Butiker.butik) + 
                            " " + Butiker.ButNamn + ".",
                        icPdfFil,
                        "Varemottak av pakkseddel " + PkSdlHode.PkSdlNr + " foretatt i butikk " + 
                            STRING(Butiker.butik) + 
                            " " + Butiker.ButNamn + ".",
                        "",
                        "") .
END PROCEDURE.

PROCEDURE GetSendPakkseddel:
    DEF OUTPUT PARAMETER obSend AS LOG NO-UNDO.

    obSend = TRUE.
END PROCEDURE.


