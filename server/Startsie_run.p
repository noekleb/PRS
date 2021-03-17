DEF VARIABLE dFraDato AS DATE NO-UNDO.
DEF VARIABLE dTilDato AS DATE NO-UNDO.
DEF VARIABLE cButLst  AS CHARACTER NO-UNDO.
DEF VARIABLE iAntLest     AS INT NO-UNDO.
DEF VARIABLE iAntPostert  AS INT NO-UNDO.
DEF VARIABLE cMsgs AS CHAR NO-UNDO.
DEFINE VARIABLE lfirsta AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cLoggFil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLoggText AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRet AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE tt_Sie NO-UNDO
    FIELD SIEEksportNr AS DEC.

ASSIGN dFraDato = TODAY - 7
       dTilDato = TODAY - 1.

ASSIGN lfirsta = TRUE.
BUTIK_LOOP:
 FOR EACH Butiker NO-LOCK:
     IF Butiker.Sentrallager = TRUE THEN
       NEXT BUTIK_LOOP.
     IF Butiker.ApningsDato > TODAY - 1 THEN
       NEXT BUTIK_LOOP.
     IF Butiker.NedlagtDato < TODAY THEN
       NEXT BUTIK_LOOP.
     IF Butiker.harButikksystem = FALSE THEN
       NEXT BUTIK_LOOP.
     IF lfirsta = TRUE THEN
     DO:
       ASSIGN lfirsta = FALSE.
       ASSIGN cButLst = STRING(Butiker.Butik).
     END.
     ELSE
       ASSIGN cButLst = cButLst + "," + STRING(Butiker.Butik).
 END.

/* MESSAGE "Butiker: "  SKIP cButLst
     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/*ASSIGN cButLst = "1,2,3,4,5,6,7,8,9,10,12".*/
ASSIGN cLoggText = "Datum: " + STRING(dFraDato) + "-" + STRING(dTilDato) + " Butiker: " + cButLst.
RUN bibl_logg.p ("Startsie", cLoggText).

RUN bibl_logg.p ("Startsie", 'Starter Generersieeksport.p').
RUN Generersieeksport.p (dFraDato, dTilDato, cButLst, INPUT-OUTPUT iAntLest, INPUT-OUTPUT iAntPostert, OUTPUT cMsgs).
RUN bibl_logg.p ("Startsie", 'Ferdig Generersieeksport.p').

IF cMsgs <> "" THEN
DO:
RUN bibl_logg.p ("Startsie","Generering av sieexport avbrutet pga test: " + cMsgs).
/*MESSAGE "Generering av sieexport avbrutet pga test: " SKIP(1)
    cMsgs
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.
ELSE
DO:
  ASSIGN iAntLest = 0.
  FOR EACH SIEEksport NO-LOCK:
      IF SIEEksport.EkspDato = ? THEN
        ASSIGN iAntLest = iAntLest + 1.
/*        DISPLAY SIEEksport.SIEEksportNr SIEEksport.ButikkNr SIEEksport.Salgsdato.*/
  END.
  IF iAntLest > 0 THEN
  DO:
    FOR EACH SIEEksport WHERE SIEEksport.EkspDato = ? NO-LOCK:
        CREATE tt_Sie.
        ASSIGN tt_Sie.SIEEksportNr = SIEEksport.SIEEksportNr.
    END.
    RUN bibl_logg.p ("Startsie"," Antal skapade filer: " + STRING(iAntLest)).
    RUN bibl_logg.p ("Startsie", 'Starter sieeksport_eksporter.p').
    RUN sieeksport_eksporter.p ("",TEMP-TABLE tt_Sie:DEFAULT-BUFFER-HANDLE,"",OUTPUT cRet, OUTPUT lOK).
    RUN bibl_logg.p ("Startsie", 'Ferdig sieeksport_eksporter.p').
/*     RUN bibl_logg.p ("Startsie"," Antal skapade filer: " + STRING(iAntLest)). */
/*     RUN bibl_logg.p ("Startsie", 'Starter sieeksport_eksporterX.p').          */
/*     RUN sieeksport_eksporterX.p.                                              */
/*     RUN bibl_logg.p ("Startsie", 'Ferdig sieeksport_eksporterX.p').           */
  END.
  ELSE
    RUN bibl_logg.p ("Startsie","Inget att behandla i sieeksport_eksporterX.p").
/*    MESSAGE "Inget att behandla i sieeksport_ghg.p"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.

QUIT.
