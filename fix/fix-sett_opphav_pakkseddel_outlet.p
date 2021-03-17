DEF VAR iMaks AS INT NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR ihBuffer AS HANDLE NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.

DEF TEMP-TABLE ttPkSdlLinje LIKE PkSdlLinje.

DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.

ASSIGN 
    iMaks = 1 /* Sett til 0, da ignoreres den */
    .
CURRENT-WINDOW:WIDTH = 350.

LOOPEN:
FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlNr = '80817154555' AND
    PkSdlHode.PkSdlStatus = 20 AND 
    PkSdlHode.RegistrertDato >= 08/08/2017,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK:

    IF NOT CAN-DO('10,20,40',STRING(PkSdlLinje.butikkNr)) THEN
        NEXT.

    IF PkSdlHode.PkSdlOpphav = 4 THEN
    DO:
        iMaks = iMaks + 1.
        iAnt = iAnt + 1.

        /* Setter opphav. */
        PkSdlHode.PkSdlOpphav = 4.
        
        EMPTY TEMP-TABLE ttPkSdlLinje.
        FOR EACH bufPkSdlLinje OF PkSdlHode NO-LOCK:
            CREATE ttpkSdlLinje.              
            BUFFER-COPY bufpkSdlLinje TO ttpkSdlLinje.
        END.
        ihBuffer = BUFFER ttpkSdlLinje:HANDLE.

        /* Fakturerer */
        RUN pksdl_internsalg.p(USERID('SkoTex'),
                               ihBuffer,
                               '',
                               OUTPUT ocReturn,
                               OUTPUT obOk
                              ).

        DISPLAY
            iAnt
            PkSdlHode.PkSdlNr
            PkSdlHode.PkSdlStatus
            PkSdlHode.RegistrertDato
            PkSdlHode.PkSdlOpphav
            PkSdlLinje.butikkNr
            PkSdlHode.MeldingFraLev FORMAT "x(200)"
            PkSdlHode.RegistrertDato
            PkSdlHode.Merknad FORMAT "x(30)"
        WITH WIDTH 350.
        IF iMaks <> 0 THEN
        DO:
            IF iAnt >= iMaks THEN
                LEAVE LOOPEN.
        END.
    END.
END. /* LOOPEN */
