DEF VAR iAntBest1 AS INT NO-UNDO.
DEF VAR iantBest2 AS INT NO-UNDO.
DEF VAR dFraDato AS DATE NO-UNDO.
DEF VAR dTilDato AS DATE NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.

DEF BUFFER bufPkSdlHode FOR PkSdlHode.
DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.
    
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    dFraDato = 11/12/2018
    dTilDato = 11/12/2018
    cFil     = 'konv\fix-SettPkSdlNr_istedenfor_DatoTid' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

DO dDato = dFraDato TO dTilDato:
FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.RegistrertDato = dDato AND 
    LENGTH(PkSdlHode.PkSdlNr) = 12 AND 
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.butikkNr = 10) AND 
    LENGTH(PkSdlHode.PkSdlNr) = 12,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK:

    FIND FIRST bufPkSdlHode NO-LOCK WHERE 
            bufPkSdlHode.PkSdlId = PkSdlHode.PkSdlId - 1 AND 
            bufPkSdlHode.RegistrertDato = PkSdlHode.RegistrertDato AND 
            bufPkSdlHode.RegistrertTid  >= PkSdlHode.RegistrertTid - 60 
            NO-ERROR.
    ASSIGN 
        iAntBest1 = 0
        iantBest2 = 0
        .
    FOR EACH bufPkSdlLinje OF PksdlHode NO-LOCK:
        iAntBest1 = iAntBest1 + bufPkSdlLinje.Antall.
    END.
    FOR EACH bufPkSdlLinje OF bufPksdlHode NO-LOCK:
        iAntBest2 = iAntBest2 + bufPkSdlLinje.Antall.
    END.
    
    IF iAntBest1 = iAntBest2 AND PkSdlHode.PkSdlNr <> bufPkSdlHode.PkSdlNr THEN
    DO:
        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        PUT STREAM Ut UNFORMATTED
            PkSdlHode.PkSdlId ';'
            PkSdlHode.PkSdlNr ';'
            PkSdlHode.RegistrertDato  ';'
            STRING(PkSdlHode.RegistrertTid,"HH:MM:SS") ';'
            bufPkSdlHode.PkSdlId ';'
            bufPkSdlHode.PkSdlNr ';'
            bufPkSdlHode.RegistrertDato ';'
            STRING(bufPkSdlHode.RegistrertTid,"HH:MM:SS")
            SKIP.
        OUTPUT STREAM Ut CLOSE.
        
        DISPLAY
            PkSdlLinje.ButikkNr
            PkSdlHode.PkSdlId
            PkSdlHode.PkSdlNr
            PkSdlHode.RegistrertDato
            STRING(PkSdlHode.RegistrertTid,"HH:MM:SS")
            iAntBest1
            PkSdlHode.Merknad FORMAT "x(40)"
            '|'
            bufPkSdlHode.PkSdlId
            bufPkSdlHode.PkSdlNr
            bufPkSdlHode.RegistrertDato
            STRING(bufPkSdlHode.RegistrertTid,"HH:MM:SS")
            iantBest2
            bufPkSdlHode.Merknad FORMAT "x(40)"
            WITH WIDTH 350.
        
        ASSIGN
            PkSdlHode.Merknad = pkSdlHode.Merknad + CHR(10) + 'Endret pksdlnr fra ' + STRING(PkSdlHode.PkSdlNr)
            PkSdlHode.PkSdlNr = bufPkSdlHode.PkSdlNr
            .
        
    END.

END.
END.

