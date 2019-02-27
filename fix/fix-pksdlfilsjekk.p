DEF VAR iFilNr   AS INT  NO-UNDO.
DEF VAR cFilNavn AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR cPrefix  AS CHAR NO-UNDO.
DEF VAR cEkst    AS CHAR NO-UNDO.

ASSIGN
    cPrefix  = "PKSDL0380"
    cEkst    = ".csv"
    .

REPEAT iFilNr = 0000 TO 0570:

    cFilNavn = cPrefix + STRING(iFilNr,"9999") + cEkst.
    IF NOT CAN-FIND(FIRST VPIFilHode WHERE
                    VPIFilHode.FilNavn = cFilNavn) THEN
        DISPLAY
        cFilNavn
        .
END.

/*
FOR EACH VPIfilHode NO-LOCK WHERE
    VPIFilHode.FilNavn BEGINS "PKSDL03805698" :

    DISPLAY 
        vpifilHode.FilNavn
        VPIFilHode.Dato
        .
END.
*/
