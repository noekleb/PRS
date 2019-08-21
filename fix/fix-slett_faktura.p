DEF VAR cFakturaLst AS CHAR NO-UNDO.
DEF VAR lFakturaNr AS DEC FORMAT "->>>>>>>>>>>>>9" NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.

/* Linjene slettes i trigger. */

ASSIGN
    cFakturaLst = '20900894,20900842,20900895,20900896,20900904'
    .

DO  iLoop = 1 TO NUM-ENTRIES(cFakturaLst):
    lFakturaNr = DEC(ENTRY(iLoop,cFakturaLst)).

    FIND FakturaHode EXCLUSIVE-LOCK WHERE 
        FakturaHode.FakturaNr = lFakturaNr NO-ERROR.
    DISPLAY
        lFakturaNr
        FakturaHode.FakturaNr

        .

    PAUSE.
END.
