DEF VAR cHovedFil   AS CHAR NO-UNDO.
DEF VAR cDublettFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR cOrgRecord AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR lFakturaNr LIKE FakturaHode.FakturaNr NO-UNDO.
DEF VAR cFakturaFil AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFakturaHode
    FIELD FakturaNr LIKE FakturaHode.FakturaNr
    .

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN 
    cHovedFil   = 'konv\GantFaktura\CREDIT0205_Total.txt'
    cDublettFil = 'konv\GantFaktura\CREDIT0205_Dublett.txt'
    cFakturaFil = 'konv\fakturafil.JSON'
    .

INPUT STREAM Inn FROM VALUE(cHovedFil).
OUTPUT STREAM Ut TO VALUE(cDublettFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED 
        cOrgRecord.
    ASSIGN 
        cRecord = REPLACE(cOrgRecord,'"','').
    lFakturaNr = DEC(ENTRY(2,cRecord,',')).
    /*
    DISPLAY
        ENTRY(1,cRecord,',')
        ENTRY(2,cRecord,',')
        lFakturaNr
        cRecord 
    WITH WIDTH 350.
    */ 
    IF NOT CAN-FIND(ttFakturaHode WHERE 
                    ttFakturaHode.FakturaNr = lFakturaNr) THEN
    DO:
        CREATE ttFakturaHode.
        ASSIGN 
            ttFakturaHode.FakturaNr = lFakturaNr
            .
    END.
    /* Logg dubletter */
    ELSE DO:
      PUT STREAM Ut 
          cOrgRecord
          SKIP.
    END.

    TEMP-TABLE ttFakturaHode:WRITE-JSON('file', cFakturaFil, TRUE).
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM inn CLOSE.

/*
TEMP-TABLE ttConfig:READ-JSON ("File",cConfigFile,"empty"). 
*/
