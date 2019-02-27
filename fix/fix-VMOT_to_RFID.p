DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR NO-UNDO.
DEF VAR cEan AS CHAR FORMAT "x(20)" NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rftpSendFile        AS CLASS cls.RFIDEtikett.ftpSendFile    NO-UNDO.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rftpSendFile = NEW cls.RFIDEtikett.ftpSendFile( INPUT 'RFIDTest' ).

ASSIGN
    cFil = 'C:\NSoft\Polygon\PRS\kom\Barex\in\VMOT40918135106.2'
    .

DEF STREAM Inn.

INPUT STREAM inn FROM VALUE(cFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.
    ASSIGN
        cEan = ENTRY(6,cRecord,';').

    DISPLAY
        cEan.
END.
INPUT STREAM Inn CLOSE.
