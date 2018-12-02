DEFINE VARIABLE cPkSdlLst AS CHAR NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cPkSdlNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE cButikkNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.

ASSIGN 
    cPkSdlLst = '145968,145970,146985'
    .

DO iLoop = 1 TO NUM-ENTRIES(cPkSdlLst):
    cPkSdlNr = ENTRY(iLoop, cPkSdlLst).
    
    EMPTY TEMP-TABLE ttpkSdlLinje.

    FIND LAST PkSdlHode EXCLUSIVE-LOCK WHERE
        PkSdlHode.PkSdlNr = cPkSdlNr NO-ERROR.
        
    DISPLAY 
         PkSdlHode.PkSdlNr.
    PAUSE.
    
    ihBuffer = BUFFER ttpkSdlLinje:HANDLE.              
    
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        CREATE ttpkSdlLinje.              
        BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
        cButikkNr = STRING(PkSdlLinje.ButikkNr).
    END.
    
    RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).

END.




