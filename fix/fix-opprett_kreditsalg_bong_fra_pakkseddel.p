DEF VAR ibutikkNR AS INT NO-UNDO.
DEF VAR cbutikkNr AS CHAR NO-UNDO.
DEF VAR ihBuffer AS HANDLE NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.

DEFINE TEMP-TABLE tmpPkSdlHode LIKE PkSdlHode.
DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.

DEF BUFFER bufButiker FOR Butiker.

FOR EACH PkSdlHode WHERE PkSdlNr = '141490':
    DISPLAY
        PkSdlHode.PkSdlNr
        .

      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        CREATE ttpkSdlLinje.              
        BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
        cButikkNr = STRING(PkSdlLinje.ButikkNr).
      END.
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN 
          FIND bufButiker NO-LOCK WHERE
              bufButiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.

      ihBuffer = BUFFER ttpkSdlLinje:HANDLE.              
      RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).

END.
