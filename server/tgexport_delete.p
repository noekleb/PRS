/* Registrer Slett SIEEksport record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST TGExport WHERE TGExport.TGExportId = DEC(ihBuffer:BUFFER-FIELD('TGExportId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL TGExport THEN
    DO:
      FOR EACH TGSales OF TGExport:
        DELETE TGSales.
      END.
      FOR EACH TGSales_Ext OF TGExport:
        DELETE TGSales_Ext.
      END.
      FOR EACH TGTimeStamp OF TGExport:
        DELETE TGTimeStamp.
      END.
      FOR EACH TGEmp OF TGExport:
        DELETE TGEmp.
      END.

      DELETE TGExport NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL TGExport THEN RELEASE TGExport.
  hQuery:GET-NEXT().
END.

