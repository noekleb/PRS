/* Sjekker om det fins pakkliste med status 10 - og dermed ALARM
   Opprettet: 30.10.2007 brynjar@chemistry.no
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

obOK = CAN-FIND(FIRST PkSdlHode WHERE PkSdlHode.PkSdlStatus = 10).
