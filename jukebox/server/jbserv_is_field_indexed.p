/* Deterimine if a database field is indexed
   Created: 18.03.09 by brynjar@chemistry.no
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix AS INT NO-UNDO.

IF NUM-ENTRIES(icParam,".") NE 2 THEN DO:
  obOK = YES.
  RETURN.
END.

REPEAT ix = 1 TO NUM-DBS:
  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(ix)).
  RUN jbserv_is_field_indexed_db.p (LDBNAME(ix),ENTRY(1,icParam,"."),ENTRY(2,icParam,"."),OUTPUT obOK).
  IF obOk THEN LEAVE.
END.
IF ix > 1 THEN DO:
  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(1)).
END.

