/* jbserv_getfieldcache.p 
   Purpose: Get field definitions for caching
   Created: 09.06.07 by brynjar@chemistry.no
-------------------------------------------------------------------------*/                  
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix AS INT NO-UNDO.

{incl/validatesession.i}

DEF NEW SHARED TEMP-TABLE tt_field
    FIELD Db-Name          AS CHAR
    FIELD Table-Name       AS CHAR 
    FIELD Field-Name       AS CHAR
    FIELD Field-Type       AS CHAR
    FIELD Field-Label      AS CHAR
    FIELD Field-Format     AS CHAR
    FIELD Field-Initial    AS CHAR
    FIELD Field-Help       AS CHAR
    FIELD Field-Order      AS INT
    FIELD Field-Extent     AS INT
    FIELD Table-Can-Read   AS CHAR
    FIELD Table-Can-Write  AS CHAR
    FIELD Table-Can-Create AS CHAR
    FIELD Table-Can-Delete AS CHAR
    FIELD Field-Can-Read   AS CHAR
    FIELD Field-Can-Write  AS CHAR
    INDEX idxFileField Table-Name Field-Name
    INDEX idxField Field-Name
    .

DEF VAR htt_field AS HANDLE NO-UNDO.
htt_field = BUFFER tt_field:HANDLE.
ohTempTable = htt_field:TABLE-HANDLE.

REPEAT ix = 1 TO NUM-DBS:
  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(ix)).
  RUN jbserv_getfieldcache_db.p (LDBNAME(ix)).
END.
IF ix > 1 THEN DO:
  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(1)).
END.


