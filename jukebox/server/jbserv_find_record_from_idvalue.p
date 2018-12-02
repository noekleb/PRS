/* (Try to) find record description from table name and value of primary key
         
   Created:  13.01.12 by Brynjar Hasle 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.

DEF VAR cTable     AS CHAR   NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hPkFlds    AS HANDLE NO-UNDO EXTENT 10.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.
DEF VAR cPkFlds    AS CHAR   NO-UNDO.
DEF VAR cWhere     AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR cPKvalue   AS CHAR   NO-UNDO.
DEF VAR cPKvalList AS CHAR   NO-UNDO.
DEF VAR bOk        AS LOG    NO-UNDO.
DEF VAR cCharField AS CHAR   NO-UNDO.


ASSIGN cTable     = ENTRY(1,icParam,"¤")
       cPKvalList = ENTRY(2,icParam,"¤")
       .

FIND FIRST _File NO-LOCK
     WHERE _File._File-Name = cTable
     NO-ERROR.
IF AVAIL _File THEN DO:
  FOR EACH _Index NO-LOCK OF _File
      WHERE _Index._Unique
        AND _Index._Active
     ,EACH _Index-Field NO-LOCK OF _Index
     ,FIRST _Field NO-LOCK OF _Index-Field
     BREAK BY _Index._Index-Name
     :
    ASSIGN ix      = ix + 1
           cPkFlds = cPkFlds + (IF cPkFlds NE "" THEN "," ELSE "") 
                   + _Field._Field-Name
           .
    IF ix > 10 THEN DO:
      cPkFlds = "".
      LEAVE.
    END.
    IF LAST-OF(_Index._Index-Name) THEN DO:
      IF ix = NUM-ENTRIES(cPKvalList,"|") THEN 
        LEAVE.
      ASSIGN ix      = 0
             cPkFlds = "".
    END.
  END.

  IF cPkFlds NE "" THEN DO:
    CREATE BUFFER hBuffer FOR TABLE cTable.
    DO iy = 1 TO ix:
      ASSIGN hPkFlds[iy] = hBuffer:BUFFER-FIELD(ENTRY(iy,cPkFlds))
             cPkValue    = ENTRY(iy,cPKvalList).

      IF hPkFlds[iy]:DATA-TYPE = "CHARACTER" THEN
        cPKvalue = QUOTER(cPKvalue).
      ELSE IF hPkFlds[iy]:DATA-TYPE = "DECIMAL" THEN
        cPKvalue = 'DEC("' + cPKvalue + '")'.
      ELSE IF hPkFlds[iy]:DATA-TYPE = "DATE" THEN
        cPKvalue = 'DATE("' + cPKvalue + '")'.

      cWhere = cWhere + hPkFlds[iy]:NAME + " = " + cPKvalue + " AND ".
    END.
    cWhere = TRIM(cWhere," AND ").
    bOk = hBuffer:FIND-UNIQUE(cWhere,NO-LOCK) NO-ERROR.
  
    IF bOk THEN DO:
      FIND FIRST _Field NO-LOCK OF _File   
           WHERE _Field._Data-Type = "CHARACTER"
             AND (_Field._Field-Name MATCHES "*navn*" OR _Field._Field-Name MATCHES "*name*")
           NO-ERROR.
      IF NOT AVAIL _Field THEN
        FIND FIRST _Field NO-LOCK OF _File   
             WHERE _Field._Data-Type = "CHARACTER"
               AND (_Field._Field-Name MATCHES "*besk*" OR _Field._Field-Name MATCHES "*desc*")
             NO-ERROR.
      IF NOT AVAIL _Field THEN
        FIND FIRST _Field NO-LOCK OF _File   
             WHERE _Field._Data-Type = "CHARACTER"
               AND (_Field._Field-Name MATCHES "*tekst*" OR _Field._Field-Name MATCHES "*text*")
             NO-ERROR.
      IF NOT AVAIL _Field THEN
        FIND FIRST _Field NO-LOCK OF _File   
             WHERE _Field._Data-Type = "CHARACTER"
               AND _Field._Field-Name MATCHES "*" + _File._File-Name + "*"
             NO-ERROR.
      IF NOT AVAIL _Field THEN
        FOR EACH _Field NO-LOCK OF _File
            WHERE _Field._Data-Type = "CHARACTER"
            BY _Field._Order:
          cCharField = _Field._Field-Name.
          LEAVE.
        END.
      ELSE cCharField = _Field._Field-Name.

      IF cCharField NE "" THEN
        ocReturn = (IF _File._File-Label NE ? THEN _File._File-Label ELSE _File._File-Name) + ", " 
                 + hBuffer:BUFFER-FIELD(cCharField):LABEL + ": "
                 + hBuffer:BUFFER-FIELD(cCharField):BUFFER-VALUE.
    END.
  END.
END.
DELETE OBJECT hBuffer NO-ERROR.

obOk = ocReturn NE "".
