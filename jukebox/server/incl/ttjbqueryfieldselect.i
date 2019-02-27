/* Field tables for jbquery: */

DEF TEMP-TABLE ttSelected
  FIELD iPos         AS INT
  FIELD iEntityId    AS INT
  FIELD iIndexSeq    AS INT
  FIELD cTabName     AS CHAR
  FIELD cFieldName   AS CHAR
  FIELD cLabel       AS CHAR
  FIELD cDataType    AS CHAR
  FIELD cDesc        AS CHAR
  INDEX idxPos iPos.
 
DEF TEMP-TABLE ttAvail
  FIELD iEntityId    AS INT
  FIELD iIndexSeq    AS INT
  FIELD cTabName     AS CHAR
  FIELD cFieldName   AS CHAR
  FIELD cLabel       AS CHAR
  FIELD cDataType    AS CHAR
  FIELD cDesc        AS CHAR
  INDEX idxTabField  IS UNIQUE cTabName cFieldName.

DEF TEMP-TABLE ttCriteria
  FIELD iPos       AS INT
  FIELD cTable     AS CHAR
  FIELD cFieldname AS CHAR
  FIELD cTabField  AS CHAR
  FIELD cDataType  AS CHAR
  FIELD iSequence  AS INT
  FIELD cAndOr     AS CHAR
  FIELD cOp        AS CHAR
  FIELD cValue     AS CHAR
  FIELD cLeftPar   AS CHAR
  FIELD cRightPar  AS CHAR
  INDEX idxPos iPos.
