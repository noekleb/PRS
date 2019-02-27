
/*------------------------------------------------------------------------
    File        : JBoxLogFuncToMeth.p
    Purpose     : Wrapper for log library functions so they can be re-used in classes with same name

    Author(s)   : Brynjar
    Created     : Thu Sep 15 15:21:54 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

DEF INPUT  PARAM icFunctionName AS CHAR    NO-UNDO.
DEF INPUT  PARAM icTypeList     AS CHAR    NO-UNDO.
DEF INPUT  PARAM icValueList    AS CHAR    NO-UNDO. /* chr(1) */
DEF INPUT  PARAM ihHandle       AS HANDLE  NO-UNDO.
DEF OUTPUT PARAM obReturn       AS LOGICAL NO-UNDO.

{JBoxFuncToMeth.i obReturn}

