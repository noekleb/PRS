/*------------------------------------------------------------------------
    File        : webButikk.w 
    Purpose     : Eksport av varer til web butikk
    
    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 18 jun 08
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.
DEFINE VAR oc2Retur AS CHARACTER  NO-UNDO.

RUN webButStd.w (INPUT 'WEBBUT', OUTPUT ocRetur).
