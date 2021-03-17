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

/* TN 25/1-16 Denne rutinen skal ikke røre elogg for web eksport. De behandles av app server rutinen for Phønix. */ 
/*RUN webButStd.w (INPUT 'PHØNIX', OUTPUT ocRetur).*/
