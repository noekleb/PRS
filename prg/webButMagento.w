&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : webButMagento.w 
    Purpose     : Eksport av varer til web butikk
    
    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 21/5-12
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.
DEFINE VAR oc2Retur AS CHARACTER  NO-UNDO.

RUN webButStd.w (INPUT 'MAGENTO', OUTPUT ocRetur).

