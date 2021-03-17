
/*------------------------------------------------------------------------
    File        : tmpPkSdlHode.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE tmpPkSdlHode NO-UNDO SERIALIZE-NAME "tmpPkSdlHode"
    FIELD PkSdlId              AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "PkSdlId" COLUMN-LABEL "PkSdlId"
    FIELD PkSdlNr              AS CHARACTER   FORMAT "X(15)" LABEL "PksdlNr"
    FIELD SendingsNr           AS CHARACTER   FORMAT "X(30)" LABEL "Sendingsnummer"
    FIELD ReturNr              AS CHARACTER   FORMAT "x(30)" LABEL "Retur nr."
    FIELD DatoTidEndret        AS DATETIME 
    FIELD shi_ID               AS CHARACTER 
    INDEX idxPkSdlId AS PRIMARY UNIQUE PksdlId
    INDEX idxPkSdlNr PkSdlNr
    INDEX idxSendingsNr SendingsNr 
    .


