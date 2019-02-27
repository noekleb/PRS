
/*------------------------------------------------------------------------
    File        : tmpTblKordreHode.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Mon Dec 10 18:31:56 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE tmpKOrdreHode NO-UNDO SERIALIZE-NAME "tmpKOrdreHode"
    FIELD KOrdre_Id            AS DECIMAL     DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
    FIELD EkstOrdreNr          AS CHARACTER   FORMAT "X(15)" LABEL "Ekst.ordrenr"
    FIELD SendingsNr           AS CHARACTER   FORMAT "X(30)" LABEL "Sendingsnummer"
    FIELD ReturNr              AS CHARACTER   FORMAT "x(30)" LABEL "Retur nr."
    FIELD DatoTidEndret        AS DATETIME 
    INDEX idxKOrdre_Id AS PRIMARY UNIQUE KOrdre_Id
    INDEX idxEkstOrdreNr EkstOrdreNr
    INDEX idxSendingsNr SendingsNr 
    .


