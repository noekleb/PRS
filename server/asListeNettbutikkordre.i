
/*------------------------------------------------------------------------
    File        : asListeNettbutikkordre.i
    Purpose     : Åpne for at samme tabell definisjon kan benyttes i flere programmer.

    Syntax      :

    Description : Definerer tempTable ttKOrdre.

    Author(s)   : Tom Nøkleby
    Created     : Sat Oct 03 10:03:43 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttKOrdreHode
  FIELD KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>9"
  FIELD Butik AS INTEGER FORMAT ">>>>>9" 
  FIELD LevFNr AS INTEGER FORMAT ">>9"
  FIELD EkstOrdreNr AS CHARACTER FORMAT "x(30)"
  FIELD Navn AS CHARACTER FORMAT "x(40)"
  FIELD MobilTlf AS CHARACTER FORMAT "x(15)"
  FIELD ePostAdresse AS CHARACTER FORMAT "x(20)"
  FIELD LevStatus AS CHARACTER FORMAT "x(4)"
  FIELD DatoTidOpprettet AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  FIELD ShipmentSendt AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
