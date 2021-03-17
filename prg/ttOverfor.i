
/*------------------------------------------------------------------------
    File        : ttOverfor.i
    Purpose     : 

    Syntax      :

    Description : For logging av overføringer inn/ut av en butikk.

    Author(s)   : tomn
    Created     : Fri Apr 10 11:20:00 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOverfor NO-UNDO
  FIELD OvType AS INTEGER FORMAT "9" /* 1=Innkommende, 2=Utgående */
  FIELD ButNr AS INTEGER FORMAT ">>>>>9"
  FIELD FraTilBut AS INTEGER FORMAT ">>>>>9"
  FIELD ButNamn AS CHARACTER FORMAT "x(30)"
  FIELD Dato AS DATE FORMAT "99/99/9999"
  FIELD Kode AS CHARACTER FORMAT "x(20)"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  FIELD Beskr AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(30)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
  FIELD StrKode AS INTEGER FORMAT ">>>>>9"
  FIELD Storl AS CHARACTER
  FIELD Antall AS DECIMAL FORMAT ">>>,>>9"
  FIELD Verdi AS DECIMAL FORMAT "->>,>>>,>>9.99"
  INDEX idxOverfor OvType ButNr FraTilBut ArtikkelNr Storl
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
