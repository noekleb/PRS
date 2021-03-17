
/*------------------------------------------------------------------------
    File        : ttRabattertSalg.i
    Purpose     : 

    Syntax      :

    Description : For logging av rabbattert salg i butikk.

    Author(s)   : tomn
    Created     : Fri Apr 10 11:20:00 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRabattertSalg NO-UNDO
  FIELD ButNr AS INTEGER FORMAT ">>>>>9"
  FIELD ButNamn AS CHARACTER FORMAT "x(30)"
  FIELD Dato AS DATE FORMAT "99/99/9999"
  FIELD ForsNr AS INTEGER FORMAT ">>>>>9"
  FIELD FoNamn AS CHARACTER FORMAT "x(30)"
  FIELD SelgerNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD Navn AS CHARACTER FORMAT "x(30)"
  FIELD Kode AS CHARACTER FORMAT "x(20)"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  FIELD Beskr AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(30)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
  FIELD StrKode AS INTEGER FORMAT ">>>>>9"
  FIELD Storl AS CHARACTER
  FIELD Antall AS DECIMAL FORMAT ">>>,>>9"
  FIELD Rabatt AS DECIMAL FORMAT "->>,>>>,>>9.99" 
  FIELD VerdiBut AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD VerdiHk AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD VerdiOutlet AS DECIMAL FORMAT "->>,>>>,>>9.99"
  FIELD RabattOutlet AS DECIMAL FORMAT "->>,>>>,>>9.99" 
  INDEX idxOverfor ButNr ArtikkelNr Storl
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
