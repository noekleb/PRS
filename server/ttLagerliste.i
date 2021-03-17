
/*------------------------------------------------------------------------
    File        : ttLagerliste.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Tue Dec 17 13:13:47 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLagerListe NO-UNDO
  FIELD PkSdlNr AS CHARACTER FORMAT "x(15)"
  FIELD SendtDato AS DATE FORMAT "99/99/9999"
  FIELD Innlevert AS CHAR FORMAT "x(12)"
  FIELD PkSdlOpphav AS INTEGER FORMAT ">>9"
  FIELD cPalleNr AS CHARACTER FORMAT "x(20)" 
  FIELD Lokasjon AS CHARACTER FORMAT "x(20)"
  FIELD Varetype AS CHARACTER FORMAT "x(30)"
  FIELD LagerSesong AS CHARACTER FORMAT "x(15)"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"  
  FIELD SO AS INTEGER FORMAT ">9"
  FIELD Varetekst AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(20)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(20)"
  FIELD Sesong AS INTEGER FORMAT ">>>>>>>9"
  FIELD MainGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD MainGrpTekst AS CHARACTER FORMAT "x(30)"
  FIELD ArtGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD ArtGrpTekst AS CHARACTER FORMAT "x(30)"
  FIELD LC AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD InnkjopsPris AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD WholeSalePris AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntPkSdl AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiLC AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiPkSdl AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiWholeSale AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL10 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiLCL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiWholeSale10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL40 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiLCL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VerdiWholeSale40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD TotAnt AS INTEGER  FORMAT "->>>>>>>9"
  FIELD TotLCVerdi AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD TotVerdiWholeSale AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD TotVerdi AS DECIMAL FORMAT "->>>>>>>>>9.99"
  INDEX idxArtikkelNr AS UNIQUE PRIMARY ArtikkelNr PksdlNr SO
  INDEX idxGant LevKod LevFargKod PkSdlNr SO
  . 
   


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
