
/*------------------------------------------------------------------------
    File        : dagsrapp_overfor.p
    Purpose     : 

    Syntax      :

    Description : 


    Author(s)   : tomn
    Created     : Fri Apr 10 11:31:24 CEST 2020
    Notes       :

DEFINE TEMP-TABLE ttRabattertSalg NO-UNDO
  FIELD ButNr AS INTEGER FORMAT ">>>>>9"
  FIELD ButNamn AS CHARACTER FORMAT "x(30)"
  FIELD Dato AS DATE FORMAT "99/99/9999"
  FIELD ForsNr AS INTEGER FORMAT ">>>>>9"
  FIELD FoNamn AS CHARACTER FORMAT "x(30)"
  FIELD SelgerNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD Navn AS CHARACTER FORMAT "x(30)"
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
  INDEX idxOverfor ButNr ArtikkelNr Storl
  .
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ttRabattertSalg.i}
DEFINE INPUT PARAMETER bAppend AS LOG NO-UNDO.
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER dDato AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttRabattertSalg.

DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 22 5 2 cOutletLst}

ASSIGN 
  iProfilNr = 2 /* Outlet profilen. */
  .

/* Tømmer tabellen hvis dette er angitt. */
IF bAppend = FALSE THEN 
  EMPTY TEMP-TABLE ttRabattertSalg.

/* Leser alle salgstransaksjoner for butikken. */
UTGAENDE:
FOR EACH Translogg NO-LOCK WHERE
  TransLogg.Butik = iButNr AND
  TransLogg.Dato  = dDato AND
  Translogg.TTId  = 1:

  IF Translogg.RabKr = 0 THEN 
    NEXT.
    
/*  FIND ttRabattertSalg WHERE                              */
/*    ttRabattertSalg.ButNr      = iButNr AND               */
/*    ttRabattertSalg.ArtikkelNr = TransLogg.ArtikkelNr AND */
/*    ttRabattertSalg.Storl      = TransLogg.Storl NO-ERROR.*/

  /*OPPSTANDELSEN*/
  IF NOT AVAILABLE ttRabattertSalg THEN
    RUN Opprett(2).

  /* Opptelling. */
  IF AVAILABLE ttRabattertSalg THEN
  ASSIGN
    ttRabattertSalg.Antall   = ttRabattertSalg.Antall   + Translogg.Antall
    ttRabattertSalg.Rabatt   = ttRabattertSalg.Rabatt   + Translogg.RabKr
    ttRabattertSalg.VerdiBut = ttRabattertSalg.VerdiBut + (Translogg.Antall * Translogg.Pris)
    ttRabattertSalg.VerdiHk  = ttRabattertSalg.VerdiHk  + (Translogg.Antall * Translogg.Pris)
    .
  IF CAN-DO(cOutletLst,STRING(TransLogg.Butik)) THEN 
  DO:
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = Translogg.ArtikkelNr AND 
      ArtPris.ProfilNr   = iProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN 
      ASSIGN 
        ttRabattertSalg.VerdiOutlet  = ttRabattertSalg.VerdiOutlet + (ArtPris.Pris[1] * TransLogg.Antall)
        ttRabattertSalg.RabattOutlet = ttRabattertSalg.RabattOutlet + ((ArtPris.Pris[1] - (Translogg.Pris - Translogg.RabKr)) * TransLogg.Antall)
        . 
    
  END.
  RELEASE ttRabattertSalg.
END. /* UTGAENDE */

PROCEDURE Opprett:
  DEFINE INPUT PARAMETER piOvtype AS INTEGER NO-UNDO.
  IF piOvType = 1 THEN 
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = Translog.butik NO-ERROR.
  ELSE 
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = Translog.OvButik NO-ERROR.
  CREATE ttRabattertSalg.
  ASSIGN
    ttRabattertSalg.ButNr      = Translogg.butik 
    ttRabattertSalg.ArtikkelNr = TransLogg.ArtikkelNr
    ttRabattertSalg.Storl      = TransLogg.Storl
    ttRabattertSalg.butNamn    = IF AVAILABLE Butiker THEN butiker.butNamn ELSE ''
    ttRabattertSalg.Dato       = Translogg.Dato
    ttRabattertSalg.ForsNr     = TransLogg.ForsNr
    ttRabattertSalg.SelgerNr   = Translogg.SelgerNr
    ttRabattertSalg.Kode       = Translogg.Kode     
    .
  FIND StrKonv NO-LOCK WHERE
    StrKonv.Storl = TransLogg.Storl NO-ERROR.
  IF AVAILABLE StrKonv THEN
    ttRabattertSalg.StrKode = StrKonv.StrKode.
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = translogg.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
    ASSIGN
      ttRabattertSalg.Beskr      = ArtBas.Beskr
      ttRabattertSalg.LevKod     = ArtBas.LevKod
      ttRabattertSalg.LevFargKod = ArtBas.LevFargKod
      .
  FIND forsalj NO-LOCK WHERE 
    Forsalj.ForsNr = INT(TransLogg.forsNr) NO-ERROR.
  IF AVAILABLE Forsalj THEN 
    ttRabattertSalg.FoNamn = Forsalj.FoNamn.
  FIND Selger NO-LOCK WHERE 
    Selger.SelgerNr = TransLogg.SelgerNr NO-ERROR.
  IF AVAILABLE Selger THEN 
    ttRabattertSalg.Navn = Selger.Navn.
  IF ttRabattertSalg.Kode = '' AND AVAILABLE StrKonv THEN 
  DO:
    FIND LAST Strekkode NO-LOCK WHERE 
      StrekKode.ArtikkelNr = Translogg.ArtikkelNr AND 
      Strekkode.Strkode    = StrKonv.StrKode NO-ERROR.
    IF AVAILABLE Strekkode THEN 
      ttRabattertSalg.Kode = Strekkode.Kode.
  END.
END PROCEDURE.  
  