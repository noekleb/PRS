
/*------------------------------------------------------------------------
    File        : dagsrapp_overfor.p
    Purpose     : 

    Syntax      :

    Description : 


    Author(s)   : tomn
    Created     : Fri Apr 10 11:31:24 CEST 2020
    Notes       :

DEFINE TEMP-TABLE ttOverfor NO-UNDO
  FIELD OvType AS INTEGER FORMAT "9" /* 1=Innkommende, 2=Utgående */
  FIELD ButNr AS INTEGER FORMAT ">>>>>9"
  FIELD FraTilBut AS INTEGER FORMAT ">>>>>9"
  FIELD ButNamn AS CHARACTER FORMAT "x(30)"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  FIELD Beskr AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(30)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
  FIELD StrKode AS INTEGER FORMAT ">>>>>9"
  FIELD Storl AS CHARACTER
  FIELD Antall AS DECIMAL FORMAT ">>>,>>9"
  FIELD Verdi AS DECIMAL FORMAT "->>,>>>,>>9.99"
  INDEX idxOverfor OvType ButNr FraButNr ArtikkelNr Storl
  .
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ttOverfor.i}

DEFINE INPUT PARAMETER bAppend AS LOG NO-UNDO.
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER dDato AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOverfor.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Tømmer tabellen hvis dette er angitt. */
IF bAppend = FALSE THEN 
  EMPTY TEMP-TABLE ttOverfor.

/* Leser alle utgående overføringsposter fra butikken. */
UTGAENDE:
FOR EACH Translogg NO-LOCK WHERE
  TransLogg.Butik = iButNr AND
  TransLogg.Dato  = dDato AND
  Translogg.TTId  = 6:

/*  FIND ttOverfor WHERE                              */
/*    ttOverfor.OvType     = 2 AND                    */
/*    ttOverfor.ButNr      = iButNr AND               */
/*    ttOverfor.FraTilBut  = TransLogg.OvButik AND    */
/*    ttOverfor.ArtikkelNr = TransLogg.ArtikkelNr AND */
/*    ttOverfor.Storl      = TransLogg.Storl NO-ERROR.*/

  /*OPPSTANDELSEN*/
  IF NOT AVAILABLE ttOverfor THEN
    RUN Opprett(2).

  /* Opptelling. */
  IF AVAILABLE ttOverfor THEN
  ASSIGN
    ttOverfor.Antall = ttOverfor.Antall + Translogg.Antall
    ttOverfor.Verdi  = ttOverfor.Verdi + (Translogg.Antall * Translogg.VVareKost)
    .
  IF AVAILABLE ttOverfor THEN 
    RELEASE ttOVerfor.  
END. /* UTGAENDE */

/* Leser alle inngående overføringsposter til butikken. */
UTGAENDE:
FOR EACH Butiker NO-LOCK:
  /* Overfører ikke til seg selv. */
  IF Butiker.butik = ibutNr THEN
    NEXT.
  FOR EACH Translogg NO-LOCK WHERE
    TransLogg.Butik   = Butiker.Butik AND
    TransLogg.Dato    = dDato AND
    Translogg.TTId    = 6 AND 
    TransLogg.OvButik = iButNr:

/*    FIND ttOverfor WHERE                              */
/*      ttOverfor.OvType     = 1 AND                    */
/*      ttOverfor.ButNr      = TransLogg.OvButik AND    */
/*      ttOverfor.FraTilBut  = iButNr AND               */
/*      ttOverfor.ArtikkelNr = TransLogg.ArtikkelNr AND */
/*      ttOverfor.Storl      = TransLogg.Storl NO-ERROR.*/

    /*OPPSTANDELSEN*/
    IF NOT AVAILABLE ttOverfor THEN
      RUN Opprett(1).

    /* Opptelling. */
    IF AVAILABLE ttOverfor THEN
    ASSIGN
      ttOverfor.Antall = ttOverfor.Antall + Translogg.Antall
      ttOverfor.Verdi  = ttOverfor.Verdi + (Translogg.Antall * Translogg.VVareKost)
      .
  END.
  IF AVAILABLE ttOverfor THEN 
    RELEASE ttOVerfor.  
  
END.  /* UTGAENDE */

PROCEDURE Opprett:
  DEFINE INPUT PARAMETER piOvtype AS INTEGER NO-UNDO.
  IF piOvType = 1 THEN 
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = Translog.butik NO-ERROR.
  ELSE 
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = Translog.OvButik NO-ERROR.
  CREATE ttOverfor.
  ASSIGN
    ttOverfor.OvType     = piOvtype
    ttOverfor.ButNr      = IF piOvtype = 2 THEN Translogg.butik ELSE TransLogg.OvButik 
    ttOverfor.FraTilBut  = IF piOvType = 2 THEN TransLogg.OvButik ELSE Translogg.butik
    ttOverfor.ArtikkelNr = TransLogg.ArtikkelNr
    ttOverfor.Storl      = TransLogg.Storl
    ttOverfor.butNamn    = IF AVAILABLE Butiker THEN butiker.butNamn ELSE ''
    ttOVerfor.Dato       = Translogg.Dato
    ttOverfor.Kode       = Translogg.Kode
    .
  FIND StrKonv NO-LOCK WHERE
    StrKonv.Storl = TransLogg.Storl NO-ERROR.
  IF AVAILABLE StrKonv THEN
    ttOverfor.StrKode = StrKonv.StrKode.
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = translogg.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
    ASSIGN
      ttOverfor.Beskr      = ArtBas.Beskr
      ttOverFor.LevKod     = ArtBas.LevKod
      ttOverFor.LevFargKod = ArtBas.LevFargKod      
      .

  IF ttOverfor.Kode = '' AND AVAILABLE StrKonv THEN 
  DO:
    FIND LAST Strekkode NO-LOCK WHERE 
      StrekKode.ArtikkelNr = Translogg.ArtikkelNr AND 
      Strekkode.Strkode    = StrKonv.StrKode NO-ERROR.
    IF AVAILABLE Strekkode THEN 
      ttOverfor.Kode = Strekkode.Kode.
  END.

END PROCEDURE.  
  