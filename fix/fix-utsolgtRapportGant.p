
/*------------------------------------------------------------------------
    File        : fix-utsolgtRapportGant.p
    Purpose     : 

    Syntax      :

    Description : - Be om input
                    - Periode for salg
                    - Periode for varemottak (innkjøp)
                    - Butikker
                    - Tilbud (Ja, Nei, Alle)
                  - Lese alle salg i perioden, alle returer og reklamasjoner.
                  - Lese alle varemottak i perioden
                  - Bygge opp ttLager.
                  - Legge ut ttLager i en excel fil.

    Author(s)   : tomn
    Created     : Tue Jul 09 08:38:20 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

DEFINE VARIABLE dDatoSalgFra AS DATE NO-UNDO.
DEFINE VARIABLE dDatoSalgTil AS DATE NO-UNDO.
DEFINE VARIABLE dDatoMottakFra AS DATE NO-UNDO.
DEFINE VARIABLE dDatoMottaktil AS DATE NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE iTilbud AS INTEGER NO-UNDO. /* 0-Alle, 1-Normalpris, 2-Tilbud,  */
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTTIdSalgLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTTIdMotLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButik AS INTEGER NO-UNDO.
DEFINE VARIABLE iStrKode AS INTEGER NO-UNDO.
DEFINE VARIABLE iLager AS INTEGER NO-UNDO.

DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop2 AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttLager 
  FIELD Butik AS INTEGER FORMAT ">>>>9"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD Beskr AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(30)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
  FIELD Storl AS CHARACTER FORMAT "x(20)"
  FIELD StrKode AS INTEGER FORMAT ">>>>9"
  FIELD Kode AS CHARACTER FORMAT "x(20)"
  
  FIELD AntMottatt AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiMottatt AS DECIMAL FORMAT "->>>>>>9"
  FIELD AntSolgt AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiSolgt AS DECIMAL FORMAT "->>>>>>9"
  FIELD AntRetur AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiRetur AS DECIMAL FORMAT "->>>>>>9"
  FIELD AntReklam AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiReklam AS DECIMAL FORMAT "->>>>>>9"
  FIELD AntNettoSolgt AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiNettoSolgt AS DECIMAL FORMAT "->>>>>>9"
  FIELD Utsolgt% AS DECIMAL FORMAT "->>>9.9"
  FIELD Solgt% AS DECIMAL FORMAT "->>>9.9"
  FIELD AntLager AS DECIMAL FORMAT "->>>>>>9"
  FIELD VerdiLager AS DECIMAL FORMAT "->>>>>>9"
  
  INDEX Vare AS UNIQUE Butik ArtikkelNr 
  INDEX Utsolgt AS PRIMARY Butik Beskr LevKod LevFargKod Storl
  .

DEFINE BUFFER bttLager FOR ttLager.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN
  cButLst        = '15,16'
  cTTIdSalgLst   = '1,3,10'
  cTTIdMotLst    = '5'
  dDatoSalgFra   = 07/01/2019
  dDatoSalgTil   = 07/09/2019
/*  dDatoMottakFra = 01/01/2019*/
/*  dDatoMottakFra = 07/09/2019*/
  iTilbud        = 2
  iLager         = 16
  cFil           = 'konv\RappUtsolgt' + REPLACE(STRING(TODAY),'/','') + '.csv'
  .

FIND Butiker NO-LOCK WHERE 
  Butiker.Butik = 15 NO-ERROR.

BUTIKK_LOOP:
DO iLoop1 = 1 TO NUM-ENTRIES(cButLst):
  iButik = INT(ENTRY(iLoop1,cButLst)).
  
  /* Bruker Idx Dato som har Dato TTId Butik som felt. */
  LES_SALG:
  DO:
    DO dDato = dDatoSalgFra TO dDatoSalgTil:
      DO iLoop2 = 1 TO NUM-ENTRIES(cTTIdSalgLst):
        iTTId = INT(ENTRY(iLoop2,cTTIdSalgLst)).
        TRANSLOGGEN:
        FOR EACH TransLogg NO-LOCK WHERE
          TransLogg.Dato  = dDato AND
          TransLogg.TTId  = iTTId AND
          TransLogg.Butik = iButik:

          IF iTilbud = 1 THEN /* Bare artikler med normalpris. */ 
          DO:
            FIND ArtPris NO-LOCK WHERE 
              ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND 
              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF AVAILABLE ArtPris AND ArtPris.tilbud = TRUE THEN 
              NEXT. 
          END.
          IF iTilbud = 2 THEN /* Bare artikler aktive på tilbud */ 
          DO:
            FIND ArtPris NO-LOCK WHERE 
              ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND 
              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF AVAILABLE ArtPris AND ArtPris.tilbud = FALSE THEN 
              NEXT. 
          END.

          FIND FIRST StrKonv NO-LOCK WHERE
            StrKonv.storl = TransLogg.Storl NO-ERROR.
          IF AVAILABLE StrKonv THEN
            iStrKode = StrKonv.strKode.
          ELSE
            iStrKode = 0.

          FIND FIRST ttLager WHERE
            ttLager.Butik      = TransLogg.Butik AND
            ttLager.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
          IF NOT AVAILABLE ttLager THEN
          DO:
            FIND ArtBas NO-LOCK WHERE 
              ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
            CREATE ttLager.
            ASSIGN
              ttLager.Butik      = TransLogg.butik
              ttLager.ArtikkelNr = TransLogg.ArtikkelNr
              ttLager.StrKode    = iStrKode
              ttLager.Storl      = TransLogg.Storl
              ttlager.Beskr      = ArtBas.Beskr          
              ttlager.LevKod     = ArtBas.LevKod         
              ttlager.LevFargKod = ArtBas.LevFargKod     
              ttlager.Kode       = Translogg.Kode           
              .
          END.
          CASE iTTId:
            WHEN 1 THEN
              ASSIGN
                ttLager.antSolgt    = ttLager.antSolgt   + TransLogg.Antall
                ttLager.VerdiSolgt  = ttLager.VerdiSolgt + (TransLogg.Pris * Translogg.antall)
                .
            WHEN 3 THEN
              ASSIGN
                ttLager.antReklam   = ttLager.antReklam   + TransLogg.Antall
                ttLager.VerdiReklam = ttLager.VerdiReklam + ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall)
                .
            WHEN 10 THEN
              ASSIGN
                ttLager.antRetur   = ttLager.antRetur   + TransLogg.Antall
                ttLager.VerdiRetur = ttLager.VerdiRetur + (TransLogg.Pris * TransLogg.Antall)
                .
          END.
        END. /* TRANSLOGGEN */
      END.
    END.
  END. /* LES_SALG */
/*  LES_MOTTAK:                                                                                             */
/*  DO:                                                                                                     */
/*    DO dDato = dDatoSalgFra TO dDatoSalgTil:                                                              */
/*      DO iLoop2 = 1 TO NUM-ENTRIES(cTTIdMotLst):                                                          */
/*        iTTId = INT(ENTRY(iLoop2,cTTIdMotLst)).                                                           */
/*        TRANSLOGGEN:                                                                                      */
/*        FOR EACH TransLogg NO-LOCK WHERE                                                                  */
/*          TransLogg.Dato  = dDato AND                                                                     */
/*          TransLogg.TTId  = iTTId AND                                                                     */
/*          TransLogg.Butik = iButik:                                                                       */
/*                                                                                                          */
/*          IF iTilbud = 1 THEN /* Bare artikler med normalpris. */                                         */
/*          DO:                                                                                             */
/*            FIND ArtPris NO-LOCK WHERE                                                                    */
/*              ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND                                               */
/*              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.                                             */
/*            IF AVAILABLE ArtPris AND ArtPris.tilbud = TRUE THEN                                           */
/*              NEXT.                                                                                       */
/*          END.                                                                                            */
/*          IF iTilbud = 2 THEN /* Bare artikler aktive på tilbud */                                        */
/*          DO:                                                                                             */
/*            FIND ArtPris NO-LOCK WHERE                                                                    */
/*              ArtPris.ArtikkelNr = TransLogg.ArtikkelNr AND                                               */
/*              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.                                             */
/*            IF AVAILABLE ArtPris AND ArtPris.tilbud = FALSE THEN                                          */
/*              NEXT.                                                                                       */
/*          END.                                                                                            */
/*                                                                                                          */
/*          FIND FIRST StrKonv NO-LOCK WHERE                                                                */
/*            StrKonv.storl = TransLogg.Storl NO-ERROR.                                                     */
/*          IF AVAILABLE StrKonv THEN                                                                       */
/*            iStrKode = StrKonv.strKode.                                                                   */
/*          ELSE                                                                                            */
/*            iStrKode = 0.                                                                                 */
/*                                                                                                          */
/*          FIND FIRST ttLager WHERE                                                                        */
/*            ttLager.Butik      = TransLogg.Butik AND                                                      */
/*            ttLager.ArtikkelNr = TransLogg.ArtikkelNr AND                                                 */
/*            ttLager.Storl      = Translogg.Storl NO-ERROR.                                                */
/*          IF NOT AVAILABLE ttLager THEN                                                                   */
/*          DO:                                                                                             */
/*            FIND ArtBas NO-LOCK WHERE                                                                     */
/*              ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.                                          */
/*            CREATE ttLager.                                                                               */
/*            ASSIGN                                                                                        */
/*              ttLager.Butik      = TransLogg.butik                                                        */
/*              ttLager.ArtikkelNr = TransLogg.ArtikkelNr                                                   */
/*              ttLager.Storl      = TransLogg.Storl                                                        */
/*              ttLager.StrKode    = iStrKode                                                               */
/*              ttlager.Beskr      = ArtBas.Beskr                                                           */
/*              ttlager.LevKod     = ArtBas.LevKod                                                          */
/*              ttlager.LevFargKod = ArtBas.LevFargKod                                                      */
/*              ttlager.Kode       = Translogg.Kode                                                         */
/*              .                                                                                           */
/*            FIND FIRST ArtLag NO-LOCK WHERE                                                               */
/*              ArtLag.ArtikkelNr = Translogg.ArtikkelNr AND                                                */
/*              ArtLag.Storl      = TransLogg.Storl AND                                                     */
/*              ArtLag.Butik      = TransLogg.butik NO-ERROR.                                               */
/*            IF AVAILABLE ArtLag THEN                                                                      */
/*            DO:                                                                                           */
/*              FIND FIRST Lager NO-LOCK WHERE                                                              */
/*                Lager.Butik = iLager AND                                                                  */
/*                Lager.ArtikkelNr = Translogg.ArtikkelNr NO-ERROR.                                         */
/*              ASSIGN                                                                                      */
/*                ttLager.AntLager = ArtLag.Lagant                                                          */
/*                ttLager.VerdiLager = (IF Lager.VVareKost <> ? THEN Lager.VVareKost * Artlag.Lagant ELSE 0)*/
/*                .                                                                                         */
/*            END.                                                                                          */
/*          END.                                                                                            */
/*          CASE iTTId:                                                                                     */
/*            WHEN 5 THEN                                                                                   */
/*              ASSIGN                                                                                      */
/*                ttLager.antMottatt    = ttLager.antMottatt   + TransLogg.Antall                           */
/*                ttLager.VerdiMottatt  = ttLager.VerdiMottatt + (TransLogg.Pris * Translogg.antall)        */
/*                .                                                                                         */
/*          END.                                                                                            */
/*        END. /* TRANSLOGGEN */                                                                            */
/*      END.                                                                                                */
/*    END.                                                                                                  */
/*  END. /* LES_MOTTAK */                                                                                   */
END. /* BUTIKK_LOOP */

AKKUMULER:
DO:    
  FOR EACH bttLager:
    FIND FIRST ttLager WHERE 
      ttLager.Butik      = 0 AND 
      ttLager.ArtikkelNr = bttLager.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ttLager THEN 
    DO:
      CREATE ttLager.
      ASSIGN 
        ttLager.Butik      = 0
        ttLager.ArtikkelNr = bttLager.ArtikkelNr
        ttLager.Storl      = bttLager.Storl
        ttlager.Beskr      = bttlager.Beskr          
        ttlager.LevKod     = bttlager.LevKod         
        ttlager.LevFargKod = bttlager.LevFargKod     
        ttlager.Storl      = ''       
        ttlager.StrKode    = 0        
        ttlager.Kode       = ''           
        .
    END. 
    
    ASSIGN 
      ttlager.AntMottatt      =  ttlager.AntMottatt      + bttlager.AntMottatt     
      ttlager.VerdiMottat     =  ttlager.VerdiMottat     + bttlager.VerdiMottat    
      ttlager.AntSolgt        =  ttlager.AntSolgt        + bttlager.AntSolgt       
      ttlager.VerdiSolgt      =  ttlager.VerdiSolgt      + bttlager.VerdiSolgt     
      ttlager.AntRetur        =  ttlager.AntRetur        + bttlager.AntRetur       
      ttlager.VerdiRetur      =  ttlager.VerdiRetur      + bttlager.VerdiRetur     
      ttlager.AntReklam       =  ttlager.AntReklam       + bttlager.AntReklam      
      ttlager.VerdiReklam     =  ttlager.VerdiReklam     + bttlager.VerdiReklam    
      ttlager.AntNettoSolgt   =  ttlager.AntNettoSolgt   + bttlager.AntNettoSolgt  
      ttlager.VerdiNettoSolgt =  ttlager.VerdiNettoSolgt + bttlager.VerdiNettoSolgt
      ttlager.Utsolgt%        =  ttlager.Utsolgt%        + bttlager.Utsolgt%       
      ttlager.AntLager        =  ttlager.AntLager        + bttlager.AntLager       
      ttlager.VerdiLager      =  ttlager.VerdiLager      + bttlager.VerdiLager     
      .
  END.
END. /* AKKUMULER */ 

BEREGN_UTSOLGT:
FOR EACH ttLager WHERE 
  ttLager.butik = 0:

  ASSIGN 
    ttLager.AntLager   = 0
    ttLager.VerdiLAger = 0
    .
  
  FOR EACH ArtLag NO-LOCK WHERE 
    ArtLag.ArtikkelNr = ttLager.ArtikkelNr AND 
    ArtLag.Butik      = 16:

    FIND FIRST Lager NO-LOCK WHERE 
      Lager.Butik = 16 AND 
      Lager.ArtikkelNr = ttLager.ArtikkelNr NO-ERROR.
    ASSIGN 
      ttLager.AntLager   = ttLager.AntLager + ArtLag.Lagant
      ttLager.VerdiLager = ttLager.VerdiLager + (IF Lager.VVareKost <> ? THEN Lager.VVareKost * Artlag.Lagant ELSE 0)
      .
  END. 
    
  ASSIGN
    ttLager.Utsolgt% =  ROUND(((ttlager.AntSolgt + ttLager.AntRetur + ttLager.AntReklam) * 100) / ttlager.AntMottatt,1)
    ttLager.Utsolgt% = IF ttLager.Utsolgt% = ? THEN 0 ELSE ttLager.Utsolgt% 
    ttLager.Solgt%   = ROUND((ttlager.AntSolgt * 100) / (ttlager.AntLager + ttlager.AntSolgt),1)
    ttLager.Solgt%   = IF ttLager.Solgt% = ? THEN 0 ELSE ttLager.Solgt% 
    .
END.

OUTPUT STREAM Ut TO value(cFil).
/*PUT STREAM Ut UNFORMATTED*/
/*/*    'Butik;'*/         */
/*    'ArtikkelNr;'        */
/*    'Beskr;'             */
/*    'LevKod;'            */
/*    'LevFargKod;'        */
/*    'Storl;'             */
/*    'StrKode;'           */
/*    'Kode;'              */
/*    'AntMottatt;'        */
/*    'AntSolgt;'          */
/*    'Utsolgt%;'          */
/*    'AntLager;'          */
/*    'VerdiMottat;'       */
/*    'VerdiSolgt;'        */
/*    'AntRetur;'          */
/*    'VerdiRetur;'        */
/*    'AntReklam;'         */
/*    'VerdiReklam;'       */
/*    'AntNettoSolgt;'     */
/*    'VerdiNettoSolgt;'   */
/*    'VerdiLager'         */
/*  SKIP.                  */

PUT STREAM Ut UNFORMATTED 
/*    'Butik;'*/
    'ArtikkelNr;'
    'Beskr;'
    'LevKod;'
    'LevFargKod;'
    'AntSolgt;'
    'Solgt%;'
    'AntLager;'
    'VerdiSolgt;'
    'VerdiLager;'
    'AntRetur;'
    'VerdiRetur;'
    'AntReklam;'
    'VerdiReklam'
  SKIP.

FOR EACH ttLager WHERE 
  ttLager.butik = 0:
  PUT STREAM Ut UNFORMATTED 
      ttlager.ArtikkelNr      ';'
      ttlager.Beskr           ';'
      ttlager.LevKod          ';'
      ttlager.LevFargKod      ';'
      ttlager.AntSolgt        ';'
      ttlager.Solgt%          ';'
      ttlager.AntLager        ';'
      ttlager.VerdiSolgt      ';'
      ttlager.VerdiLager      ';'
      ttlager.AntRetur        ';'
      ttlager.VerdiRetur      ';'
      ttlager.AntReklam       ';'
      ttlager.VerdiReklam     ';'
    SKIP.
    
/*  PUT STREAM Ut UNFORMATTED          */
/*/*      ttlager.Butik           ';'*/*/
/*      ttlager.ArtikkelNr      ';'    */
/*      ttlager.Beskr           ';'    */
/*      ttlager.LevKod          ';'    */
/*      ttlager.LevFargKod      ';'    */
/*      ttlager.Storl           ';'    */
/*      ttlager.StrKode         ';'    */
/*      ttlager.Kode            ';'    */
/*      ttlager.AntMottatt      ';'    */
/*      ttlager.AntSolgt        ';'    */
/*      ttlager.Utsolgt%        ';'    */
/*      ttlager.AntLager        ';'    */
/*      ttlager.VerdiMottat     ';'    */
/*      ttlager.VerdiSolgt      ';'    */
/*      ttlager.AntRetur        ';'    */
/*      ttlager.VerdiRetur      ';'    */
/*      ttlager.AntReklam       ';'    */
/*      ttlager.VerdiReklam     ';'    */
/*      ttlager.AntNettoSolgt   ';'    */
/*      ttlager.VerdiNettoSolgt ';'    */
/*      ttlager.VerdiLager             */
/*    SKIP.                            */
END.
OUTPUT STREAM Ut CLOSE.
