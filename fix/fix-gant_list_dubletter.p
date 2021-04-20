
/*------------------------------------------------------------------------
    File        : fix-gant_sett_LC_Modell.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon May 10 21:30:20 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntUten AS INTEGER NO-UNDO.
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE bUtfor AS LOG NO-UNDO.
DEF VAR cLagAntLst AS CHAR NO-UNDO.
DEF VAR cLagButLst AS CHAR NO-UNDO.
DEF VAR cEanLst AS CHAR NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.

DEFINE BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER buf2ArtBas FOR ArtBas.
DEFINE BUFFER sanerArtBas FOR ArtBas.

DEFINE STREAM Ut.

DEFINE TEMP-TABLE tmpDublett NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Beskr LIKE ArtBas.Beskr
    FIELD LevNr LIKE ArtBas.LevNr
    FIELD LevKod LIKE ArtBas.LevKod
    FIELD LevFargKod LIKE ArtBas.LevFargKod
    FIELD AntKoder AS INTEGER 
    FIELD Sasong LIKE ArtBas.Sasong
    FIELD KjedeInnkPris LIKE ArtBas.KjedeInnkPris
    INDEX idxArtikkel LevNr LevKod LevFargKod AntKoder.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cUtFil = 'konv\ArtDubletter' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    bUtfor = FALSE 
    .
    
RUN finnDubletter.

OUTPUT STREAM Ut TO VALUE('konv\SanerArtikler.csv').

  PUT STREAM Ut UNFORMATTED 
    'LevNr;'
    'ArtikkelNr;'
    'Beskr;'
    'LevKod;'
    'LevFargKod;'
    'AntKoder;' 
    'Sasong;'
    'KjedeInnkPris;'
    'Handling'
    SKIP.

FOR EACH tmpDublett
  BREAK BY tmpDublett.LevNr
        BY tmpdublett.LevKod
        BY tmpDublett.LevFargKod
        BY tmpDublett.AntKoder DESC:
          
  /* Første artikkel i brytgruppen skal ikke saneres. */
  IF FIRST-OF(tmpDublett.LevFargKod) THEN 
    lArtikkelNr = tmpDublett.ArtikkelNr.
    
  DISPLAY
    tmpDublett.LevNr
    tmpDublett.ArtikkelNr
    tmpdublett.Beskr
    tmpDublett.LevKod
    tmpDublett.LevFargKod
    tmpDublett.AntKoder
    tmpDublett.Sasong
    tmpDublett.KjedeInnkPris
    (IF tmpDublett.ArtikkelNr <> lArtikkelNr THEN 'Saneres til ' + STRING(STRING(lArtikkelNr)) ELSE '') FORMAT "x(40)"
  WITH WIDTH 350.

  PUT STREAM Ut UNFORMATTED  
    tmpDublett.LevNr ';'
    tmpDublett.ArtikkelNr ';'
    tmpdublett.Beskr ';'
    tmpDublett.LevKod ';'
    tmpDublett.LevFargKod ';'
    tmpDublett.AntKoder ';' 
    tmpDublett.Sasong ';'
    tmpDublett.KjedeInnkPris ';'
    (IF tmpDublett.ArtikkelNr <> lArtikkelNr THEN 'Saneres til ' + STRING(STRING(lArtikkelNr)) ELSE '')
  SKIP.
  
  IF tmpDublett.ArtikkelNr <> lArtikkelNr THEN 
  DO FOR ELogg, sanerArtBas TRANSACTION:
    FIND sanerArtBas EXCLUSIVE-LOCK WHERE 
      sanerArtBas.ArtikkelNr = tmpDublett.ArtikkelNr NO-ERROR.
    IF AVAILABLE sanerArtBas THEN 
      DO:
        ASSIGN 
          sanerArtbas.IKasse     = FALSE
          sanerArtBas.Sanertdato = TODAY
          sanerArtBas.Beskr      = 'KORR: ' + sanerArtBas.Beskr.
      END.
    CREATE eLogg.
    ASSIGN 
      ELogg.EksterntSystem  = "KORRHK"
      ELogg.TabellNavn      = "VPIArtBas"
      ELogg.Verdier         = "0|" + STRING(lArtikkelNr) + "|"
                                   + STRING(tmpDublett.ArtikkelNr)
      ELogg.EndringsType    = 1 
      ELogg.Behandlet       = FALSE
      .
      
MESSAGE 'sanerArtBas:' sanerartbas.Beskr
VIEW-AS ALERT-BOX.      
    RELEASE ELogg.
    RELEASE sanerArtBas.
  END. /* TRANSACTION */
  
END.
OUTPUT STREAM Ut CLOSE.

RUN vpikorreksjon.w.

RETURN.

/* ************************  Function Implementations ***************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE finnDubletter:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE pbApen AS LOG NO-UNDO.
    DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE plLagAnt LIKE Lager.Lagant NO-UNDO.

    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.LevNr > 0 AND 
        ArtBas.LevKod > '' AND 
        ArtBas.LevKod = '1000143' AND /* TEST */
/*        ArtBas.LevKod = '1500156' AND*/
        ArtBas.LevFargKod > '' AND 
        NOT CAN-DO('90,92',STRING(ArtBas.Anv-Id)) AND 
        ArtBas.RegistrertDato >= 01/01/2018
        BREAK BY ArtBas.LevNr
        BY ArtBas.LevKod
        BY ArtBas.LevFargKod
        BY ArtBas.Sasong
        BY ArtBas.KjedeInnkPris:

        piAnt = piant + 1.
        
        IF LAST-OF(ArtBas.LevFargKod) THEN
        DO: 
            IF piAnt > 1 THEN
            EKSPORT:          
            DO:
                IF pbApen = FALSE THEN 
                DO:
                    pbApen = TRUE.
                    OUTPUT STREAM Ut TO VALUE(cUtFil).
                    PUT STREAM Ut UNFORMATTED 
                        'ArtikkelNr;'
                        'Beskr;'
                        'LevNr;'
                        'LevKod;'
                        'LevFargKod;'
                        'Sasong;'
                        'KjedeInnkPris;'
                        'TotalLagerBeh;'
                        'RegistrertDato;'
                        'SistEndretDato;'
                        'AntKoder;'
                        'Antall;'
                        'But. med lager;'
                        'Lager ant. pr. butikk;'
                        'Strekkoder'
                    SKIP.
                END.          
                
                FOR EACH bufArtBas NO-LOCK WHERE 
                    bufArtBas.LevNr = ArtBas.LevNr AND 
                    bufArtBas.LevKod = ArtBas.LevKod AND 
                    bufArtBas.LevFargKod = ArtBas.LevFargKod 
                    BREAK BY bufArtBas.LevNr
                    BY bufArtBas.LevKod
                    BY bufArtBas.LevFargKod
                    BY bufArtBas.Sasong:
                
                    ASSIGN 
                        plLagAnt = 0
                        cLagantLst = ''
                        cLagButLst = ''
                        .
                    FOR EACH Lager OF bufArtBas NO-LOCK:
                        IF Lager.Lagant > 0 AND Lager.butik < 99 THEN 
                        DO:
                            ASSIGN 
                                plLagAnt = plLagAnt + Lager.Lagant
                                cLagantLst = cLagantLst + (IF cLagAntLst = '' THEN '' ELSE '|') + STRING(Lager.LagAnt)
                                cLagButLst = cLagButLst + (IF cLagButLst = '' THEN '' ELSE '|') + STRING(Lager.Butik)
                                .
                        END.
                    END.
                    
                    cEanLst = ''.
                    FOR EACH Strekkode NO-LOCK WHERE 
                        Strekkode.ArtikkelNr = bufArtBas.ArtikkelNr:
                        cEanLst = cEanLst + 
                                  (IF cEanLst = '' THEN '' ELSE ',') + 
                                  Strekkode.Kode.
                    END.
                    
                    /* Logger dublettene. */
                    DO:
                      CREATE tmpDublett.
                      ASSIGN 
                        tmpDublett.LevNr         = bufArtBas.LevNr 
                        tmpdublett.LevKod        = bufArtBas.LevKod 
                        tmpDublett.LevFargKod    = bufArtBas.LevFargKod
                        tmpDublett.ArtikkelNr    = bufArtBas.ArtikkelNr
                        tmpDublett.AntKoder      = (IF cEanLst = '' THEN 0 ELSE NUM-ENTRIES(cEanLst))
                        tmpDublett.Sasong        = bufArtBas.Sasong
                        tmpDublett.KjedeInnkPris = bufArtBas.KjedeInnkPris
                        tmpdublett.Beskr         = bufArtBas.Beskr
                        .
                    END.                      
                    
                    PUT STREAM Ut UNFORMATTED   
                        bufArtBas.ArtikkelNr ';'
                        bufArtBas.Beskr ';'
                        bufartBas.LevNr ';'
                        bufArtBas.LevKod ';'
                        bufArtBas.LevFargKod ';'
                        bufArtBas.Sasong ';'
                        bufArtBas.KjedeInnkPris ';'
                        plLagAnt ';'
                        bufArtBas.RegistrertDato ';'
                        bufArtBas.EDato ';'
                        (IF cEanLst = '' THEN 0 ELSE NUM-ENTRIES(cEanLst)) ';'
                        piAnt ';'
                        cLagButLst ';'
                        cLagAntLst ';'
                        cEanLst
                    SKIP.
/*                DISPLAY                 */
/*                    ArtBas.ArtikkelNr   */
/*                    ArtBas.Beskr        */
/*                    artBas.LevNr        */
/*                    ArtBas.LevKod       */
/*                    ArtBas.LevFargKod   */
/*                    ArtBas.Sasong       */
/*                    ArtBas.KjedeInnkPris*/
/*                    iAntUten            */
/*                WITH WIDTH 350.         */
                END.
            END. /* EKSPORT */
            piAnt = 0.
        END. /* LASST-OF */    
    END.    
    IF pbApen THEN 
        OUTPUT STREAM Ut CLOSE.
END PROCEDURE.


