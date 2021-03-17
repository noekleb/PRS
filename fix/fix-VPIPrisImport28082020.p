&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fix-VPIPrisImport28082020.p
    Purpose     : Innlesning av priser

    Syntax      : 

    Description :  

    Author(s)   : Tom Nøkleby
    Created     : 28/08/2020
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE iTotAntLinjer       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLinje              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilNavn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntLinjer          AS INTEGER   NO-UNDO.

DEFINE VARIABLE piLinjeNr           AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcLinje             AS CHARACTER NO-UNDO.
DEFINE VARIABLE piAntFeil           AS INTEGER   NO-UNDO. 

DEFINE VARIABLE lDec                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cStr                AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk                 AS LOG       NO-UNDO.
DEFINE VARIABLE piLoop              AS INTEGER   NO-UNDO.
DEFINE VARIABLE cExcelFilNavn       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrorFilNavn       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSheets             AS INTEGER   NO-UNDO.

DEFINE VARIABLE iLevNr              AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE iSesong             AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE cLevKod             AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE cBeskr              AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cFarge              AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE lInnkjopsPris       AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO. 
DEFINE VARIABLE lUtPris             AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l2UtPris            AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS     cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE STREAM InnFil.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INTEGER
  FIELD Tekst   AS CHARACTER
  .
  
DEFINE BUFFER bufArtPris FOR ArtPris.
  
{windows.i}
/*{incl/devmode.i}*/
/*{incl/custdevmode.i}*/
/*{AssignRutiner.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

CURRENT-WINDOW:WIDTH  = 350.
CURRENT-WINDOW:HEIGHT = 40.

FORM 
  WITH FRAME A DOWN WIDTH 350.

IF SEARCH('tnc.txt') <> ? THEN 
  ASSIGN
    cLogg         = 'fix-VPIPrisImport28082020' + REPLACE(STRING(TODAY),'/','')
    cFilNavn      = 'konv\Gant\28082020ArtikkelfilPriser\PriserSko202003.xlsx'
    cFilNavn      = 'konv\Gant\28082020ArtikkelfilPriser\PriserSko202003.csv'
    cErrorFilNavn = 'log\ErrPriserSko202003.txt'
    .
ELSE 
  ASSIGN
    cLogg         = 'fix-VPIPrisImport28082020' + REPLACE(STRING(TODAY),'/','')
    cFilNavn      = 'konv\PriserSko202003.xlsx'
    cFilNavn      = 'konv\PriserSko202003.csv'
    cErrorFilNavn = 'log\ErrPriserSko202003.txt'
    .

/* Er det en excel fil, skal den kovnerteres til csv. */
IF CAN-DO('xls,xlsx',ENTRY(NUM-ENTRIES(cFilNavn,'.'),cFilNavn,'.')) THEN 
DO:
  cExcelFilNavn = cFilNavn.
  RUN bibl_loggDbFri.p (cLogg,
    '  Start Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + '.'
    ).    
  rStandardFunksjoner:konvExcel2csv(cExcelFilNavn,'',OUTPUT cFilNavn, OUTPUT iSheets).
  RUN bibl_loggDbFri.p (cLogg,
    '  Slutt Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cFilNavn + ' iSheets: ' + STRING(iSheets) + '.'
    ).    
END.

RUN LesInnFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
 
&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE b2Ok     AS LOG     NO-UNDO.
  DEFINE VARIABLE iStrKode AS INTEGER NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH ttError:
    DELETE ttError.
  END.

  RUN TellOppLinjer.
  RUN bibl_logg.p (cLogg, 'Leser inn fil: ' + cFilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer) + ' ' + string(TIME,"HH:MM:SS")).

  ASSIGN
    piLinjeNr  = 0
    pcLinje    = ''
    piAntFeil  = 0
    iAntLinjer = 0
    b2Ok       = TRUE 
    .
      
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      pcLinje    = REPLACE(pcLinje,CHR(9),';') /* Bytter ut TAB med semicolon. */
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
      NEXT LESERLINJER.
    IF TRIM(ENTRY(1,pcLinje,';')) = '' THEN 
      NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4;5" THEN
      NEXT LESERLINJER.

    /* Skipper tomme linjer nederst i filen. */
    IF pcLinje BEGINS ";;;;" THEN
      NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN 
      lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 7 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Skal være minst 7 entries) " + STRING(iAntLinjer) + ": " + pcLinje
        .
      NEXT LESERLINJER.
    END.

    /* NB: 0 er tillatte verdier hos FEDAS. */
    ASSIGN 
      iLevNr        = INT(TRIM(ENTRY(1,pcLinje,';')))
      iSesong       = INT(TRIM(ENTRY(2,pcLinje,';'))) 
      cLevKod       = (TRIM(ENTRY(3,pcLinje,';'))) 
      cBeskr        = (TRIM(ENTRY(4,pcLinje,';'))) 
      cFarge        = (TRIM(ENTRY(5,pcLinje,';')))
      lInnkjopsPris = DEC(TRIM(ENTRY(6,pcLinje,';')))
      lUtPris       = DEC(TRIM(ENTRY(7,pcLinje,';')))
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE ttError.
      ASSIGN
        ttError.LinjeNr = piAntFeil
        ttError.Tekst   = "** Feil på linje (Ugyldige tegn i beløpsfelt) " + STRING(iAntLinjer) + ": " + pcLinje
        .
      NEXT LESERLINJER.
    END.
    
    FOR EACH ArtBas  NO-LOCK WHERE 
      ArtBas.LevNr = iLevNr AND 
      ArtBas.Sasong = iSesong AND 
      ArtBas.LevKod = cLevKod:
        
      FIND ArtPris OF ArtBas EXCLUSIVE-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR. 
      FIND bufArtPris OF ArtBas EXCLUSIVE-LOCK WHERE 
        bufArtPris.ProfilNr = 16 NO-ERROR. 
      
      IF AVAILABLE ArtPris AND (ArtPris.Pris[1] = 0 OR ArtPris.InnkjopsPris[1] = 0) THEN 
      DO:
        /* Henter fra Profil 16 hvis det ligger pris der. */
        IF AVAILABLE bufArtPris AND bufArtPris.Pris[1] <> 0 THEN 
          DO:
            BUFFER-COPY bufArtPris
              EXCEPT ProfilNr
              TO ArtPris.
          END.
          
        /* Oppdaterer fra fil */
        IF ArtPris.Pris[1] = 0 THEN 
          DO:
            /* Avrunder til nærmeste hele 10 kr. */
            l2UtPris = ROUND(lUtPris / 10,0) * 10.
            
            /* Kalkulerer opp artikkelen ut fra kjente parametre. */
            ASSIGN
              ArtPris.InnkjopsPris[1] = lInnkjopsPris
              ArtPris.ValPris[1]      = lInnkjopsPris
              ArtPris.Rab1%[1]        = 10
              ArtPris.Rab1Kr[1]       = ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1])/ 100,2) 
              ArtPris.Rab1Kr[1]       = IF ArtPris.Rab1Kr[1] = ? THEN 0 ELSE ArtPris.Rab1Kr[1] 
              ArtPris.VareKost[1]     = ArtPris.InnkjopsPris[1] - ArtPris.Rab1Kr[1]
              ArtPris.Mva%[1]         = 25              
              ArtPris.Pris[1]         = l2UtPris 
              ArtPris.MvaKr[1]        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
              ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
              ArtPris.DB%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.VareKost[1] + ArtPris.DbKr[1]),2)
              ArtPris.DB%[1]          = IF ArtPris.DB%[1] = ? THEN 0 ELSE ArtPris.DB%[1]
              .          
            END.
            
        /* Kopierer til Profil 16 hvis den mangler pris. */
        IF AVAILABLE bufArtPris AND bufArtPris.Pris[1] = 0 THEN 
          DO:
            BUFFER-COPY ArtPris
              EXCEPT ProfilNr
              TO bufArtPris.
          END.
          
        DISPLAY 
          ArtBas.LevNr
          ArtBas.Sasong
          ArtBas.ArtikkelNr
          ArtBas.Beskr
          ArtBas.LevKod
          lInnkjopsPris
          lUtPris
          l2Utpris
          '|'
          ArtPris.InnkjopsPris[1] WHEN AVAILABLE ArtPris
          ArtPris.Rab1%[1]
          ArtPris.VareKost[1]
          ArtPris.DB%[1]
          ArtPris.Pris[1] WHEN AVAILABLE ArtPris
          '|'
          bufArtPris.InnkjopsPris[1] WHEN AVAILABLE bufArtPris
          bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
        WITH FRAME A DOWN WIDTH 350.
        DOWN WITH FRAME A.
      END.
    END.
    
    STATUS DEFAULT "Lese linje " + 
      STRING(iAntLinjer) + 
      " av " + 
      STRING(iTotAntLinjer) + 
      ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure
PROCEDURE TellOppLinjer:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  iTotAntLinjer = 0.
  
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.
    iTotAntLinjer = iTotAntLinjer + 1.
  END.
  INPUT STREAM Innfil CLOSE.
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

