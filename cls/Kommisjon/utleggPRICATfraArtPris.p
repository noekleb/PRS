&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : utleggManuellEDI.p
    Purpose     : Utlegg av EDI PRICAT filer på grunnlag av manuelle 
                  prisendringer som er gjort.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 14/1-21
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE bOk     AS LOG      NO-UNDO.
DEFINE VARIABLE cReturn AS CHAR     NO-UNDO. 
DEFINE VARIABLE cLogg   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.

{ttElogg.i}
{cls\kommisjon\ttArtPris.i}  
DEFINE TEMP-TABLE bttArtPris LIKE ttArtPris.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

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
         HEIGHT             = 14.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

ASSIGN 
  cLogg = 'utleggManuellEDI' + REPLACE(STRING(TODAY,"99/99/9999"),'/','')
  .

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start' 
    ).

RUN kopierELogg.
RUN PrepPRICAT.
RUN EksportPRICAT.

IF CAN-FIND(FIRST ttArtPris) THEN 
  ASSIGN 
    bOk     = TRUE
    cReturn = STRING(iAnt) + ' nye prisposter klargjort for eksport.'
    .
ELSE 
  ASSIGN 
    bOk     = FALSE 
    cReturn = 'Ingen nye eller endrede artprisposter logget.'
    .

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  ' + cReturn + ' (' + CAPS(STRING(bOk)) + ').' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Slutt' 
    ).

EMPTY TEMP-TABLE ttELogg.
EMPTY TEMP-TABLE ttArtPris.
EMPTY TEMP-TABLE bttArtPris.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-EksportPRICAT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportPRICAT Procedure
PROCEDURE EksportPRICAT:
/*------------------------------------------------------------------------------
 Purpose: Opprettelse av PRICAT filer.
 
 Notes  : For artikler som ikke er på tilbud, opprettes en fil pr. artikkel.
          Filen inneholder alle artikkelens størrelser. Datoavgrensing fra 
          dagens dato og 999 dager.
          
          For artikler som står på tilbud, må det opprettes to filer.
          Først en fil med tilbudsprisen, som har datoavgrensing for tilbuds
          perioden. Deretter en fil med normalprisen, som har datoavgrensing
          fra og med dagen etter siste tilbuds dag og 999 dager.
          
          For kommisjonsbutikker, logges endringer bare på profilnr = 100.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pitype AS INTEGER NO-UNDO. /* 1=Normalpris, 2-Tilbud, 3=Normalpris etter tilbud */
  
  EMPTY TEMP-TABLE bttArtPris.
  
  IF CAN-FIND(FIRST ttArtPris) THEN 
  DO:
    FOR EACH ttArtPris
      BREAK BY ttArtPris.ArtikkelNr
            BY ttArtPris.ProfilNr:
              
      CREATE bttArtPris.
      BUFFER-COPY ttArtPris
        TO bttArtPris.
        
      IF LAST-OF(ttArtPris.ProfilNr) THEN 
      DO: 
        FIND ArtPris NO-LOCK WHERE 
          ArtPris.ArtikkelNr = ttArtPris.ArtikkelNr AND 
          ArtPris.ProfilNr   = ttArtPris.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN 
        DO:
          /* Ved normalpris endring, sendes bare normalpris. */
          IF ArtPris.tilbud = FALSE THEN
            DO: 
              piType = 1. /* Normalpris */        
              RUN cls\kommisjon\artprisPRICAT.p (cLogg, piType, TABLE bttArtPris).
            END.
          /* Er det tilbud aktivt på profilen, sendes først ny tilbudspris, deretter normalpris som skal gjelde etter tilbudet. */
          ELSE DO:
              piType = 2. /* Tilbudspris */        
              RUN cls\kommisjon\artprisPRICAT.p (cLogg, piType, TABLE bttArtPris).
              piType = 3. /* Normalpris etter tilbud. */        
              RUN cls\kommisjon\artprisPRICAT.p (cLogg, piType, TABLE bttArtPris).
          END.
          EMPTY TEMP-TABLE bttArtPris.
        END.
      END.
    END.
  END.
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-kopierELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopierELogg Procedure 
PROCEDURE kopierELogg :
  /*------------------------------------------------------------------------------
       Purpose:
       Notes:
      ------------------------------------------------------------------------------*/
  DEF VAR lNow  AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.
  DEF VAR lDiff AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.
    
  DEFINE BUFFER bElogg FOR Elogg.
    
  IF SEARCH('tnc.txt') <> ? THEN
    lDiff = 1. /* For test */
  ELSE 
    lDiff = 1.
  ASSIGN 
    lNow = dec(
                    STRING(YEAR(TODAY),"9999") +
                    string(MONTH(TODAY),"99") + 
                    string(DAY(TODAY),"99") +
                    string(TIME,"99999")
                   ).

  LOOPEN:
  FOR EACH ELogg NO-LOCK WHERE 
    ELogg.TabellNavn     = "ArtPris" AND
    ELogg.EksterntSystem = "PRICAT_KOMMISJON":

    FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL bElogg THEN
      NEXT.

    /* Bare modne poster skal behandles. */
    IF (lNow - ELogg.Opprettet) < lDiff THEN
      NEXT.

    BUFFER-COPY ELogg TO ttELogg NO-ERROR.
    DELETE bELogg.
    IF AVAILABLE ttELogg THEN
      RELEASE ttELogg.
      
  END. /* LOOPEN */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrepPRICAT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrepPRICAT Procedure
PROCEDURE PrepPRICAT:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  PRICAT_KOMMISJON:
  FOR EACH ttELogg:

    IF NUM-ENTRIES(ttELogg.Verdier,'|') < 2 THEN
      NEXT.

    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = DEC(ENTRY(1,ttELogg.Verdier,'|')) AND  
      ArtPris.ProfilNr   = INT(ENTRY(2,ttELogg.Verdier,'|')) NO-ERROR.
    IF AVAILABLE ArtPris THEN
    LOGGES: 
    DO:
      IF NOT CAN-FIND(FIRST ttArtPris WHERE 
        ttArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
        ttArtPris.ProfilNr   = ArtPris.ProfilNr) THEN 
      DO: 
        iAnt = iAnt + 1.
        CREATE ttArtPris.
        BUFFER-COPY ArtPris
          TO ttArtPris
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO: 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '  Feil: Artpris med artikkelnr/profilnr ' + STRING(ArtPris.ArtikkelNr) + '/' + STRING(ArtPris.ProfilNr) + '.'  
              ).
          DELETE ttArtPris.
        END.
        ELSE 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '  Logget artpris med artikkelnr/profilnr ' + STRING(ArtPris.ArtikkelNr) + '/' + STRING(ArtPris.ProfilNr) + '.'  
              ).
      END.          
    END. /* LOGGES*/
  END. /* PRICAT_KOMMISJON */   

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

