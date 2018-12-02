&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER cLanButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cPRSFtpButiker AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cKKampFiler   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarGr   AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "mix."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE iCl             AS INTEGER NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE TT_KKamp
    FIELD KampId   AS DEC       FORMAT ">>>>>>>>>>>>9"
    FIELD ButNr    AS INTEGER   FORMAT ">>>>>9"           
    FIELD MixNr    AS INTEGER   FORMAT ">>>>>>>>9"        
    FIELD MixType  AS INTEGER   FORMAT ">9"               
    FIELD Medlem   AS LOGICAL                             
    FIELD FraDato  AS DATE      FORMAT "99/99-99"         
    FIELD FraTid   AS CHARACTER FORMAT "MM:SS" 
    FIELD TilDato  AS DATE      FORMAT "99/99-99"         
    FIELD TilTid   AS CHARACTER FORMAT "MM:SS"             
    FIELD Aksjon   AS INTEGER   FORMAT "9"
    FIELD Antall   AS DECIMAL DECIMALS 3 FORMAT ">>>9.999" 
    FIELD Utpris   AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD Subtotal AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD EAN      AS DECIMAL DECIMALS 0 FORMAT ">>>>>>>>>>>>9" 
    FIELD RadAnt   AS DECIMAL DECIMALS 3 FORMAT ">>>9.99" 
    FIELD RadPris  AS DECIMAL DECIMALS 2 FORMAT ">>>>9.99" 
    FIELD GrpNr    AS INTEGER FORMAT ">9"
    FIELD ModellNr AS INTEGER FORMAT ">>>>>>>9" 
    INDEX MixKampanje KampId MixType MixNr.

DEF BUFFER clButiker FOR Butiker.

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
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
   END-TABLES.
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
/* eksport katalog til kasse. */
{syspara.i 1 1 56 cTekst}
IF cTekst <> '' THEN 
  cEksportKatalog = RIGHT-TRIM(cTekst,'\') + '\'.
ASSIGN
  cExportFil = cEksportKatalog + cExportFil
  . 

/* Setter datoformat for eksport til kassen. */
{syspara.i 1 1 55 iKasseEksportFormat INT}

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  DO:
    MESSAGE "Kan ikke finne sentrallager " + string(icl) + '. Kombinasjonskampanje er ikke eksportert.'
    VIEW-AS ALERT-BOX.
    RETURN.
  END.

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.

MIXMATCH:
FOR EACH TT_ELogg WHERE 
  TT_ELogg.TabellNavn     = "KampanjeMixMatch" AND
  TT_ELogg.EksterntSystem = "POS"   AND
  TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier:
  
  FIND KampanjeMixMatch NO-LOCK WHERE
    KampanjeMixMatch.KampId = INTEGER(TT_ELogg.Verdier) NO-ERROR.
  IF NOT AVAILABLE KampanjeMixMatch THEN 
    NEXT MIXMATCH.

  RUN FixKKampEndringer (KampanjeMixMatch.KampId).

  /* Här skall vi loopa runt alla kassor mm */
  IF cLanButiker <> "" THEN DO:
    DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
        /* Sjekker om den aktuelle butikken skal ha kampanjen. */
        IF NOT CAN-FIND(FIRST Kampanjebutikker WHERE
                        KampanjeButikker.KampId = KampanjeMixMatch.KampId AND
                        KampanjeButikker.Butik  = INT(ENTRY(iCount,cLanButiker))) THEN
            NEXT.
        /* Eksport */
        RUN ExportKKAmp IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt",KampanjeMixMatch.KampId). /* parameter = den loopade butiken */
    END.
  END.
  IF cFtpButiker <> "" THEN DO:
    DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
        /* Sjekker om den aktuelle butikken skal ha kampanjen. */
        IF NOT CAN-FIND(FIRST Kampanjebutikker WHERE
                        KampanjeButikker.KampId = KampanjeMixMatch.KampId AND
                        KampanjeButikker.Butik  = INT(ENTRY(iCount,cFtpButiker))) THEN
            NEXT.
        /* Eksport */
        RUN ExportKKamp IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker),KampanjeMixMatch.KampId). /* parameter = den loopade butiken */
    END.
  END.

  RUN SlettTT_ELoggKKamp. /* */

END. /* MIXMATCH */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-create_ttKKamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create_ttKKamp Procedure 
PROCEDURE create_ttKKamp :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:
                        
        Eksempel type 1: Krav til antall satt i hode. Pris i hode.
    729 360 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 2 36 0 7310070378507 0 0 0 0
    729 360 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 2 36 0 7310070381903 0 0 0 0
    729 360 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 2 36 0 7310070382108 0 0 0 0
    729 360 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 2 36 0 7310070382306 0 0 0 0
    729 360 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 2 36 0 7310070382504 0 0 0 0

    Eksempel type 2: Krav til antall på radnivå. Pris i hode.
    729 361 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 0 36 0 7310070378507 1 0 0 0
    729 361 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 0 36 0 7310070381903 1 0 0 0
    729 361 1 no 10/07/05 "00:00" 10/07/18 "23:59" 1 0 36 0 7310070382108 4 0 0 0
                                                                                                                                                                  
        ------------------------------------------------------------------------------*/
DEF INPUT PARAMETER plKampId LIKE KampanjeTilbud.KampId NO-UNDO.

  CREATE tt_KKamp.
  ASSIGN
    /*  0 */  tt_KKamp.KampId   = plKampId
    /*  1 */  tt_KKamp.ButNr    = 0
    
    /*  2 */  tt_KKamp.MixNr    = KampanjeTilbud.KampTilbId 
    /*  3 */  tt_KKamp.MixType  = (IF KampanjeTilbud.KampTilbTypeId = 10 THEN 1 
                                   ELSE IF KampanjeTilbud.KampTilbTypeId = 11 THEN 2
                                   ELSE 1)
    
    /*  4 */  tt_KKamp.Medlem   = FALSE 
    /*  5 */  tt_KKamp.FraDato  = KampanjeMixMatch.KampStartDato 
    /*  6 */  tt_KKamp.FraTid   = STRING(KampanjeMixMatch.KampStartTid,"HH:MM")
    /*  7 */  tt_KKamp.TilDato  = KampanjeMixMatch.KampSluttDato
    /*  8 */  tt_KKamp.TilTid   = STRING(KampanjeMixMatch.KampSluttTid,"HH:MM")
    
    /*  9 */  tt_KKamp.Aksjon   = 1
    /* 10 */  tt_KKamp.Antall   = KampanjeTilbud.KamptilbGrenseAntall
    /* 11 */  tt_KKamp.Utpris   = KampanjeTilbud.KampTilbBelop
    /* 12 */  tt_KKamp.Subtotal = 0.0
    
    /* 13 */  tt_KKamp.EAN      = DECIMAL(STrekkode.Kode)
    /* 14 */  tt_KKamp.RadAnt   = (IF KampanjeTilbud.KampTilbTypeId = 10 THEN 0 
                                   ELSE IF KampanjeTilbud.KampTilbTypeId = 11 THEN KampanjeTilbArtikkel.KampTilbArtMinAntall
                                   ELSE KampanjeTilbArtikkel.KampTilbArtMinAntall)
    /* 15 */  tt_KKamp.RadPris  = 0
    /* 16 */  tt_KKamp.GrpNr    = 0    
    /* 17 */  tt_KKamp.ModellNr = 0
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportKKamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportKKamp Procedure 
PROCEDURE ExportKKamp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER plKampId   LIKE KampanjeMixMatch.KampId NO-UNDO.

    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    
    DO:
      ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
             cDateFormat            = SESSION:DATE-FORMAT
             SESSION:NUMERIC-FORMAT = "American"
             SESSION:DATE-FORMAT    = "dmy".
      ASSIGN iAntVarGr = 0. /* tyvärr eftersom jag gör detta flera gånger */
      IF iKasseEksportFormat = 1 
        THEN SESSION:DATE-FORMAT    = "ymd".
      ELSE SESSION:DATE-FORMAT      = "dmy".
   
      OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
   
      FOR EACH TT_KKamp WHERE
        tt_KKamp.KampId = plKampId
        BY tt_KKamp.KampId
        BY tt_KKamp.MixType
        BY tt_KKamp.MixNr:
   
        /* Setter riktig butikknummer i filen */
        IF cFilSuffix <> "txt" THEN 
          TT_KKamp.ButNr = int(cFilSuffix).
        ELSE 
          tt_KKamp.butNr = clButiker.Butik.
      
        EXPORT 
          TT_KKamp.ButNr    
          TT_KKamp.MixNr    
          TT_KKamp.MixType  
          TT_KKamp.Medlem   
          TT_KKamp.FraDato  
          TT_KKamp.FraTid   
          TT_KKamp.TilDato  
          TT_KKamp.TilTid   
          TT_KKamp.Aksjon   
          TT_KKamp.Antall   
          TT_KKamp.Utpris   
          TT_KKamp.Subtotal 
          TT_KKamp.EAN      
          TT_KKamp.RadAnt   
          TT_KKamp.RadPris  
          TT_KKamp.GrpNr    
          TT_KKamp.ModellNr 
          .
      END.
   
      OUTPUT CLOSE.
      
      FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
      IF FILE-INFO:FILE-SIZE = 0 THEN
          OS-DELETE VALUE(cExportFil + cFilsuffix).
      ELSE IF NOT CAN-DO(cKKampFiler,cExportFil + cFilsuffix) THEN
          ASSIGN cKKampFiler = cKKampFiler + (IF cKKampFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
   
      ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
             SESSION:DATE-FORMAT    = cDateFormat.
    END.

END PROCEDURE. /* eksportKKamp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKKampEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKKampEndringer Procedure 
PROCEDURE FixKKampEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER plKampId LIKE KampanjeMixMatch.KampId NO-UNDO.

  /* Tar bare med de kombinasjonstyper som kan håndteres av InfoPOS 8.0 kassene */  
  FOR EACH KampanjeTilbud NO-LOCK WHERE
    KampanjeTilbud.KampId          = plKampId AND 
    KampanjeTilbud.KampTilbTypeId >= 10 AND 
    KampanjeTilbud.KampTilbTypeId <= 11:
    
    /* Leser linjene i kampanjetilbudet. */
    ARTIKKEL:
    FOR EACH KampanjeTilbArtikkel OF KampanjeTilbud NO-LOCK:
    
      /* Kuponger håndteres pt. ikke */
      IF KampanjeTilbArtikkel.KupongId > 0 THEN 
        NEXT ARTIKKEL.
      SKAPELSEN:
      DO: 
        /* Artikler i tilbudet. */
        IF KampanjeTilbArtikkel.KampTilbArtId > 0 THEN 
        DO:
          FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = KampanjeTilbArtikkel.KampTilbArtId NO-ERROR.
          IF AVAILABLE ArtBas THEN 
          DO:
              /* TN 23/6-11 Det er kun strekkode med HovedEAN som skallegges ut. 
              FOR EACH Strekkode OF ArtBas NO-LOCK:
                RUN  create_ttKKamp (plKampId).
              END.
              */
              FIND FIRST Strekkode OF ArtBas NO-LOCK WHERE
                Strekkode.HovedNr = TRUE NO-ERROR.
              IF NOT AVAILABLE Strekkode THEN
                  FIND FIRST Strekkode OF ArtBas NO-LOCK NO-ERROR.
              IF AVAILABLE Strekkode THEN
                  RUN  create_ttKKamp (plKampId).
          END.
        END.
        /* Produktfamilier i tilbudet. */
        ELSE DO:
          FOR EACH ProduktFamilie NO-LOCK WHERE
            ProduktFamilie.ProdFamId = KampanjeTilbArtikkel.ProdFamId,
            EACH ProduktFamMedlem OF ProduktFamilie:
            
            FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = ProduktFamMedlem.ProdFamArtikkelNr NO-ERROR.
            IF AVAILABLE ArtBas THEN 
              /* TN 23/6-11 Det er kun strekkode med HovedEAN som skallegges ut. 
              FOR EACH Strekkode OF ArtBas WHERE
                Strekkode.StrKode = ProduktFamMedlem.ProdFamStrKode:
                RUN  create_ttKKamp (plKampId).
              END.
              */
              FIND FIRST Strekkode OF ArtBas NO-LOCK WHERE
                Strekkode.HovedNr = TRUE NO-ERROR.
              IF NOT AVAILABLE Strekkode THEN
                  FIND FIRST Strekkode OF ArtBas NO-LOCK NO-ERROR.
              IF AVAILABLE Strekkode THEN
                  RUN  create_ttKKamp (plKampId).
          END.
        END.
      END. /* SKAPELSEN */
    END. /* ARTIKKEL */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "KampanjeMixMatch" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "KampanjeMixMatch" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "KampanjeMixMatch" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggKKamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggKKamp Procedure 
PROCEDURE SlettTT_ELoggKKamp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "KampanjeMixMatch" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

