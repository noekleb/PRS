&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
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
/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cFilNavn  AS CHAR INIT "c:\appdir\skotex\kom\IN\J011019.042" NO-UNDO.      
&ELSE
    DEFINE INPUT PARAMETER cFilNavn  AS CHAR NO-UNDO.      
&ENDIF

/* Local Variable Definitions ---                                       */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    {ttUtskrKopi.i &NEW="NEW"}
&ELSE
    {ttUtskrKopi.i}
&ENDIF
DEFINE VARIABLE cRetur_Verdi AS CHARACTER INIT "AVBRYT" NO-UNDO.

/* 
DEFINE {&NEW} SHARED TEMP-TABLE ttUtskrKopi
  FIELD ButikkNr  AS INTE
  FIELD GruppeNr  AS INTE INITIAL 1
  FIELD KasseNr   AS INTE
  FIELD Dato      AS DATE
  FIELD BongNr    AS INTE
  FIELD Filnavn   AS CHAR
  FIELD Tekst     AS CHAR
  FIELD MedlemsNr AS CHAR
  FIELD TekstIdx  AS CHAR
  INDEX UtskrKopi IS PRIMARY 
         Filnavn  ASCENDING
         ButikkNr ASCENDING
         GruppeNr ASCENDING
         KasseNr  ASCENDING
         Dato     ASCENDING
         BongNr   ASCENDING.
 */

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

RUN StartInnLes.
RETURN cRetur_Verdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BytUt13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytUt13 Procedure 
PROCEDURE BytUt13 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cKonvFil  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cPlusNavn AS CHARACTER  NO-UNDO.
   
   DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
   INPUT FROM VALUE(cKonvFil) UNBUFFERED.
   OUTPUT TO VALUE(cKonvFil + cPlusNavn) NO-ECHO.
   FILE-INFO:FILENAME = cKonvFil.
   REPEAT WHILE iCount < FILE-INFO:FILE-SIZE:
       READKEY.
       IF LASTKEY = 13 THEN
           PUT UNFORMATTED CHR(10).
       ELSE
           PUT UNFORMATTED CHR(LASTKEY).
       iCount = iCount + 1.
   END.
   INPUT CLOSE.
   OUTPUT CLOSE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cLesFil  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInrad        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOldInrad     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lKvittoStart  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE lWaitForFF    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cTmpStr       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iButik        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iKassa        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dKvittoDato   AS DATE      NO-UNDO.
  DEFINE VARIABLE iKvittoNr     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cTekst        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMedlem       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFirstFsg     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cWordTekst    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lMomsFunnet   AS LOGICAL    NO-UNDO.
  INPUT FROM VALUE(cLesFil) NO-ECHO.
  REPEAT:
      IMPORT UNFORMATTED cInrad.
      IF NOT cInrad BEGINS FILL(" ",16) THEN /* 16 blanka = betalrad */
          ASSIGN cInrad = TRIM(REPLACE(cInrad,"@","à")).
      IF cInrad BEGINS "***" OR
         cInrad BEGINS "SKAPAD" THEN 
            NEXT.
      ELSE IF cInrad = "" AND NOT lFirstFsg THEN
          NEXT.
      ELSE IF cInrad = "" AND (COldInrad = "" OR (lFirstFsg = TRUE AND NOT lMomsFunnet)) THEN
          NEXT.
      ASSIGN cOldInrad = cInrad.
      IF lKvittoStart THEN DO:
          IF NOT lWaitForFF AND cInrad BEGINS "FF" THEN DO:
              ASSIGN lWaitForFF = TRUE
                     cTekst = cTekst + REPLACE(cInrad,"FF","") + CHR(10).
          END.
          ELSE IF lWaitForFF AND cInrad BEGINS "FF" THEN DO:
              /* här avslutar vi kvittot */
              ASSIGN lWaitForFF   = FALSE
                     lKvittoStart = FALSE
                     cTekst = cTekst + REPLACE(cInrad,"FF","").
              CREATE ttUtskrKopi.
              ASSIGN ttUtskrKopi.ButikkNr  = iButik
/*                      ttUtskrKopi.GruppeNr */
                     ttUtskrKopi.KasseNr   = iKassa
                     ttUtskrKopi.Dato      = dKvittoDato
                     ttUtskrKopi.BongNr    = iKvittonr
                     ttUtskrKopi.Filnavn   = cFilnavn
                     ttUtskrKopi.Tekst     = cTekst
                     ttUtskrKopi.MedlemsNr = cMedlem
                     ttUtskrKopi.TekstIdx  = REPLACE(cWordTekst,","," ")
                  .
              ASSIGN cOldInrad    = ""
                     iButik       = ?
                     iKassa       = ?
                     dKvittoDato  = ?
                     iKvittoNr    = ?
                     cTekst       = ""
                     cMedlem      = ""
                     lFirstFsg    = FALSE
                     cWordTekst   = ""
                     lMomsFunnet  = FALSE.
          END.
          ELSE IF lWaitForFF THEN DO:
              IF NUM-ENTRIES(cInrad,":") = 2 AND ENTRY(1,cInrad,":") = "Köp registrerat på medlem nr" THEN DO:
                  ASSIGN cMedlem = TRIM(ENTRY(2,cInrad,":"))
                         cTekst = cTekst + cInrad + CHR(10)
                         lMomsFunnet = TRUE.
              END.
              ELSE IF NUM-ENTRIES(cInrad,"à") = 2 THEN
                  ASSIGN cTekst = cTekst + FILL(" ",5 - LENGTH(ENTRY(1,cInrad," "))) + cInrad + CHR(10)
                         lFirstFsg    = TRUE.
              ELSE IF NUM-ENTRIES(SUBSTR(TRIM(cInrad),1,8),"-") = 3 THEN DO:
                  ASSIGN cTmpStr = SUBSTR(TRIM(cInrad),1,8)
                         dKvittoDato = DATE(INT(ENTRY(2,cTmpStr,"-")),INT(ENTRY(3,cTmpStr,"-")),2000 + INT(ENTRY(1,cTmpStr,"-")))
                         cTmpStr = ENTRY(1,cInrad,"/")
                         iButik  = INT(ENTRY(NUM-ENTRIES(cTmpStr," "),cTmpStr," "))
                         cTmpStr = ENTRY(2,cInrad,"/")
                         iKassa  = INT(ENTRY(1,cTmpStr," "))
                         cTekst  = cTekst + cInrad + CHR(10)
                         lMomsFunnet = TRUE.
              END.
              ELSE IF cInrad BEGINS "Exp" THEN
                  ASSIGN cTekst  = cTekst + cInrad + CHR(10).
              ELSE
                  ASSIGN cTekst  = cTekst + cInrad + CHR(10).
              IF NOT lFirstFsg AND NOT lMomsFunnet AND NOT cInrad = "" AND NUM-ENTRIES(ENTRY(1,TRIM(cInrad)," "),"/") = 1 
                               AND NOT CAN-DO(cWordTekst,ENTRY(1,TRIM(cInrad)," ")) THEN
                  ASSIGN cWordTekst = cWordTekst + (IF cWordTekst = "" THEN "" ELSE ",") + ENTRY(1,TRIM(cInrad)," ").
          END.
          IF cInrad BEGINS FILL(" ",16) THEN DO:
              IF NOT CAN-DO("SUBTOTAL,TILLBAKA,MOMS",ENTRY(1,TRIM(cInrad)," ")) AND 
                 NOT CAN-DO(cWordTekst,ENTRY(1,TRIM(cInrad)," ")) THEN
                  ASSIGN cWordTekst = cWordTekst + (IF cWordTekst = "" THEN "" ELSE ",") + ENTRY(1,TRIM(cInrad)," ").
              IF TRIM(cInrad) begins "MOMS" THEN
                  ASSIGN lMomsFunnet = TRUE.
          END.
      END.
      ELSE IF NOT lKvittoStart THEN DO:
          IF NUM-ENTRIES(cInrad,":") = 2 AND ENTRY(1,cInrad,":") = "KVITTO" THEN DO:
              ASSIGN iKvittonr = INT(TRIM(ENTRY(2,cInrad,":")))
                     lKvittoStart = TRUE
                     cTekst = cInrad + CHR(10).
         END.
      END.
  END.
  INPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartInnLes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartInnLes Procedure 
PROCEDURE StartInnLes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cPlusNavn AS CHARACTER INIT "UTS" NO-UNDO.
  RUN BytUt13 (cFilnavn,cPlusNavn). 
  RUN LesInnFil(cFilnavn + cPlusNavn).
  OS-DELETE VALUE(cFilnavn + cPlusNavn).
  ASSIGN cRetur_Verdi = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

