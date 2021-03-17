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

DEFINE INPUT  PARAMETER cQRY        AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER cTabell     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelnr AS CHARACTER  NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.

DEF VAR hTTArt  AS HANDLE NO-UNDO.

{tmp2artbasdef.i}

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

RUN SkapaTTArt.
RUN KopierDataToTTArt.

/* RUN testtillfil. */
RUN DynBildeGrid.p (hTTArt,"FOR EACH TTArt","Artikkelnr"). 

/* 
 DYNAMIC-FUNCTION('closeQuery':U).
 bufTTh:BUFFER-RELEASE().
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KopierDataToTTArt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierDataToTTArt Procedure 
PROCEDURE KopierDataToTTArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hTTArtBuf    AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBufferField AS HANDLE  NO-UNDO.
  DEFINE VARIABLE lFunnet      AS LOGICAL NO-UNDO.

  hTTArtBuf = hTTArt:DEFAULT-BUFFER-HANDLE.
  hTTArtBuf:EMPTY-TEMP-TABLE().

  CREATE QUERY  hQuery.
  CREATE BUFFER hBuffer FOR TABLE cTabell.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(cQRY).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      lFunnet = hTTArtBuf:FIND-FIRST("where artikkelnr = " + 
                                     STRING(hBuffer:BUFFER-FIELD(cArtikkelnr):BUFFER-VALUE())) NO-ERROR.
      IF NOT lFunnet THEN DO:
          hTTArtBuf:BUFFER-CREATE().
          hTTArtBuf:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE() = hBuffer:BUFFER-FIELD(cArtikkelnr):BUFFER-VALUE().
      END.

      hQuery:GET-NEXT().
  END.
  hTTArtBuf:BUFFER-RELEASE().
  hQuery:QUERY-CLOSE().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTTArt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTArt Procedure 
PROCEDURE SkapaTTArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE TEMP-TABLE hTTArt.
    hTTArt:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
    hTTArt:ADD-NEW-INDEX("Artikkel",YES).
    hTTArt:ADD-INDEX-FIELD("Artikkel","ArtikkelNr").
    hTTArt:TEMP-TABLE-PREPARE("TTArt").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-testtillfil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testtillfil Procedure 
PROCEDURE testtillfil :
DEFINE VARIABLE hTTArtBuf AS HANDLE  NO-UNDO.
  DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.

  hTTArtBuf = hTTArt:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY  hQuery.
  hQuery:SET-BUFFERS(hTTArtBuf).
  hQuery:QUERY-PREPARE("FOR EACH TTArt").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  OUTPUT TO "CLIPBOARD".
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      hBufferField = hTTArtBuf:BUFFER-FIELD("ArtikkelNr").
      PUT UNFORMATTED  hBufferField:BUFFER-VALUE() SKIP.
      hQuery:GET-NEXT().
  END.
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

