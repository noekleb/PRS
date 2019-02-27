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
DEF INPUT PARAMETER  cParameter        AS CHAR   NO-UNDO. /* Input parametre til programmet.           */
DEF INPUT PARAMETER  httBuffer         AS HANDLE NO-UNDO. /* Handle til temptable buffer - hvis det benyttes. */
DEF INPUT  PARAM     icSessionId       AS CHAR   NO-UNDO.
DEF OUTPUT PARAMETER cReturn           AS CHAR   NO-UNDO. /* Retur veri til kallende program.          */
DEF OUTPUT PARAMETER lOk               AS LOG    NO-UNDO. /* Vellykket utføring.                       */

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

IF httBuffer = ? THEN
  RUN lagrekategorikobling.
ELSE 
  RUN LimInnKategoriKobling.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-lagrekategorikobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagrekategorikobling Procedure 
PROCEDURE lagrekategorikobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piVg               AS INT  NO-UNDO.
  DEF VAR cKategoriRowIdList AS CHAR NO-UNDO.
  DEF VAR piLoop             AS INT  NO-UNDO.

  IF NUM-ENTRIES(cParameter,"|") <> 2 THEN
  DO:
      ASSIGN
          lOk     = FALSE
          cReturn = "Feil antall entries i variabel send til lagrekategorikobling.p."
          .
      RETURN.
  END.

  ASSIGN
      piVg          = int(ENTRY(1,cParameter,"|"))
      cKategoriRowIdList = ENTRY(2,cParameter,"|")
      .

  KOBLING:
  DO:
      FOR EACH VgKat EXCLUSIVE-LOCK WHERE
          VgKat.Vg = piVg:
          DELETE VgKat.
      END.
      DO piLoop = 1 TO NUM-ENTRIES(cKategoriRowIdList) TRANSACTION:
          FIND Kategori NO-LOCK WHERE
              ROWID(Kategori) = TO-ROWID(ENTRY(piLoop,cKategoriRowIdList)) NO-ERROR.
          IF AVAILABLE Kategori THEN
          DO:
              CREATE VgKat.
              ASSIGN
                  VgKat.Vg    = piVg
                  VgKat.KatNr = Kategori.KatNr
                  VgKat.VgKat = Kategori.KatNr
                  .
          END.
      END.
      lOk = TRUE.
  END. /* KOBLING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LimInnKategoriKobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LimInnKategoriKobling Procedure 
PROCEDURE LimInnKategoriKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR ix     AS INT NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(httBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + httBuffer:NAME).
hQuery:QUERY-OPEN().

KOBLING:
DO:
  DO ix = 1 TO NUM-ENTRIES(cParameter,"|"):
    FOR EACH VgKat EXCLUSIVE-LOCK WHERE
        VgKat.Vg = INT(ENTRY(ix,cParameter,"|")):
        DELETE VgKat.
    END.
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      FIND Kategori NO-LOCK WHERE
           Kategori.KatNr = INT(httBuffer:BUFFER-FIELD("KatNr"):BUFFER-VALUE) NO-ERROR.
      IF AVAILABLE Kategori THEN
      DO:
        CREATE VgKat.
        ASSIGN
            VgKat.Vg       = INT(ENTRY(ix,cParameter,"|"))
            VgKat.KatNr    = Kategori.KatNr
            VgKat.VgKat    = Kategori.KatNr
            .
      END.
  
      hQuery:GET-NEXT().
    END.
  END.
  lOk = TRUE.
END. /* KOBLING */

DELETE OBJECT hQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

