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
DEF INPUT PARAMETER  httBuffer         AS HANDLE NO-UNDO. /* buffer for evt. temp-table. */
DEF INPUT  PARAMETER icSessionId       AS CHAR   NO-UNDO.
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
    RUN lagrekndgrpkobling.
ELSE 
    RUN LimInnKndGrpKobling.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-lagrekndgrpkobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagrekndgrpkobling Procedure 
PROCEDURE lagrekndgrpkobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piId       AS INT  NO-UNDO.
DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.

IF NUM-ENTRIES(cParameter,"|") <> 2 THEN
DO:
  ASSIGN
      lOk     = FALSE
      cReturn = "Feil antall entries i variabel send til lagrekndgrpkobling.p."
      .
  RETURN.
END.

ASSIGN piId       = int(ENTRY(1,cParameter,"|"))
       cRowIdList = IF NUM-ENTRIES(cParameter,"|") > 1 THEN ENTRY(2,cParameter,"|") ELSE ""
       .

KOBLING:
DO:
  DO piLoop = 1 TO NUM-ENTRIES(cRowIdList) TRANSACTION:
    FIND KundeGruppe NO-LOCK WHERE
         ROWID(KundeGruppe) = TO-ROWID(ENTRY(piLoop,cRowIdList)) NO-ERROR.
    IF AVAILABLE KundeGruppe THEN DO:
      FIND FIRST VgKundeGrpRabatt 
           WHERE VgKundeGrpRabatt.Vg       = piId
             AND VgKundeGrpRabatt.GruppeId = KundeGruppe.GruppeId
           NO-LOCK NO-ERROR.
      IF NOT AVAIL VgKundeGrpRabatt THEN DO:
        CREATE VgKundeGrpRabatt.
        ASSIGN VgKundeGrpRabatt.Vg       = piId
               VgKundeGrpRabatt.GruppeId = KundeGruppe.GruppeId
               .
      END.
    END.
  END.
  FOR EACH VgKundeGrpRabatt EXCLUSIVE-LOCK 
      WHERE VgKundeGrpRabatt.Vg = piId,
      FIRST KundeGruppe OF VgKundeGrpRabatt NO-LOCK
      WHERE NOT CAN-DO(cRowIdList,STRING(ROWID(KundeGruppe))):
    DELETE VgKundeGrpRabatt.
  END.
  lOk = TRUE.
END. /* KOBLING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LimInnKndGrpKobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LimInnKndGrpKobling Procedure 
PROCEDURE LimInnKndGrpKobling :
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
    FOR EACH VgKundeGrpRabatt EXCLUSIVE-LOCK WHERE
        VgKundeGrpRabatt.Vg = INT(ENTRY(ix,cParameter,"|")):
        DELETE VgKundeGrpRabatt.
    END.
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      FIND KundeGruppe NO-LOCK WHERE
           KundeGruppe.GruppeId = INT(httBuffer:BUFFER-FIELD("GruppeId"):BUFFER-VALUE) NO-ERROR.
      IF AVAILABLE KundeGruppe THEN
      DO:
        CREATE VgKundeGrpRabatt.
        ASSIGN
            VgKundeGrpRabatt.Vg       = INT(ENTRY(ix,cParameter,"|"))
            VgKundeGrpRabatt.GruppeId = KundeGruppe.GruppeId
            VgKundeGrpRabatt.Rabatt%  = httBuffer:BUFFER-FIELD("Rabatt%"):BUFFER-VALUE
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

