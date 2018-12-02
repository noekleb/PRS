&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSdo Include 
PROCEDURE SokSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFelt       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcValues     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSort       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcOperators  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcFeltListe  AS CHAR NO-UNDO.
  DEF VAR piLoop1      AS INT  NO-UNDO.
  DEF VAR pcRemoveFelt AS CHAR NO-UNDO.
  DEF VAR pcRemoveOper AS CHAR NO-UNDO.

  ASSIGN
      pcRemoveFelt = ""
      pcRemoveOper = "=,EQ,BEGINS,MATCHES,<,LE,<=,>,GE,>=,<>"
      .

  TA-BORT-KRITERIE:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFeltListe):
      /*IF NOT CAN-DO(pcFelt,ENTRY(piLoop1,pcFeltListe)) THEN*/
      DO:
          ASSIGN
              pcRemoveFelt = pcRemoveFelt + 
                             (IF pcRemoveFelt = ""
                              THEN ""
                              ELSE ",") +
                              ENTRY(piLoop1,pcFeltListe)
              .
      END.
  END. /* TA-BORT-KRITERIE */
  /* Tar bort kriterier.                                               */
  /* En gang pr. operatortype. Ikke mulig å "huske" hva som ble brukt. */
  IF pcRemoveFelt <> "" THEN
    DO piLoop1 = 1 TO NUM-ENTRIES(pcRemoveOper):
      DYNAMIC-FUNCTION('removeQuerySelection':U,
       INPUT pcRemoveFelt /* CHARACTER */,
        INPUT entry(piLoop1,pcRemoveOper) /* CHARACTER */).
    END.

  /* Legger på kriterier */
  IF pcFelt <> "" THEN
      DYNAMIC-FUNCTION('assignQuerySelection':U,
           INPUT pcFelt /* CHARACTER */,
           INPUT pcValues /* CHARACTER */,
           INPUT pcOperators /* CHARACTER */).

  /* Legger inn valgt sortering */
  IF pcSort <> "" THEN
    DYNAMIC-FUNCTION('setQuerySort':U,
       INPUT pcSort /* CHARACTER */).

  /* Åpner query med ny filterverdi. */
  DYNAMIC-FUNCTION('openQuery':U).
  /*
  MESSAGE 
    "Søkefelt:" pcFelt SKIP
    "Verdier:" pcValues SKIP
    "Operators:" pcOperators SKIP(2)
    "Slett kriterie:" pcRemoveFelt SKIP
    "               " pcRemoveOper SKIP
    "Alle felt:" pcFeltListe SKIP(2)
    "Aktiv query:" DYNAMIC-FUNCTION('getQueryWhere':U ) SKIP
    "BasisQuery:"  DYNAMIC-FUNCTION('getOpenQuery':U )
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

