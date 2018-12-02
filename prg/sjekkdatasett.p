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
DEF INPUT PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT PARAMETER cDatoListe  AS CHAR   NO-UNDO.

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

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId.

RUN SjekkDatoListe.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SjekkDatoListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkDatoListe Procedure 
PROCEDURE SjekkDatoListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop1      AS INT  NO-UNDO.
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEF VAR plDataSettId AS DEC  NO-UNDO.
  
  FOR EACH Kasse NO-LOCK WHERE
      Kasse.Aktiv           = TRUE AND
      Kasse.KvitteringAktiv = TRUE:


      DO piLoop1 = 1 TO NUM-ENTRIES(cDatoListe):
        ASSIGN
            pdDato = DATE(ENTRY(piLoop1,cDatoListe))
            .
        FIND FIRST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = Kasse.ButikkNr AND
          Datasett.GruppeNr = Kasse.GruppeNr AND
          Datasett.KasseNr  = Kasse.KasseNr  AND
          Datasett.Dato     = pdDato    AND
          DataSett.FilType  = Filer.FilType NO-ERROR.
        IF NOT AVAILABLE DataSett THEN
        DO:
            /* Finner neste DataSettId */
            FIND LAST DataSett NO-LOCK
                USE-INDEX DataSettId NO-ERROR.
            IF AVAILABLE DataSett THEN 
                plDataSettId = DataSett.DataSettId + 1.
            ELSE
                plDataSettId = 1.

            CREATE Datasett.
            ASSIGN
              DataSett.DataSettId = plDataSettId
              DataSett.ButikkNr   = Kasse.ButikkNr 
              DataSett.GruppeNr   = Kasse.GruppeNr
              DataSett.KasseNr    = Kasse.KasseNr
              DataSett.Dato       = pdDato
              DataSett.SettNr     = 1
              DataSett.Tid        = 0
              DataSett.FilId      = 0 /*lFilId*/
              DataSett.SettStatus = 1 /* Ventet */
              DataSett.FilType    = Filer.Filtype
              .
        END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

