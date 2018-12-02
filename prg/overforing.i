&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        :  overforing.i
    Purpose     :  Definisjon av temp-tabell for oppdatering av overfringer.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF {&New} {&Shared} TEMP-TABLE tmpOverfor NO-UNDO
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD Vg         LIKE ArtBas.Vg
    FIELD LopNr      LIKE ArtBas.LopNr
    FIELD FraBut     AS INT 
    FIELD TilBut     AS INT
    FIELD FraStorl   AS CHAR
    FIELD TilStorl   AS CHAR
    FIELD Antall     AS INT
    FIELD BuntNr     AS INT
    FIELD OrdreNr    AS CHARACTER 
    FIELD Rab%       AS DECIMAL 
    FIELD Kode       AS CHARACTER 
    INDEX idxOverfor OrdreNr FraBut TilBut.

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


