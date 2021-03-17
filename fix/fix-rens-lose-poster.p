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

FOR EACH ArtPris WHERE
    NOT CAN-FIND(ArtBas OF ArtPris):
    DELETE ArtPris.
END.
FOR EACH Strekkode WHERE
    NOT CAN-FIND(ArtBas OF Strekkode):
    DELETE Strekkode.
END.
FOR EACH Lager WHERE
    NOT CAN-FIND(ArtBas OF Lager):
    DELETE Lager.
END.
FOR EACH ArtLAg WHERE
    NOT CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = ArtLag.ArtikkelNr):
    DELETE ArtLag.
END.
FOR EACH ArtLAg WHERE
    NOT CAN-FIND(Lager WHERE
                 Lager.ArtikkelNr = ArtLag.ArtikkelNr AND
                 LAger.Butik      = ArtLAg.Butik):
    DELETE ArtLag.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


