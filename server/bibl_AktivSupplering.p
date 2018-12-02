&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : bibl_AktivSupplering.p
    Purpose     : Henter vareboknr på aktiv suppleringsordrebok.

    Syntax      : run bibl_AktivSupplering.p (input iMesseType, input iOpprett, output lVareBehNr).

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER iMesseType AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER iOpprett   AS LOG      NO-UNDO.
DEFINE OUTPUT PARAMETER dVareBehNr AS DECIMAL  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getAktivSupplering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAktivSupplering Procedure 
FUNCTION getAktivSupplering RETURNS DECIMAL 
    ( INPUT piMesseType AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

/* Sjekker om vektkoder 20 koder skal konverteres */

dVareBehNr = getAktivSupplering(iMesseType).
IF iOpprett AND dVareBehNr = 0 OR dVareBehNr = ? THEN 
  RUN bibl_OpprettMesseSuppOrd.p (iMesseType, OUTPUT dVareBehNr). 

RETURN ''.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getAktivSupplering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAktivSupplering Procedure 
FUNCTION getAktivSupplering RETURNS DECIMAL 
        (INPUT piMesseType AS INTEGER ):

/*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes: piMesseType = 1 Forhåndsordre
               piMesseType = 2 Suppleringsordre                                                                       
------------------------------------------------------------------------------*/    

  DEFINE VARIABLE pdVarebehnr LIKE Varebehhode.VareBehNr    NO-UNDO.
  
  GETVAREBEHNR:
  FOR EACH Messe NO-LOCK WHERE 
    Messe.MesseType = piMesseType AND
    Messe.PubliserStartDato <= TODAY AND
    Messe.PubliserStoppDato >= TODAY 
    BY Messe.PubliserStoppDato DESCENDING:
    
    FIND LAST VareBehHode WHERE 
      VarebeHhode.messenr = messe.messenr NO-LOCK NO-ERROR.
    IF AVAIL VarebehHode THEN 
    DO:
      pdVarebehnr = Varebehhode.Varebehnr.
      LEAVE GETVAREBEHNR.
    END.
  END.
  RETURN pdVarebehnr.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

