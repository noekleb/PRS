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

DEF INPUT  PARAMETER cBrukerId    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cButikkListe AS CHAR NO-UNDO.

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

DEF BUFFER bButiker FOR Butiker.


FIND Bruker NO-LOCK WHERE
    Bruker.BrukerId = cBrukerId NO-ERROR.

IF Bruker.BrukerType = 2 THEN
DO:
    IF Bruker.Butik = 0 THEN
      DO:
          MESSAGE
          "Bruker (Butikk)" cBrukerId " er ikke koblet til butikk."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          QUIT.
      END.
      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = Bruker.Butik NO-ERROR.
      IF NOT AVAILABLE Butiker THEN
          DO:
              MESSAGE
              "Bruker " cBrukerId " er koblet til en ukjent butikk."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
              QUIT.
          END.
          FIND bButiker NO-LOCK WHERE
              bButiker.Butik = Butiker.clButik NO-ERROR.
          IF Butiker.clButik = 0 OR
              NOT AVAILABLE bButiker THEN
          DO:
              MESSAGE
              "Bruker " cBrukerId " er ikke koblet til et sentrallager via butikk."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
              QUIT.
          END.
          /* Lager liste over butikker i distribusjonsteam */
          FOR EACH bButiker NO-LOCK WHERE
              bButiker.clButik = Butiker.clButik
              BREAK BY bButiker.butik:

              IF NOT CAN-DO(cButikkListe,STRING(bButiker.Butik)) THEN
                  cButikkListe = cButikkListe +
                                 (IF cButikkListe = ""
                                    THEN ""
                                     ELSE ",") +
                                 STRING(bButiker.Butik).
          END.
END.
ELSE IF Bruker.Brukertype = 3 THEN
DO:
    IF NOT can-find(FIRST BrukerLEv where
                    BrukerLev.BrukerId = Bruker.BrukerId) THEN
      DO:
          MESSAGE
          "Bruker (Leverandør) " cBrukerId " er ikke koblet til leverandør."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          QUIT.
      END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


