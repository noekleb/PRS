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

/* --- Sequnces --

CURRENT-VALUE
( sequence [ , logical-dbname ] ) [ = expression ]

Initial                Max/Min
Sequence Name                         Value  Increment       Value Cycle?
-------------------------------- ---------- ---------- ----------- ------
ArtikkelNr                                1          1           ? no
BatchLogg                                 0          1    99999999 yes
BestSeq                                   0          1     9999999 yes
BongNr                                    0          1    99999999 yes
JobbNr                                    1          1     9999999 yes
LeveringsNr                               0          1     9999999 yes
Next_iEntityId                            0          1           ? no
Next_iLogId                               0          1           ? no
OvBuffer                                  0          1     9999999 yes
Overforingsordre                          1          1    99999999 yes
Pakkenr                                   0          1        9999 yes
seqJBoxGenCodeId                          0          1           ? no
seqJBoxGenCodeTypeId                      0          1           ? no
seqJBoxTranslationId                      0          1           ? no
seqJBoxUserSettingId                      0          1           ? no
Telling                                   1          1    99999999 yes
VPIfil                                    1          1     9999999 yes

--------------- */

FIND LAST ArtBas USE-INDEX ArtikkelNr NO-LOCK NO-ERROR.
IF AVAILABLE ArtBas THEN
    CURRENT-VALUE(ArtikkelNr,SkoTex) = ArtBas.ArtikkelNr + 1.
ELSE
    CURRENT-VALUE(ArtikkelNr,SkoTex) = 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


