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
DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEF VAR iTotAntLinjer AS INT NO-UNDO.
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR lLapTop       AS LOG NO-UNDO.
DEF VAR iProfilNr     AS INT NO-UNDO.

DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR rArtBasRecid  AS RECID  NO-UNDO.
DEF VAR cEndelse      AS CHAR   NO-UNDO.
DEF VAR cTekst        AS CHAR   NO-UNDO.
DEF VAR cTblLst       AS CHAR   NO-UNDO.

DEF VAR bStatus       AS LOG  NO-UNDO.

DEF VAR iAntArtikler  AS INT  NO-UNDO.
DEF VAR iAntKoblet    AS INT  NO-UNDO.
DEF VAR iAntOppdat    AS INT  NO-UNDO.

DEF VAR cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell    LIKE ImpKonv.Tabell     NO-UNDO.

DEF VAR cNumericFormat AS CHAR NO-UNDO.
DEF VAR cDateFormat    AS CHAR NO-UNDO.

DEF TEMP-TABLE tmpVPIFilLinje LIKE VPIFilLinje.
/* DEF TEMP-TABLE tmpVarGr       LIKE VarGr */
/*   FIELD EType AS INT.                    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

/* Sjekker om det kjøres på en LapTop */
if valid-handle(h_dproclib) then
  run SjekkLapTop in h_dproclib (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    MESSAGE "Ingen VPIFilHode tilgjengelig"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

/* Henter ID for konvertering */
FIND EkstVPILev NO-LOCK WHERE
    EkstVPILev.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
IF NOT AVAILABLE EkstVPILev THEN
DO:
    MESSAGE "Ingen ekstern VPI leverandør tilgjengelig." SKIP
            "Id: " + STRING(VPIFilHode.EkstVPILevNr) + "."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ASSIGN
    cEDB-System = EkstVPILev.KortNavn
    .

/* Datasett statuspost. */
FIND VPIDatasett NO-LOCK WHERE
    VPIDatasett.EkstVPILevNr = VPIFilHode.EkstVPILevNr NO-ERROR.
                                                
/* /* Er datasettet ferdig oppdatert? */                                                 */
/* IF AVAILABLE VPIDatasett THEN                                                         */
/* DO:                                                                                   */
/*     IF VPIDatasett.DatasettStatus >= 3 AND                                            */
/*        VPIDatasett.DatasettStatus <= 4 THEN                                           */
/*     DO:                                                                               */
/*         MESSAGE "Forrige datasett er ikke oppdatert." SKIP                            */
/*                 "Det må være ferdig oppdatert før ny import av salgsdata kan gjøres." */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        */
/*         RETURN.                                                                       */
/*     END.                                                                              */
/* END.                                                                                  */

ASSIGN 
  cNumericFormat         = SESSION:NUMERIC-FORMAT
  cDateFormat            = SESSION:DATE-FORMAT
  SESSION:NUMERIC-FORMAT = "European"
  SESSION:DATE-FORMAT    = "dmy"
  .

DO TRANSACTION:
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABL VPIFilHode THEN
    VPIFilHode.VPIFilStatus = 5. /* Utpakket */
END.
FIND CURRENT VPIFilHode NO-LOCK NO-ERROR.

ASSIGN 
  SESSION:NUMERIC-FORMAT = cNumericFormat
  SESSION:DATE-FORMAT    = cDateFormat
  .

ON CLOSE OF THIS-PROCEDURE 
DO:
  RUN disable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


