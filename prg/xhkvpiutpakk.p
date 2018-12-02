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

DEF VAR dcValPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcInnPris     AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcUtpris      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcRabatt      AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.
DEF VAR dcFrakt       AS DEC  FORMAT "->>>,>>9.99" NO-UNDO.

DEF VAR bStatus       AS LOG  NO-UNDO.

DEF VAR iAntArtikler  AS INT  NO-UNDO.
DEF VAR iAntKoblet    AS INT  NO-UNDO.
DEF VAR iAntOppdat    AS INT  NO-UNDO.

DEF VAR cEDB-System   LIKE ImpKonv.EDB-System NO-UNDO.
DEF VAR cImpTabell    LIKE ImpKonv.Tabell     NO-UNDO.

DEF VAR cNumericFormat AS CHAR NO-UNDO.
DEF VAR cDateFormat    AS CHAR NO-UNDO.

DEF TEMP-TABLE tmpVPIFilLinje LIKE VPIFilLinje.
DEF TEMP-TABLE tmpVarGr       LIKE VarGr
  FIELD EType AS INT.
DEF TEMP-TABLE tmpAvdeling    LIKE Avdeling
  FIELD EType AS INT.
DEF TEMP-TABLE tmpKategori    LIKE Kategori
  FIELD EType AS INT.
DEF TEMP-TABLE tmpHuvGr       LIKE HuvGr
  FIELD EType AS INT.
DEF TEMP-TABLE tmpLevBas      LIKE LevBas
  FIELD EType AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

/* Start av procedurebibliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN prisko.p PERSISTENT SET h_PrisKo.

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
                                                
/* Er datasettet ferdig oppdatert? */
/* IF AVAILABLE VPIDatasett THEN                                                   */
/* DO:                                                                             */
/*     IF VPIDatasett.DatasettStatus >= 3 AND                                      */
/*        VPIDatasett.DatasettStatus <= 4 THEN                                     */
/*     DO:                                                                         */
/*         MESSAGE "Forrige datasett er ikke oppdatert." SKIP                      */
/*                 "Det må være ferdig oppdatert før ny import av VPI kan gjøres." */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
/*         RETURN.                                                                 */
/*     END.                                                                        */
/* END.                                                                            */

ASSIGN 
  cNumericFormat         = SESSION:NUMERIC-FORMAT
  cDateFormat            = SESSION:DATE-FORMAT
  SESSION:NUMERIC-FORMAT = "European"
  SESSION:DATE-FORMAT    = "dmy"
  .

/* Utpakking av VPI til VPI bufferet. */
/*RUN UtpakkVpi.*/

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
  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_Prisko.
  RUN disable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  def INPUT        PARAMETER pcFraFelt  as char no-undo.
  DEF INPUT-OUTPUT PARAMETER pcSkjerm   AS CHAR NO-UNDO.
  
  def var pcFeltListe as char no-undo.
  def var piFeltNr    as int  no-undo.
  DEF VAR lTilbud     AS LOG  NO-UNDO.
  DEF VAR lDirekte    AS LOG  NO-UNDO.

  DEF BUFFER bArtBas FOR VPIArtBas.
  DEF BUFFER bVarGr  FOR VarGr.
  DEF BUFFER bMoms   FOR Moms.
  DEF BUFFER bValuta FOR Valuta.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  assign
    lTilbud     = FALSE
    lDirekte    = FALSE
    pcFeltListe = "ValPris,InnPris,Rab1,Rab1%,Rab2,Rab2%,Frakt,Frakt%," + 
                  "DivKost,DivKost%,Rab3,Rab3%,VareKost,DB,DB%," +
                  "FI-Mva,FI-Mva%,Pris,EU-Pris".
                 
  /* Finner i hvilket felt markøren sto når prosedyren ble kalt. */
  assign
    /*pcFraFelt = substring(pcFraFelt,4)*/
    piFeltNr  = lookup(pcFraFelt,pcFeltListe)
    .

  /* Ukjent felt. */  
  if piFeltNr = 0 then
    do:
      message "Ukjent felt!" view-as alert-box title "Kalkylefeil".
      return no-apply.  
    end.

  /* Henter nødvendige buffere */
  FIND bArtBas NO-LOCK WHERE
      RECID(bArtBas) = rArtBasRecid NO-ERROR.
  IF NOT AVAILABLE bArtBas THEN
  DO:
    MESSAGE "Ukjent bArtBas"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bVarGr OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bVarGr THEN
  DO:
    MESSAGE "Ukjent bVarGr"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bMoms  OF bVarGr  NO-ERROR.
  IF NOT AVAILABLE bMoms THEN
  DO:
    MESSAGE "Ukjent bMoms"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.
  FIND bValuta OF bArtBas NO-ERROR.
  IF NOT AVAILABLE bValuta THEN
  DO:
    MESSAGE "Ukjent bValuta for Artikkel/leverandør:" ArtBas.ArtikkelNr ArtBas.LevNr ":" + ArtBas.ValKod + ":"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
  END.

  /* Starter omkalkulering.                         */
  run Omregning in h_PrisKo
       (input rArtBasRecid, 
        input iProfilNr,
        input-output pcSkjerm,
        input bMoms.MomsProc,
        input bValuta.ValKurs, 
        input piFeltNr,
        INPUT lTilbud).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-KalkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KalkStreng Procedure 
FUNCTION KalkStreng RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var pcTekst   as char no-undo.
  def var pdAktDato as date no-undo.
  DEF VAR plTilbud AS LOG  NO-UNDO.
  DEF VAR plManuel AS LOG  NO-UNDO.
  
  assign
    plTilbud = false
    plManuel = FALSE
    pdAktDato =  TODAY - 1
    pcTekst   =   
      /*string(input FI-ValPris) */ string(dcValPris) + ";" +
      /*string(input FI-InnPris) */ string(dcInnPris) + ";" +
      /*string(input FI-Rab1)    */ string(dcRabatt)  + ";" +
      /*string(input FI-Rab1%)   */ "0" + ";" +
      /*string(input FI-Rab2)    */ "0" + ";" +
      /*string(input FI-Rab2%)   */ "0" + ";" +
      /*string(input FI-Frakt)   */ "0" + ";" +
      /*string(input FI-Frakt%)  */ "0" + ";" +
      /*string(input FI-DivKost) */ "0" + ";" +
      /*string(input FI-DivKost%)*/ "0" + ";" +
      /*string(input FI-Rab3)    */ "0" + ";" +
      /*string(input FI-Rab3%)   */ "0" + ";" +
      /*string(input FI-VareKost)*/ "0" + ";" +
      /*string(input FI-Mva)     */ "0" + ";" +
      /*string(input FI-Mva%)    */ "0" + ";" +
      /*string(input FI-DB)      */ "0" + ";" +
      /*string(input FI-DB%)     */ "0" + ";" +
      /*string(input FI-Pris)    */ STRING(dcUtpris) + ";" +
      /*string(input FI-EUPris)  */ "0" + ";" +
      /*plManuel                 */ "no" + ";"
       .
  /* Normal aktiveringsdag/tid */                 
  ASSIGN  
    cEndelse = cEndelse +              
             (if pdAktDato <> ?
                then string(pdAktDato)
                else "") + ";" +
              "0;"
    cEndelse = cEndelse + 
             ";0;;0;no"
    pcTekst = pcTekst + cEndelse
    .
  
  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

