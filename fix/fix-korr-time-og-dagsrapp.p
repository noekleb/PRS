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
DEF INPUT PARAMETER d1Dato AS DATE NO-UNDO.
DEF INPUT PARAMETER d2Dato AS DATE NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.

DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cBehKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntBonger   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR lFilId          AS CHAR NO-UNDO.
DEF VAR iStart          AS INT  NO-UNDO.

DEF VAR iCl              as INT  NO-UNDO.
DEF VAR lVVareKost       as DEC  NO-UNDO.
DEF VAR idags_moms       AS INT  NO-UNDO.
DEF VAR cTekst           AS CHAR NO-UNDO.
DEF VAR iZNr             AS INT  NO-UNDO.
DEF VAR iBatchNr         AS INT  NO-UNDO.
DEF VAR plDbFaktorBrutto AS DEC  NO-UNDO.
DEF VAR plDbFaktorNetto  AS DEC  NO-UNDO.
DEF VAR plMva%           AS DEC  NO-UNDO.
DEF VAR bMotpostert      AS LOG  NO-UNDO.
DEF VAR h_PrisKo         AS HANDLE NO-UNDO.
DEF VAR dVVareKost       AS DEC  NO-UNDO.
DEF VAR bPlukkliste      AS LOG  NO-UNDO.
DEF VAR bLoggPrisAvvik   AS LOG  NO-UNDO.
DEF VAR iTilgodeGyldig   AS INT  NO-UNDO.

DEF VAR wEDB-System      AS CHARACTER  NO-UNDO.
DEF VAR wTabell          AS CHARACTER  NO-UNDO.

DEF BUFFER bKundeTrans    FOR KundeTrans.                
DEF BUFFER bKundeBetTrans FOR KundeBetTrans.                
DEF BUFFER bKundeSaldo    FOR KundeSaldo.

DEF TEMP-TABLE ttKundeBut 
    FIELD KundeNr  LIKE Kunde.KundeNr
    FIELD ButikkNr AS   INT.

DEF TEMP-TABLE ttMedlemBut 
    FIELD MedlemsNr LIKE Medlem.MedlemsNr
    FIELD ButikkNr  AS   INT.

DEF TEMP-TABLE ttBongLinje 
    FIELD TTId     AS INT
    FIELD LinjeSum AS DEC.

DEF TEMP-TABLE tt_Lager
  FIELD ArtikkelNr AS DEC
  FIELD ButikkNr   AS INT
  FIELD Storl      AS CHAR
  FIELD Antall     AS DEC.

DEF BUFFER bBokforingsbilag FOR Bokforingsbilag.

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

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK where
  clButiker.Butik = iCl NO-ERROR.

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.

/* Avgjør om dagsrapporten skal posteres inklusive eller eksklusive mva */
idags_moms = 0.
{syspara.i 6 4 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    idags_moms = 1. /* Poster inkl. Mva */
ELSE
    iDags_Moms = 0. /* Poster eks.  Mva  */

/* Faktor for beregning av DB på grunnlag av oms inkl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorBrutto = 0.
{syspara.i 6 4 3 plDbFaktorBrutto DEC}
IF plDbFaktorBrutto = 0 THEN
    plDbFaktorBrutto = 0.24.

/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plDbFaktorNetto = 0.
{syspara.i 6 4 4 plDbFaktorNetto DEC}
IF plDbFaktorNetto = 0 THEN
    plDbFaktorNetto = 0.30.

/* Faktor for beregning av DB på grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes på artikkelen. */
plMva% = 0.
{syspara.i 6 4 2 plMva% DEC}
IF plMva% = 0 THEN
    plMva% = 0.30.

RUN NullstillForst.
RUN OverforDatasett.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Aktivitetsrapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktivitetsrapport Procedure 
PROCEDURE Aktivitetsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Aktivitetsrapport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Hovedgrupperapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hovedgrupperapport Procedure 
PROCEDURE Hovedgrupperapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{Hovedgrupperapport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NullstillForst) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillForst Procedure 
PROCEDURE NullstillForst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  PUBLISH 'infoDisp' ("Korr dag/akt - Nullstiller").

  FOR EACH Akt_Rapp EXCLUSIVE-LOCK WHERE
      Akt_rapp.Dato >= d1Dato AND
      Akt_Rapp.Dato <= d2Dato:
      DELETE Akt_Rapp.
  END.

  FOR EACH dags_rap EXCLUSIVE-LOCK WHERE
      Dags_Rap.Butik > 0 AND
      Dags_Rap.Dato  >= d1Dato AND
      Dags_Rap.Dato   <= d2Dato:
      DELETE Dags_Rap.
  END.

  PUBLISH 'infoDisp' ("Korr dag/akt - Ferdig: Nullstiller").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OverforDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforDatasett Procedure 
PROCEDURE OverforDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR piLinjeNr      AS INT   NO-UNDO.
  DEF VAR piBongNr       AS INT   NO-UNDO.
  DEF VAR piOldBongNr    AS INT   NO-UNDO.
  DEF VAR pbDobbel       AS LOG   NO-UNDO.
  DEF VAR piBongLinje    AS INT   NO-UNDO.
  DEF VAR piLoop1        AS INT   NO-UNDO.
  DEF VAR prBongRowId    AS ROWID NO-UNDO.
  DEF VAR pcError        AS CHAR  NO-UNDO.
  DEF VAR pcFilError     AS CHAR  NO-UNDO.
  DEF VAR plLinjeSum     AS DEC   NO-UNDO.
  DEF VAR piBokforingsNr AS INT   NO-UNDO.
  DEF VAR iAntBonger     AS INT   NO-UNDO.

  ASSIGN
      iAntBonger  = 0
      iStart      = TIME
      pbDobbel    = FALSE
      prBongRowId = ?
      .

  /* Leser alle bongene på datasettet som ikke er overført fra før. */
  BONGHODER-TRANSACTION:
  FOR EACH BongHode NO-LOCK WHERE
      BongHode.ButikkNr > 0 AND
      BongHode.GruppeNr > 0 AND
      BongHode.KasseNr  > 0 AND
      BongHode.Dato     >= d1Dato AND
      BongHode.Dato     <= d2Dato:

      FIND DataSett OF BongHode NO-LOCK NO-ERROR.

      /* Flagger den som under overføring. */
      ASSIGN
          iAntBonger          = iAntBonger + 1
          .

      /* Formidler til bruker hva som skjer. */
      IF iAntBonger MODULO 10 = 0 THEN
      DO:
        PUBLISH 'infoDisp' ("Korr dag/akt rapp Bonger: " + 
                                      STRING(iAntBonger)).
      END.

      /* Oppdaterer hovedgrupperapporten */
      RUN Hovedgrupperapport.

      /* Oppdaterer Aktivitetersrapport. */
      RUN Aktivitetsrapport.

  END. /* BONGHODER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

