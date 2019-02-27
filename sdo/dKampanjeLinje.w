&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE hArtPris AS HANDLE     NO-UNDO.

DEF VAR cLevKod AS CHAR FORMAT "x(20)" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KampanjeLinje

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  KampanjeId Vg LopNr LevKod ArtikkelNr RegistrertDato RegistrertAv EDato~
 BrukerID Pris1 Pris2 ProfilNr RegistrertTid ETid Behandlet Feilkode~
 VareKost
&Scoped-define ENABLED-FIELDS-IN-KampanjeLinje KampanjeId Vg LopNr ~
ArtikkelNr RegistrertDato RegistrertAv EDato BrukerID Pris1 Pris2 ProfilNr ~
RegistrertTid ETid Behandlet Feilkode VareKost 
&Scoped-Define DATA-FIELDS  KampanjeId Vg LopNr LevKod ArtikkelNr Beskr RegistrertDato RegistrertAv~
 EDato BrukerID Pris1 Pris2 ProfilNr RegistrertTid ETid Behandlet BildNr~
 Feilkode VareKost Endring% Konflikt Farge NormalPris LevFargKod
&Scoped-define DATA-FIELDS-IN-KampanjeLinje KampanjeId Vg LopNr ArtikkelNr ~
RegistrertDato RegistrertAv EDato BrukerID Pris1 Pris2 ProfilNr ~
RegistrertTid ETid Behandlet Feilkode VareKost 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Pris1 = KampanjeLinje.Pris[1]~
  rowObject.Pris2 = KampanjeLinje.Pris[2]
&Scoped-Define DATA-FIELD-DEFS "dKampanjeLinje.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH KampanjeLinje NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH KampanjeLinje NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main KampanjeLinje
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main KampanjeLinje


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtBasBeskr dTables  _DB-REQUIRED
FUNCTION getArtBasBeskr RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtBasBildNr dTables  _DB-REQUIRED
FUNCTION getArtBasBildNr RETURNS INTEGER
  ( INPUT ArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtikkelNr dTables  _DB-REQUIRED
FUNCTION getArtikkelNr RETURNS DECIMAL
  ( INPUT ipVg AS INTEGER, INPUT ipLopNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetArtPris dTables  _DB-REQUIRED
FUNCTION GetArtPris RETURNS CHARACTER
  ( INPUT ipArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAvvik% dTables  _DB-REQUIRED
FUNCTION getAvvik% RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFarge dTables  _DB-REQUIRED
FUNCTION getFarge RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevFargKod dTables  _DB-REQUIRED
FUNCTION getLevFargKod RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevKod dTables  _DB-REQUIRED
FUNCTION getLevKod RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrisKoKonflikt dTables  _DB-REQUIRED
FUNCTION getPrisKoKonflikt RETURNS CHARACTER
  ( /* INPUT ipProfilNr AS INTEGER, ipArtikkelNr AS DECIMAL */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegistrert dTables  _DB-REQUIRED
FUNCTION getRegistrert RETURNS LOGICAL
    ( INPUT ipKampanjeId AS INTEGER, INPUT ipVg AS INTEGER, INPUT ipLopNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetVareKost dTables  _DB-REQUIRED
FUNCTION GetVareKost RETURNS CHARACTER
  ( INPUT ipArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VarGrFinnes dTables  _DB-REQUIRED
FUNCTION VarGrFinnes RETURNS LOGICAL
  ( INPUT ipVg AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      KampanjeLinje SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 61.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "skotex.KampanjeLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.KampanjeLinje.KampanjeId
"KampanjeId" "KampanjeId" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[2]   > skotex.KampanjeLinje.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[3]   > skotex.KampanjeLinje.LopNr
"LopNr" "LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[4]   > "_<CALC>"
"getLevKod(INPUT ArtikkelNr)" "LevKod" "Lev.art.nr" "x(20)" "character" ? ? ? ? ? ? yes ? no 20 no ?
     _FldNameList[5]   > skotex.KampanjeLinje.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[6]   > "_<CALC>"
"getArtBasBeskr(INPUT ArtikkelNr)" "Beskr" ? "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[7]   > skotex.KampanjeLinje.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[8]   > skotex.KampanjeLinje.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[9]   > skotex.KampanjeLinje.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[10]   > skotex.KampanjeLinje.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[11]   > skotex.KampanjeLinje.Pris[1]
"Pris[1]" "Pris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[12]   > skotex.KampanjeLinje.Pris[2]
"Pris[2]" "Pris2" ? ">>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[13]   > skotex.KampanjeLinje.ProfilNr
"ProfilNr" "ProfilNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[14]   > skotex.KampanjeLinje.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[15]   > skotex.KampanjeLinje.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[16]   > skotex.KampanjeLinje.Behandlet
"Behandlet" "Behandlet" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[17]   > "_<CALC>"
"getArtBasBildNr(INPUT ArtikkelNr)" "BildNr" ? "zzzzz9" "Integer" ? ? ? ? ? ? no ? no 6.2 no ?
     _FldNameList[18]   > skotex.KampanjeLinje.Feilkode
"Feilkode" "Feilkode" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[19]   > skotex.KampanjeLinje.VareKost
"VareKost" "VareKost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[20]   > "_<CALC>"
"getAvvik%()" "Endring%" "Endring%" "->>9.9" "Decimal" ? ? ? ? ? ? no ? no 8.8 no ?
     _FldNameList[21]   > "_<CALC>"
"getPrisKoKonflikt()" "Konflikt" "Konflikt" "x(150)" "character" ? ? ? ? ? ? no ? no 150 no ?
     _FldNameList[22]   > "_<CALC>"
"getFarge()" "Farge" "Farge" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[23]   > "_<CALC>"
"DECI(ENTRY(1,getArtPris(RowObject.ArtikkelNr),"";""))" "NormalPris" "Normalpris" ">>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 13.4 no ?
     _FldNameList[24]   > "_<CALC>"
"getLevFargKod(INPUT ArtikkelNr)" "LevFargKod" "Lev.fargekode" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.Beskr = (getArtBasBeskr(INPUT ArtikkelNr))
         rowObject.BildNr = (getArtBasBildNr(INPUT ArtikkelNr))
         rowObject.Endring% = (getAvvik%())
         rowObject.Farge = (getFarge())
         rowObject.Konflikt = (getPrisKoKonflikt())
         rowObject.LevFargKod = (getLevFargKod(INPUT ArtikkelNr))
         rowObject.LevKod = (getLevKod(INPUT ArtikkelNr))
         rowObject.NormalPris = (DECI(ENTRY(1,getArtPris(RowObject.ArtikkelNr),";")))
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable dTables  _DB-REQUIRED
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE piKampanjeId AS INTEGER NO-UNDO.
    DEFINE VARIABLE iProfilNr    AS INTEGER NO-UNDO.
/* !! test */    DEFINE BUFFER bRowObject FOR RowObject.

    FIND KampanjeHode WHERE KampanjeHode.KampanjeId = 
        INT(ENTRY(1,DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))) NO-LOCK.
    ASSIGN iProfilNr = KampanjeHode.ProfilNr.
/*     ASSIGN iProfilNr =                                                                                                                */
/*         INT(ENTRY(INT(LOOKUP("ProfilNr",DYNAMIC-FUNCTION('getForeignFields':U)) / 2),DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))). */
    FOR EACH RowObjUpd WHERE CAN-DO("A,C",RowObjUpd.RowMod):
        IF CAN-FIND(KampanjeLinje WHERE
                    KampanjeLinje.KampanjeId = RowObjUpd.KampanjeId AND
                    KampanjeLinje.Vg         = RowObjUpd.Vg AND        
                    KampanjeLinje.LopNr      = RowObjUpd.LopNr) THEN
            RETURN "Allerede registrert.".
        FIND ArtBas WHERE ArtBas.Vg    = RowObjUpd.Vg AND
                          ArtBas.LopNr = RowObjUpd.LopNr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                               ArtPris.ProfilNr   = iProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
                RETURN "Artikkel mangler kalkyle med prisprofil " + STRING(iProfilNr).
/*             ELSE IF ArtPris.InnkjopsPris[1] = 0 AND ArtPris.Pris[1] = 0 THEN           */
/*                 RETURN "Artikkel mangler inn-/utpris for profil " + STRING(iProfilNr). */
            ELSE
                ASSIGN RowObjUpd.ArtikkelNr = ArtBas.ArtikkelNr
                       RowObjUpd.ProfilNr   = iProfilNr.
        END.
        ELSE DO:
            RUN undoTransaction.
            RUN doUndoUpdate.
            RETURN "Feil varegruppe/lopnr".
        END.
    END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowObjectValidate dTables  _DB-REQUIRED
PROCEDURE rowObjectValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE VAR cErrMessage AS CHARACTER NO-UNDO.                 */
/*     DEFINE BUFFER bRowObject FOR RowObject.                      */
/*     DEFINE VARIABLE h_Navigation AS HANDLE     NO-UNDO.          */
/*     IF CAN-DO("A,C",RowObjUpd.RowMod) AND                        */
/*        CAN-FIND(bRowObject WHERE                                 */
/*                 bRowObject.KampanjeId = RowObject.KampanjeId AND */
/*                 bRowObject.Vg = RowObject.Vg AND                 */
/*                 bRowObject.LopNr = RowObject.LopNr AND           */
/*                 ROWID(bRowObject) <> ROWID(RowObject)) THEN      */
/*        ASSIGN cErrMessage = "Allerede registrert" + CHR(10).     */
/*     IF cErrMessage NE "" THEN DO:                                */
/*         RETURN cErrMessage.                                      */
/*     END.                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArtPrisHandle dTables  _DB-REQUIRED
PROCEDURE setArtPrisHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iphArtPris AS HANDLE     NO-UNDO.
ASSIGN hArtPris = iphArtpris.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtBasBeskr dTables  _DB-REQUIRED
FUNCTION getArtBasBeskr RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtBasBildNr dTables  _DB-REQUIRED
FUNCTION getArtBasBildNr RETURNS INTEGER
  ( INPUT ArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN ArtBas.BildNr ELSE 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtikkelNr dTables  _DB-REQUIRED
FUNCTION getArtikkelNr RETURNS DECIMAL
  ( INPUT ipVg AS INTEGER, INPUT ipLopNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*     DEFINE VARIABLE iProfilNr    AS INTEGER NO-UNDO.                                                                                  */
/*     ASSIGN iProfilNr =                                                                                                                */
/*         INT(ENTRY(INT(LOOKUP("ProfilNr",DYNAMIC-FUNCTION('getForeignFields':U)) / 2),DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))). */
/*                                                                                                                                       */
  FIND ArtBas WHERE ArtBas.Vg = ipVg AND ArtBas.LopNr = ipLopNr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas AND ArtBas.BildNr > 0 THEN DO:
      FIND BildeRegister WHERE BildeRegister.BildNr = ArtBas.BildNr NO-LOCK NO-ERROR.
      IF AVAIL BildeRegister AND Bilderegister.FilNavn <> "" THEN
          PUBLISH "NyttBilde" ((IF CAN-FIND(FIRST bildedata OF BildeRegister WHERE teller >= 200) THEN "mini" ELSE "") + Bilderegister.FilNavn).
  END.
/*   IF VALID-HANDLE(hArtPris) AND AVAIL ArtBas THEN DO:                                                                                   */
/*       ASSIGN iProfilNr =                                                                                                                */
/*           INT(ENTRY(INT(LOOKUP("ProfilNr",DYNAMIC-FUNCTION('getForeignFields':U)) / 2),DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))). */
/*       DYNAMIC-FUNCTION('setQueryWhere':U IN hArtPris,                                                                                   */
/*      INPUT "ArtPris.ArtikkelNr = '" + STRING(ArtBas.ArtikkelNr) + "' AND ArtPris.ProfilNr = '"                                          */
/*                        + STRING(iProfilNr) + "'" /* CHARACTER */).                                                                      */
/*       DYNAMIC-FUNCTION('openQuery':U IN hArtPris).                                                                                      */
/*   END.                                                                                                                                  */
  RETURN IF AVAIL ArtBas THEN ArtBas.ArtikkelNr ELSE ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetArtPris dTables  _DB-REQUIRED
FUNCTION GetArtPris RETURNS CHARACTER
  ( INPUT ipArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND KampanjeHode WHERE KampanjeHode.KampanjeId = 
      INT(ENTRY(1,DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))) NO-LOCK.
  FIND ArtPris WHERE ArtPris.ArtikkelNr = ipArtikkelNr AND
                     ArtPris.ProfilNr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.

/*   FIND ArtPris WHERE ArtPris.ProfilNr =                                     */
/*             INT(ENTRY(2,DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))) AND */
/*                       ArtPris.ArtikkelNr = ipArtikkelNr NO-LOCK NO-ERROR.   */
/*   RUN KampanjVerdier IN h_vartpris    */
/* ( INPUT dKampanjePris /* DECIMAL */). */

/*   IF VALID-HANDLE(hArtPris) AND AVAIL ArtPris THEN                       */
/*       RUN FindArtPris IN hArtPris (ArtPris.ArtikkelNr,ArtPris.ProfilNr). */
/*   RETURN IF NOT AVAIL ArtPris THEN "?;" ELSE IF ArtPris.Tilbud = FALSE THEN           */
/*                   STRING(ArtPris.Pris[1]) + ";N" ELSE STRING(ArtPris.Pris[2]) + ";J". */
  RETURN IF NOT AVAIL ArtPris THEN "?;" ELSE  
                  STRING(ArtPris.Pris[1]) + ";N".
  RELEASE ArtPris.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAvvik% dTables  _DB-REQUIRED
FUNCTION getAvvik% RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dNP AS DECIMAL    NO-UNDO.

  ASSIGN dNP = DECI(ENTRY(1,getArtPris(RowObject.ArtikkelNr),";")).
  RETURN ROUND((dNP - RowObject.Pris2) / dNP * -100,1).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFarge dTables  _DB-REQUIRED
FUNCTION getFarge RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN DO:
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
      RETURN IF AVAIL Farg THEN Farg.farBeskr ELSE "".   /* Function return value. */
  END.
  ELSE
      RETURN "".

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevFargKod dTables  _DB-REQUIRED
FUNCTION getLevFargKod RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN ArtBas.LevFargKod ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevKod dTables  _DB-REQUIRED
FUNCTION getLevKod RETURNS CHARACTER
  ( INPUT ArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN ArtBas.LevKod ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrisKoKonflikt dTables  _DB-REQUIRED
FUNCTION getPrisKoKonflikt RETURNS CHARACTER
  ( /* INPUT ipProfilNr AS INTEGER, ipArtikkelNr AS DECIMAL */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE cKonflikt AS CHARACTER  NO-UNDO.

 /*
 FIND KampanjeHode OF RowObject NO-LOCK NO-ERROR.
 IF AVAIL KampanjeHode THEN DO:
     FIND FIRST PrisKo WHERE PrisKo.ArtikkelNr = RowObject.ArtikkelNr AND 
                           PrisKo.ProfilNr   = RowObject.ProfilNr NO-LOCK NO-ERROR.
     IF AVAIL PrisKo THEN DO:
       IF PrisKo.TYPE = 3 AND PrisKo.AktiveresDato >= KampanjeHode.StartDato AND 
                              PrisKo.AktiveresDato <= KampanjeHode.SluttDato THEN
           ASSIGN cKonflikt = "Kamp.avslut".
/*        FOR EACH PrisKo WHERE PrisKo.ArtikkelNr = RowObject.ArtikkelNr AND                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        */
/*                              PrisKo.ProfilNr   = RowObject.ProfilNr   AND                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        */
/*                              PrisKo.TYPE       = 2  NO-LOCK:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     */
/*            IF  THEN                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              */
/*            ASSIGN cKonflikt = "post finns".                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      */
/* /*          PrisKo.AktiveresDato PrisKo.AktiveresTid PrisKo.Aktivert PrisKo.ArtikkelNr PrisKo.BrukerID PrisKo.DB% PrisKo.DBKr PrisKo.DivKost% PrisKo.DivKostKr PrisKo.EDato PrisKo.ETid PrisKo.EuroManuel PrisKo.EuroPris PrisKo.Frakt PrisKo.Frakt% PrisKo.GyldigTilDato PrisKo.GyldigTilTid PrisKo.InnkjopsPris PrisKo.LevNr PrisKo.Mva% PrisKo.MvaKr PrisKo.Pris PrisKo.ProfilNr PrisKo.Rab1% PrisKo.Rab1Kr PrisKo.Rab2% PrisKo.Rab2Kr PrisKo.Rab3% PrisKo.Rab3Kr PrisKo.RegistrertAv PrisKo.RegistrertDato PrisKo.RegistrertTid PrisKo.Tilbud PrisKo.Timestyrt PrisKo.Type PrisKo.ValPris PrisKo.VareKost */ */
/*        END.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      */
     END.
 END.
  */
 IF AVAIL KampanjeLinje THEN
  ASSIGN cKonflikt = IF num-entries(KampanjeLinje.FeilKode,Chr(1)) > 1
                     THEN ENTRY(2,KampanjeLinje.FeilKode,CHR(1))
                     ELSE KampanjeLinje.FeilKode
         .

  RETURN cKonflikt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegistrert dTables  _DB-REQUIRED
FUNCTION getRegistrert RETURNS LOGICAL
    ( INPUT ipKampanjeId AS INTEGER, INPUT ipVg AS INTEGER, INPUT ipLopNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(KampanjeLinje 
                  WHERE KampanjeLinje.KampanjeId = ipKampanjeId AND
                        KampanjeLinje.Vg         = ipVg AND
                        KampanjeLinje.LopNr      = ipLopNr).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetVareKost dTables  _DB-REQUIRED
FUNCTION GetVareKost RETURNS CHARACTER
  ( INPUT ipArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND KampanjeHode WHERE KampanjeHode.KampanjeId = 
      INT(ENTRY(1,DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))) NO-LOCK.
  FIND ArtPris WHERE ArtPris.ArtikkelNr = ipArtikkelNr AND
                     ArtPris.ProfilNr = KampanjeHode.ProfilNr NO-LOCK NO-ERROR.

/*   FIND ArtPris WHERE ArtPris.ProfilNr =                                     */
/*             INT(ENTRY(2,DYNAMIC-FUNCTION('getForeignValues':U),CHR(1))) AND */
/*                       ArtPris.ArtikkelNr = ipArtikkelNr NO-LOCK NO-ERROR.   */
/*   RUN KampanjVerdier IN h_vartpris    */
/* ( INPUT dKampanjePris /* DECIMAL */). */

/*   IF VALID-HANDLE(hArtPris) AND AVAIL ArtPris THEN                       */
/*       RUN FindArtPris IN hArtPris (ArtPris.ArtikkelNr,ArtPris.ProfilNr). */
/*   RETURN IF NOT AVAIL ArtPris THEN "?;" ELSE IF ArtPris.Tilbud = FALSE THEN           */
/*                   STRING(ArtPris.Pris[1]) + ";N" ELSE STRING(ArtPris.Pris[2]) + ";J". */
  RETURN IF NOT AVAIL ArtPris THEN "?;" ELSE  
                  STRING(ArtPris.Varekost[1]) + ";N".
  RELEASE ArtPris.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VarGrFinnes dTables  _DB-REQUIRED
FUNCTION VarGrFinnes RETURNS LOGICAL
  ( INPUT ipVg AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN CAN-FIND(VarGr WHERE VarGr.Vg = ipVg).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

