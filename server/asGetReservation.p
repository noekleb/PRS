&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE INPUT  PARAMETER iButikknr    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cTyp         AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lcHode       AS LONGCHAR    NO-UNDO.

DEFINE TEMP-TABLE ttKOrderList NO-UNDO
    FIELD KOrdre_Id   AS DECI DECIMALS 0
    FIELD EkstOrdreNr AS CHAR
    FIELD Namn        AS CHAR
    FIELD Streckkoder AS CHAR
    INDEX KI IS PRIMARY UNIQUE KOrdre_Id.


/* DEFINE TEMP-TABLE ttKorderLinje NO-UNDO */
/*     FIELD KOrdre_Id AS DECI             */
/*     FIELD Kode AS CHAR                  */
/*     INDEX KI IS PRIMARY KOrdre_Id.      */

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

CASE cTyp:
    WHEN "LIST" THEN DO:
        RUN GetList.
        IF CAN-FIND(FIRST ttKOrderList) THEN DO:
            TEMP-TABLE ttKOrderList:WRITE-JSON("longchar", lcHode, FALSE).
            EMPTY TEMP-TABLE ttKOrderList.
        END.
    END.
    WHEN "GET" THEN DO:
        RUN GetSpecific.
    END.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetList Procedure 
PROCEDURE GetList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEFINE INPUT  PARAMETER iButikknr    AS INTEGER     NO-UNDO. */
/* DEFINE OUTPUT PARAMETER lcHode       AS LONGCHAR    NO-UNDO. */
DEFINE VARIABLE cStreckkoder AS CHARACTER   NO-UNDO.
FOR EACH KOrdreHode WHERE KOrdreHode.DatoTidEndret > DATETIME(TODAY - 8) NO-LOCK.
    IF NOT KOrdreHode.butik = iButikknr THEN
        NEXT.
    IF NOT KOrdreHode.Levstatus = "45" THEN
        NEXT.
    CREATE ttKOrderList.
    ASSIGN ttKOrderList.KOrdre_Id    = KOrdreHode.KOrdre_Id  
           ttKOrderList.EkstOrdreNr  = KOrdreHode.EkstOrdreNr
           ttKOrderList.Namn         = KOrdreHode.Navn.
    cStreckkoder = "".
    FOR EACH KOrdreLinje OF KOrdreHode WHERE KOrdreLinje.plockstatus = 2 NO-LOCK.
        cStreckkoder = cStreckkoder + (IF cStreckkoder <> "" THEN "," ELSE "") + KOrdreLinje.Kode.
        ttKOrderList.Streckkoder = cStreckkoder.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

