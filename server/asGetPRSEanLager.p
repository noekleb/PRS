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
DEFINE INPUT  PARAMETER cEan     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lcStock  AS LONGCHAR NO-UNDO.
/* DEFINE OUTPUT PARAMETER obOk     AS LOG      NO-UNDO. */
/* DEFINE OUTPUT PARAMETER ocReturn AS CHAR     NO-UNDO. */

/* DEF VAR StockDataSet AS HANDLE NO-UNDO. */
/*                                         */

/* DEFINE TEMP-TABLE tt_EAN NO-UNDO */
/*     FIELD upcid AS CHAR          */
/*     FIELD strkode AS INTE.       */
DEFINE TEMP-TABLE facilities NO-UNDO
    FIELD upcid AS CHAR SERIALIZE-HIDDEN
    FIELD availableQuantity AS INTE
    FIELD facilityType AS CHAR
    FIELD id AS CHAR  SERIALIZE-NAME "pimcoreID"
    FIELD butik AS CHAR SERIALIZE-NAME "infoposID".


/* DEFINE DATASET  StockDataSet FOR tt_EAN,facilities DATA-RELATION FOR tt_EAN,facilities RELATION-FIELDS(upcid,upcid). */
/* ASSIGN                                                                                                               */
/*     StockDataSet:SERIALIZE-HIDDEN = TRUE                                                                             */
/*     StockDataSet:SERIALIZE-NAME   = "None".                                                                          */
/*                                                                                                                      */
/* StockDataSet:ADD-BUFFER(TEMP-TABLE tt_EAN:DEFAULT-BUFFER-HANDLE).                                                    */
/* StockDataSet:ADD-BUFFER(TEMP-TABLE facilities:DEFAULT-BUFFER-HANDLE).                                                */

/* /* StockDataSet:RELATION-FIELDS(tt_EAN.upcid,facilities.upcid). */                                                                     */
/*                                                                                                                                        */
/* StockDataSet:ADD-PARENT-ID-RELATION(TEMP-TABLE tt_EAN:DEFAULT-BUFFER-HANDLE,TEMP-TABLE facilities:DEFAULT-BUFFER-HANDLE,"upcid",?,?).  */
/*                                                                                                                                        */
/* ASSIGN */
/*     StockDataSet:SERIALIZE-HIDDEN = TRUE */
/* StockDataSet:SERIALIZE-NAME   = "Stock". */
/* StockDataSet:ADD-BUFFER(TEMP-TABLE tt_EAN:DEFAULT-BUFFER-HANDLE).                        */
/* StockDataSet:ADD-BUFFER(TEMP-TABLE facilities:DEFAULT-BUFFER-HANDLE).                    */
/* StockDataSet:ADD-RELATION(BUFFER tt_EAN:HANDLE, BUFFER facilities:HANDLE,"upcid,upcid"). */

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
         HEIGHT             = 14.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN GetData.
/* FOR EACH facilities WHERE facilities.availableQuantity = 0: */
/*     DELETE facilities.                                      */
/* END.                                                        */
TEMP-TABLE facilities:write-json("longchar",lcStock,FALSE).
lcStock = TRIM(lcStock,"~{").
lcStock = CHR(91) + CHR(123) + '"upcid":"' + cEAN + '",' + lcStock + CHR(93).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData Procedure 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iRecType AS INT  NO-UNDO.
DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.

FIND strekkode WHERE strekkode.kode = cEan NO-LOCK NO-ERROR.

/* CREATE tt_EAN.                                                            */
/* ASSIGN tt_EAN.upcid   = cEAN                                              */
/*        tt_EAN.strkode = IF AVAIL strekkode THEN strekkode.strkode ELSE 0. */
FOR EACH butiker WHERE TRIM(Butiker.EksterntId) <> "" NO-LOCK:
    CREATE facilities.
    ASSIGN facilities.upcid        = cEAN
           facilities.butik        = string(butiker.butik)
           facilities.id           = butiker.EksterntId
           facilities.facilityType = "retail".
END.

IF AVAIL strekkode THEN DO:
    FOR EACH facilities:
        FIND FIRST artlag WHERE artlag.artikkelnr = strekkode.artikkelnr AND
                                artlag.butik      = INT(facilities.butik)     AND
                                artlag.strkode    = strekkode.strkode   NO-LOCK NO-ERROR.
        IF AVAIL artlag AND artlag.lagant > 0 THEN
            facilities.availableQuantity = artlag.lagant.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

