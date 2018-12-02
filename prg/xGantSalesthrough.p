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

/* DEFINE INPUT  PARAMETER dDato AS DATE       NO-UNDO. */
def var dParaDato as date no-undo.
def var iMinus as inte no-undo.
def var dLoopDato as date no-undo.
def var cFilkatalog as char no-undo.
def var cFilprefix as char no-undo.
DEFINE STREAM xmlfile.

DEFINE TEMP-TABLE TT_MapBut NO-UNDO
    FIELD Butikknr  AS INTE
    FIELD Gantstore AS INTE
    INDEX butik IS PRIMARY UNIQUE Butikknr.

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
         HEIGHT             = 19.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 1 no-lock.
cFilkatalog = right-trim(syspara.parameter1,"\") + "\".

find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 6 no-lock.
cFilprefix = syspara.parameter1.


/* run ExportButiker. */
RUN ByggButMappning.
IF CAN-FIND(FIRST bonghode WHERE BongHode.pfFlagg = 1) THEN
    run ExportData.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggButMappning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggButMappning Procedure 
PROCEDURE ByggButMappning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH SysPara WHERE SysPara.SysHId = 210 AND 
                           SysPara.SysGr  = 261 NO-LOCK.
        CREATE TT_MapBut.
        ASSIGN Butikknr  = SysPara.Paranr
               Gantstore = INT(SysPara.Parameter1).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportButiker Procedure 
PROCEDURE ExportButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var cButiker as char no-undo.
    def var cParaDatotid as char no-undo.
    def var cTst as char no-undo.
    
    find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 7 no-lock.
    assign cParaDatoTid = syspara.parameter1.
    for each butiker no-lock.
        if edato = ? then
            cButiker = cButiker + (if cButiker <> "" then "," else "") + string(butiker.butik).
        else do:
            cTst = string(year(butiker.edato),"9999") + string(month(butiker.edato),"99") + 
                   string(day(butiker.edato),"99") + string(butiker.etid,"99999").
            if cTst > cParaDatoTid then
                cButiker = cButiker + (if cButiker <> "" then "," else "") + string(butiker.butik).
        end.
    end.
    if cButiker <> "" then do:
        message cButiker view-as alert-box.
/*     run Butiker2File. */
        cTst = string(year(TODAY),"9999") + string(month(TODAY),"99") + 
               string(day(TODAY),"99") + string(TIME,"99999").
        find current syspara exclusive.
        syspara.parameter1 = cTst.
    end.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportData Procedure 
PROCEDURE ExportData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hDoc AS HANDLE no-undo.
    DEFINE VARIABLE hRoot AS HANDLE no-undo.
    DEFINE VARIABLE hRow AS HANDLE no-undo.
    DEFINE VARIABLE hField AS HANDLE no-undo.
    DEFINE VARIABLE hText AS HANDLE no-undo.
    DEFINE VARIABLE hDBFld AS HANDLE no-undo.
    DEFINE VARIABLE hHead AS HANDLE no-undo.
    DEFINE VARIABLE hNode1 AS HANDLE no-undo.
    DEFINE VARIABLE hNode2 AS HANDLE no-undo.
    DEFINE VARIABLE hNode3 AS HANDLE no-undo.
    DEFINE VARIABLE hItemGroups AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hItemGroup AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iRad AS integer no-undo.    
    DEFINE VARIABLE cGantFil AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lDataFinnes AS LOGICAL     NO-UNDO.

    CREATE X-DOCUMENT hDoc.
    hdoc:ENCODING = 'utf-8'.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hHead.
    CREATE X-NODEREF hNode1.
    CREATE X-NODEREF hNode2.
    CREATE X-NODEREF hNode3.
    CREATE X-NODEREF hRow.
    CREATE X-NODEREF hField.
    CREATE X-NODEREF hText.
    CREATE X-NODEREF hItemGroups.
    CREATE X-NODEREF hItemGroup.
    /* sätt datum och tidsstränggar */
/*     MESSAGE "Ingen från avdelning 1"       */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*     FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR. */
/*     IF NOT AVAIL butiker THEN                                      */
/*         RETURN.                                                    */
    /* Get a buffer for the Customer table. */
    /* Set up a root node. */
    hDoc:CREATE-NODE (hRoot, "Transfer", "ELEMENT").
    hDoc:APPEND-CHILD (hRoot).
    hDoc:CREATE-NODE (hHead, "Head", "ELEMENT").
    hRoot:APPEND-CHILD (hHead).
    hDoc:CREATE-NODE (hNode1, "Date", "ELEMENT").
    hHead:APPEND-CHILD (hNode1).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hNode1:APPEND-CHILD (hText).
    hText:NODE-VALUE = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") + " " + STRING(TIME,"HH:MM").
    hDoc:CREATE-NODE (hNode1, "Provider", "ELEMENT").
    hHead:APPEND-CHILD (hNode1).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hNode1:APPEND-CHILD (hText).
    hText:NODE-VALUE = "Polygon".
    hDoc:CREATE-NODE (hNode1, "Type", "ELEMENT").
    hHead:APPEND-CHILD (hNode1).
    hDoc:CREATE-NODE (hText, "", "TEXT").
    hNode1:APPEND-CHILD (hText).
    hText:NODE-VALUE = "Transaction".

for each bonghode where BongHode.pfFlagg = 1:
    IF Bonghode.makulert <> 2 AND CAN-FIND(TT_MapBut WHERE TT_MapBut.butikknr = Bonghode.butikknr) THEN DO:
        FOR EACH Bonglinje WHERE bonglinje.b_id = BongHode.b_id AND bonglinje.makulert = FALSE AND can-do("1,10",string(bonglinje.ttid)):
            FIND TT_MapBut WHERE TT_MapBut.butikknr = Bonghode.butikknr.
            lDataFinnes = TRUE.
            iRad = iRad + 1.
            hDoc:CREATE-NODE (hHead, "Transaction", "ELEMENT").
            hRoot:APPEND-CHILD (hHead).
            hDoc:CREATE-NODE (hNode1, "Action", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = "S".
            hDoc:CREATE-NODE (hNode1, "Id", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = string(iRad).
            
            hDoc:CREATE-NODE (hNode1, "Date", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = string(year(Bonghode.dato),"9999") + "-" + string(month(Bonghode.dato),"99") + "-" + string(day(Bonghode.dato),"99") + " " + string(Bonghode.tid,"HH:MM").
            
            hDoc:CREATE-NODE (hNode1, "EAN", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = Bonglinje.strekkode.
            
            hDoc:CREATE-NODE (hNode1, "Quantity", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = string(Bonglinje.antall).
            
            hDoc:CREATE-NODE (hNode1, "StoreId", "ELEMENT").
            hHead:APPEND-CHILD (hNode1).
            hDoc:CREATE-NODE (hText, "", "TEXT").
            hNode1:APPEND-CHILD (hText).
            hText:NODE-VALUE = string(TT_MapBut.Gantstore).
        END.
    END.
    Bonghode.pfflagg = 3.
END.
    cGantFil = cFilKatalog + cFilprefix + substr(string(YEAR(TODAY),"9999"),3) + string(MONTH(TODAY),"99") + string(DAY(TODAY),"99") + "T.xml".

    OUTPUT STREAM xmlfile to VALUE(cGantFil) NO-ECHO.
    hDoc:SAVE ("stream", "xmlfile").
    OUTPUT STREAM xmlfile CLOSE.
    IF lDataFinnes = FALSE THEN
        OS-DELETE VALUE(cGantFil).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExportDataxx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportDataxx Procedure 
PROCEDURE ExportDataxx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    
    find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 2 no-lock.
    iMinus = int(syspara.parameter1).
    find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 4 no-lock.
    dParadato = date(Syspara.Parameter1).
    if ddato = ? then do dLoopDato = dParaDato + 1 to today - iMinus:
        if can-find(first translogg where translogg.dato = dLoopDato and (translogg.ttid = 1 or translogg.ttid = 3 or translogg.ttid = 10)) then do:
            RUN Transactions (dLoopDato).
/*             RUN Inventory (dLoopDato). /* detta kan väl bara köras dags dato. */ */
        end.
        find syspara where SysPara.SysHId = 210 and SysPara.SysGr = 260 and SysPara.ParaNr = 4 exclusive.
        Syspara.parameter1 = string(dLoopDato).
        release syspara.
    end.
    else if dDato <= dParaDato then
        RUN Transactions (dDato).
*/


    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Inventory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inventory Procedure 
PROCEDURE Inventory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

