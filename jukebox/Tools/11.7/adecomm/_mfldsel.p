/*********************************************************************
* Copyright (C) 2005 by Progress Software Corporation. All rights    *
* reserved.  Prior versions of this work may contain portions        *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*
 * _m_fldsel.p - side-by-side field picker
 */

/*
p_TblList     -> list of dbname.tables that should be viewed in list 1 (the left
                 side)
                 Note: Leave blank ("") if selecting from a DataObject.
p_hDataObject -> handle to DataObject that should be viewed in list 1
p_TT          -> ? if no temp-tables need to be included
                 Otherwise a comma delitted list where each entry has the form:
                 _like-db._like-table|_name
                 The _like-db and _like-table are real database.table combinations
                 and _name is either the name of the temp-table that is like
                 _like-table or ? if it is the same.

p_Items       -> Either "1", "2", or "3".  This indicates the number of
      	          components that will be used in the select lists and in 
                 the output.  i.e., 1: field, 2: table.field, 3: db.table.field
p_Dlmtr       -> The delimiter of the p_Result List
p_Exclude     -> Delimited list of fields to exclude from the Available Fields
                 list.
p_Result      -> input-output list of selected fields
*/

/*
   IT DOESN'T LOOK LIKE THIS ANYMORE, BUT THIS GIVES YOU THE IDEA! 

        Available Fields                  Selected Fields
+------------------------------+ +------------------------------+  
| Cust-num  (demo.customer)  | | |                            | |  
| Name      (demo.customer)  | | |                            | |  
| Address   (demo.customer)  | | |                            | | +----------+
| Address2  (demo.customer)  | | |                            | | |  Add >>  |
| City      (demo.customer)  | | |                            | | +----------+
| St        (demo.customer)  | | |                            | | +----------+
| Zip       (demo.customer)  | | |                            | | |<< Remove |
| Phone     (demo.customer)  | | |                            | | +----------+
| Contact   (demo.customer)  | | |                            | | +----------+
| Sales-rep (demo.customer)  | | |                            | | | Move Up  |
|                            | | |                            | | +----------+
|                            | | |                            | | +----------+
|                            | | |                            | | |Move Down |
+------------------------------+ +------------------------------+ +----------+
*/     
/* Modified
   wood 8-18-95 Deselect Avail Fields after ADD of multiple fields
                (Bug # 95-07-18-090).  
   wood 6-6-96  Clicking Cancel returns "Cancel". This avoids the
                problem of sending in an initial list and then
                clicking Cancel. You cannot just check p_Result because
                it won't be empty.
                
   Brynjar 12/20/13:
           Added procedure ExpandFrame                     
           Added pre-selected fields from JukeBox view definitions (relevant for browsers)
           When a browse has existing fields move up/down is disabled (p_Result = "NoMove")
*/
&GLOBAL-DEFINE WIN95-BTN YES
{adecomm/commeng.i}  /* Help contexts */
{adecomm/adestds.i}  /* Standard layout stuff, colors etc. */
{adecomm/tt-brws.i "NEW"}

DEFINE INPUT        PARAMETER p_TblLst      AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER p_hDataObject AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT        PARAMETER p_TT          AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER p_Items       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER p_Dlmtr       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER p_Exclude     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p_Result      AS CHARACTER NO-UNDO.

DEFINE VARIABLE stat          AS LOGICAL NO-UNDO. /* For status of widget methods */
DEFINE VARIABLE useDataObject AS LOGICAL NO-UNDO.

DEF VAR cFieldsInUse AS CHAR NO-UNDO. /* <- Brynjar, JukeBox - tables defined in query / queries */
DEF VAR bMove        AS LOG  NO-UNDO INIT YES.
IF p_Result = "NoMove" THEN
  ASSIGN bMove = NO
         p_Result = "".

DEF VAR cDbName       AS CHAR NO-UNDO.
DEF VAR cTableName    AS CHAR NO-UNDO.
DEF VAR cFieldName    AS CHAR NO-UNDO.
DEF VAR cLabel        AS CHAR NO-UNDO.
DEF VAR cDataType     AS CHAR NO-UNDO.
DEF VAR cFormat       AS CHAR NO-UNDO.
DEF VAR cHelp         AS CHAR NO-UNDO.
DEF VAR cDesc         AS CHAR NO-UNDO.
DEF VAR cDEF-LABEL-ATTR AS CHARACTER NO-UNDO.
DEF VAR cDEF-FORMAT-ATTR  AS CHAR NO-UNDO.
DEF VAR cDEF-HELP-ATTR    AS CHAR NO-UNDO.
DEF VAR cDEF-VALEXP   AS CHAR NO-UNDO.
DEF VAR extnt         AS INT  NO-UNDO.
DEF VAR intl          AS CHAR NO-UNDO.
DEF VAR valmsg        AS CHAR NO-UNDO.
DEF VAR valmsg-sa     AS CHAR NO-UNDO.
DEF VAR bMANDATORY    AS LOG  NO-UNDO.
DEF VAR cTmp          AS CHAR NO-UNDO.
DEF VAR cList         AS CHAR NO-UNDO.
DEF VAR iSpaceWidth   AS INT  NO-UNDO.

/*--------------------------------------------------------------------------*/

DEFINE VARIABLE v_SrcLst AS CHARACTER  NO-UNDO
  VIEW-AS SELECTION-LIST MULTIPLE SIZE 31 by 10 SCROLLBAR-V SCROLLBAR-H.
DEFINE VARIABLE v_TargLst AS CHARACTER NO-UNDO
  VIEW-AS SELECTION-LIST MULTIPLE SIZE 31 by 10 SCROLLBAR-V SCROLLBAR-H.
/*DEFINE VARIABLE v_OrigSrc AS CHARACTER NO-UNDO. */

DEFINE VARIABLE cArrayEntry AS CHARACTER NO-UNDO.
DEFINE VARIABLE cArrayList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTemp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE first-time  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iArrayHigh  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iArrayLow   AS INTEGER   NO-UNDO. 
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE j           AS INTEGER   NO-UNDO.
DEFINE VARIABLE l_Cancel    AS LOGICAL   NO-UNDO.

DEFINE BUTTON qbf-up LABEL "Move &Up"   SIZE 13 BY 1.125.
DEFINE BUTTON qbf-dn LABEL "Move &Down" SIZE 13 BY 1.125.
DEFINE BUTTON qbf-ad LABEL "&Add >>"    SIZE 13 BY 1.125.
DEFINE BUTTON qbf-rm LABEL "<< &Remove" SIZE 13 BY 1.125.

Define button qbf-ok   label "OK"      {&STDPH_OKBTN} AUTO-GO.
Define button qbf-cn   label "Cancel"  {&STDPH_OKBTN} AUTO-ENDKEY.
Define button qbf-hlp  label "&Help"   {&STDPH_OKBTN}.

/* Dialog Button Box */
&IF {&OKBOX} &THEN
   DEFINE RECTANGLE rect_Btn_Box {&STDPH_OKBOX}.
&ENDIF

DEFINE VARIABLE t_int AS INTEGER   NO-UNDO. /* scrap/loop */
DEFINE VARIABLE t_log AS LOGICAL   NO-UNDO. /* scrap/loop */
define variable wintitle as character no-undo init "Multi-Field Selector":t32.
FORM 
  SKIP ({&TFM_WID})
  "Available Fields:":L31 AT 2 VIEW-AS TEXT
  "Selected Fields:" 	  AT 48 VIEW-AS TEXT
  SKIP({&VM_WID})

  v_SrcLst     	     	  AT 2 
  v_TargLst    	     	  AT 48              

  { adecomm/okform.i
      &BOX    ="rect_Btn_Box"
      &OK     ="qbf-ok"
      &CANCEL ="qbf-cn"
      &HELP   ="qbf-hlp" 
  }

  qbf-ad AT ROW 4 COL 34 SKIP ({&VM_WID})
  qbf-rm AT 34 	     	 SKIP ({&VM_WID})
  qbf-up AT 34 	     	 SKIP ({&VM_WID})
  qbf-dn AT 34 		 SKIP ({&VM_WID})
  SKIP ({&VM_WID})
  WITH FRAME FldPicker NO-LABELS
   DEFAULT-BUTTON qbf-ok CANCEL-BUTTON qbf-cn
   &if DEFINED(IDE-IS-RUNNING) = 0  &then 
   TITLE wintitle VIEW-AS DIALOG-BOX
   &else
   NO-BOX THREE-D
   &endif
   .

ASSIGN v_SrcLst:DELIMITER  = p_Dlmtr
       v_TargLst:DELIMITER = p_Dlmtr.


&if DEFINED(IDE-IS-RUNNING) <> 0  &then
 {adeuib/ide/dialoginit.i "FRAME FldPicker:handle"}
  dialogService:View(). 
&endif


RUN ExpandFrame.  /* Brynjar */
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ON "VALUE-CHANGED" OF v_SrcLst IN FRAME FldPicker DO:
  DEFINE VAR some_thing AS LOGICAL.
  some_thing = SELF:SCREEN-VALUE NE ?.

  /* Sensitize ADD button only if something is selected */
  IF some_thing NE qbf-ad:SENSITIVE 
  THEN qbf-ad:SENSITIVE = some_thing.
  /* Turn off the Target List and Actions */
  IF qbf-rm:SENSITIVE
  THEN ASSIGN
    v_TargLst:SCREEN-VALUE = ""
    qbf-rm:SENSITIVE       = no
    qbf-up:SENSITIVE       = no
    qbf-dn:SENSITIVE       = no.

END.

ON "VALUE-CHANGED" OF v_TargLst IN FRAME FldPicker DO:
  DEFINE VAR some_thing AS LOGICAL.
  some_thing = SELF:SCREEN-VALUE NE ?. 
  
  /* Sensitize Remove/Move buttons only if something is selected */
  IF some_thing NE qbf-rm:SENSITIVE THEN
    ASSIGN qbf-rm:SENSITIVE = some_thing.
  
  ASSIGN qbf-up:SENSITIVE = ENTRY(1,v_TargLst:SCREEN-VALUE,p_Dlmtr)
                                  NE v_TargLst:ENTRY(1) AND bMove
         qbf-dn:SENSITIVE = ENTRY(NUM-ENTRIES(v_TargLst:SCREEN-VALUE,p_Dlmtr),
                                              v_TargLst:SCREEN-VALUE,p_Dlmtr) NE
                                  v_Targlst:ENTRY(v_Targlst:NUM-ITEMS) AND bMove.
             
  /* Turn off the Source List and Actions */
  IF qbf-rm:SENSITIVE
  THEN ASSIGN
    v_SrcLst:SCREEN-VALUE  = ""
    qbf-ad:SENSITIVE       = no.
END.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ON "GO" OF FRAME FldPicker DO:
  DEFINE VARIABLE tmp-entry AS CHARACTER                             NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER                               NO-UNDO.

  RUN adecomm/_setcurs.p ("WAIT":U).
  p_Result = (IF v_TargLst:LIST-ITEMS IN FRAME FldPicker = ? 
              THEN ""
              ELSE v_TargLst:LIST-ITEMS IN FRAME FldPicker
              ).
  IF INDEX(p_TblLst,"Temp-Tables":U) > 0 THEN DO:
    DO i = 1 TO NUM-ENTRIES(p_Result, p_Dlmtr):
      tmp-entry = ENTRY(i, p_Result, p_Dlmtr).
      IF NUM-ENTRIES(tmp-entry,".":U) = 2 AND
         LOOKUP("Temp-Tables.":U + ENTRY(1, tmp-entry, ".":U), p_TblLst) > 0 THEN
           ENTRY(i, p_Result, p_Dlmtr) = "Temp-Tables.":U + tmp-entry.
      ELSE IF (useDataObject OR p_TT = ?) AND
           NUM-ENTRIES(tmp-entry,".":U) = 1 THEN
        /* Assume DataObject RowObject table. jep-code */
        ENTRY(i, p_Result, p_Dlmtr) = p_TblLst + ".":U + tmp-entry.
    END.  /* Do i = 1 to Num-Entries */
  END.  /* If  there are temp-tables in the initial table list */

  /* Brynjar - remove datatypes */
  DO i = 1 TO NUM-ENTRIES(p_Result,p_Dlmtr):
    tmp-entry = ENTRY(i, p_Result, p_Dlmtr).
    IF INDEX(tmp-entry," (") > 0 THEN
      tmp-entry = SUBSTRING(tmp-entry,1,INDEX(tmp-entry," (")).
    ENTRY(i, p_Result, p_Dlmtr) = TRIM(tmp-entry).
  END.
END.  /* On GO of Frame FldPicker */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ON WINDOW-CLOSE OF FRAME FldPicker
   apply "END-ERROR" to frame FldPicker.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ON "CHOOSE" OF qbf-up IN FRAME FldPicker DO:
  DEFINE VARIABLE qbf_i AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_j AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_l AS CHARACTER NO-UNDO.
  DEFINE VARIABLE qbf_v AS CHARACTER NO-UNDO.
  IF v_TargLst:SCREEN-VALUE IN FRAME FldPicker <> ? THEN DO:
    ASSIGN
      qbf_v = v_TargLst:SCREEN-VALUE IN FRAME FldPicker
      qbf_l = v_TargLst:LIST-ITEMS IN FRAME FldPicker
      qbf_i = LOOKUP(ENTRY(1,qbf_v,p_Dlmtr),qbf_l,p_Dlmtr).
    DO qbf_j = 1 TO NUM-ENTRIES(qbf_v,p_Dlmtr):
      ASSIGN
        ENTRY(LOOKUP(ENTRY(qbf_j,qbf_v,p_Dlmtr),qbf_l,p_Dlmtr),qbf_l,p_Dlmtr) = ""
        qbf_l = TRIM(REPLACE(qbf_l,p_Dlmtr + p_Dlmtr,p_Dlmtr),p_Dlmtr).
    END.
    ASSIGN
      qbf_i = MAXIMUM(qbf_i - 1,1)
      ENTRY(qbf_i,qbf_l,p_Dlmtr) = qbf_v +
                              (IF qbf_l = "" THEN ""
                                             ELSE p_Dlmtr + ENTRY(qbf_i,qbf_l,p_Dlmtr))
      v_TargLst:LIST-ITEMS IN FRAME FldPicker = qbf_l
      v_TargLst:SCREEN-VALUE      IN FRAME FldPicker = qbf_v
      qbf-up:SENSITIVE = ENTRY(1,v_TargLst:SCREEN-VALUE,p_Dlmtr)
                                NE v_TargLst:ENTRY(1) 
      qbf-dn:SENSITIVE = ENTRY(NUM-ENTRIES(v_TargLst:SCREEN-VALUE,p_Dlmtr),
                                         v_TargLst:SCREEN-VALUE,p_Dlmtr) NE
                         v_Targlst:ENTRY(v_Targlst:NUM-ITEMS).
  END.
END /*TRIGGER*/ .

ON "CHOOSE" OF qbf-dn IN FRAME FldPicker DO:
  DEFINE VARIABLE qbf_i AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_j AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_l AS CHARACTER NO-UNDO.
  DEFINE VARIABLE qbf_v AS CHARACTER NO-UNDO.
  IF v_TargLst:SCREEN-VALUE IN FRAME FldPicker <> ? THEN DO:
    ASSIGN
      qbf_v = v_TargLst:SCREEN-VALUE IN FRAME FldPicker
      qbf_l = v_TargLst:LIST-ITEMS IN FRAME FldPicker
      qbf_i = LOOKUP(ENTRY(1,qbf_v,p_Dlmtr),qbf_l,p_Dlmtr).
    DO qbf_j = 1 TO NUM-ENTRIES(qbf_v,p_Dlmtr):
      ASSIGN
        ENTRY(LOOKUP(ENTRY(qbf_j,qbf_v,p_Dlmtr),qbf_l,p_Dlmtr),qbf_l,p_Dlmtr) = ""
        qbf_l = REPLACE(qbf_l,p_Dlmtr + p_Dlmtr,p_Dlmtr).
    END.
    ASSIGN
      qbf_l = TRIM(qbf_l,p_Dlmtr)
      qbf_i = MINIMUM(qbf_i,NUM-ENTRIES(qbf_l,p_Dlmtr)).
    IF qbf_i = 0 THEN
      qbf_l = qbf_v.
    ELSE
      ENTRY(qbf_i,qbf_l,p_Dlmtr) = ENTRY(qbf_i,qbf_l,p_Dlmtr) + p_Dlmtr + qbf_v.
    ASSIGN
      v_TargLst:LIST-ITEMS IN FRAME FldPicker   = qbf_l
      v_TargLst:SCREEN-VALUE IN FRAME FldPicker = qbf_v
      qbf-up:SENSITIVE = ENTRY(1,v_TargLst:SCREEN-VALUE,p_Dlmtr)
                                NE v_TargLst:ENTRY(1)
      qbf-dn:SENSITIVE = ENTRY(NUM-ENTRIES(v_TargLst:SCREEN-VALUE,p_Dlmtr),
                                         v_TargLst:SCREEN-VALUE,p_Dlmtr) NE
                         v_Targlst:ENTRY(v_Targlst:NUM-ITEMS).
  END.
END /*TRIGGER*/ .


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
ON   CHOOSE                OF qbf-ad    IN FRAME FldPicker
  OR MOUSE-SELECT-DBLCLICK OF v_SrcLst  IN FRAME FldPicker DO:
  /* Why MOUSE-SELECT-DBLCLICK and not DEFAULT-ACTION?  Because this is in
     a dialog box and the RETURN key will fire the GO for the frame.  I don't
     want it to also fire the ADD trigger. [Arguably, this is a bug with the
     Progress DEFAULT-BUTTON logic.  RETURN should do one or the other, not
     both DEFAULT-ACTION for selection-lists and DEFAULT-BUTTON for the frame */

  DEFINE VARIABLE cnt         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_a       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE qbf_i       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_v       AS CHARACTER NO-UNDO.
  DEFINE VAR empty_target     AS LOGICAL   NO-UNDO.

  IF v_SrcLst:SCREEN-VALUE IN FRAME FldPicker <> ? THEN DO:
    /* when going from left to right, always add to end of list */
    qbf_v = v_SrcLst:SCREEN-VALUE IN FRAME FldPicker.

    DO qbf_i = 1 TO NUM-ENTRIES(qbf_v,p_Dlmtr):
      cTemp = ENTRY (qbf_i, qbf_v,p_Dlmtr) NO-ERROR.
      
      /* If it matches the it is an array thing. */
      IF cTemp MATCHES ("*[*]*") THEN DO:
        ASSIGN cTemp      = TRIM (cTemp)
               cArrayList = SUBSTRING(cTemp,INDEX(cTemp,'[':u) + 1,-1,
                                      "CHARACTER":u)
               cArrayList = REPLACE(cArrayList,"]":u,"")
               cTemp      = SUBSTRING(cTemp,1,INDEX(cTemp,'[':u) - 1,
                                      "CHARACTER":u)
               NO-ERROR.

        /* Loop through the list base on " " */
        DO i = 1 TO NUM-ENTRIES (cArrayList, ' '):
          ASSIGN cArrayEntry = ENTRY (i, cArrayList, ' ')
                 iArrayLow   = INTEGER (ENTRY (1, cArrayEntry, '-'))
                 iArrayHigh  = INTEGER (ENTRY (NUM-ENTRIES (cArrayEntry, '-'),
                                                            cArrayEntry, '-'))
                 NO-ERROR.

          /* Loop through the list base on X-Y. X is the low number.
                                                Y is the high number.           */
          IF NOT ERROR-STATUS:ERROR THEN        /* Bugfix, Brynjar , Jukebox 31.oct.2017 */
          DO j = iArrayLow TO iArrayHigh:
            ASSIGN cArrayEntry = cTemp + '[' + STRING (j) + ']'.
            /* Add element if it is not already there */
            IF v_TargLst:LOOKUP(cArrayEntry) = 0 THEN
                   qbf_a = v_TargLst:ADD-LAST (cArrayEntry) IN FRAME FldPicker.
          END. /* DO J */
        END. /* DO I */
      END. /* IF MATCHES */
      ELSE qbf_a = v_TargLst:ADD-LAST(ENTRY(qbf_i,qbf_v,p_Dlmtr)) IN FRAME FldPicker.

      /* Keep the position in the list highlighted if there was only one. */
      IF NUM-ENTRIES(qbf_v,p_Dlmtr) eq 1
      THEN RUN adecomm/_delitem.p (v_SrcLst:HANDLE, 
                                  ENTRY(qbf_i, qbf_v, p_Dlmtr),
                                  OUTPUT cnt) NO-ERROR.  
      ELSE DO: 
/*         IF ENTRY(qbf_i, qbf_v, p_Dlmtr) NE "" THEN /* Bugfix, Brynjar , Jukebox 31.oct.2017 */ */
          stat = v_SrcLst:DELETE (ENTRY(qbf_i, qbf_v, p_Dlmtr)) NO-ERROR. 
          IF ERROR-STATUS:ERROR THEN
              MESSAGE qbf_i SKIP qbf_v SKIP p_dlmtr
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END.

    END. /* NUM-ENTRIES */
    /* If the source has no selection then disable all the buttons */
    qbf-ad:SENSITIVE IN FRAME FldPicker = ( v_SrcLst:SCREEN-VALUE NE ? ).
  END.
END /*TRIGGER*/ .

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*/
ON   CHOOSE                OF qbf-rm     IN FRAME FldPicker
  OR MOUSE-SELECT-DBLCLICK OF v_TargLst  IN FRAME FldPicker DO:
  /* Why MOUSE-SELECT-DBLCLICK and not DEFAULT-ACTION?  Because this is in
     a dialog box and the RETURN key will fire the GO for the frame.  I don't
     want it to also fire the REMOVE trigger. [Arguably, this is a bug with the
     Progress DEFAULT-BUTTON logic.  RETURN should do one or the other, not
     both DEFAULT-ACTION for selection-lists and DEFAULT-BUTTON for the frame */
  DEFINE VARIABLE qbf_a AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE qbf_i AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_j AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cnt   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE qbf_l AS CHARACTER NO-UNDO.
  DEFINE VARIABLE qbf_v AS CHARACTER NO-UNDO.
  DEFINE VAR empty_target	AS LOGICAL			NO-UNDO.
  
  IF v_TargLst:SCREEN-VALUE IN FRAME FldPicker <> ? THEN DO:
    /* when going from right to left, put back in original position */

    ASSIGN
      qbf_l = v_SrcLst:LIST-ITEMS IN FRAME FldPicker
      qbf_v = v_TargLst:SCREEN-VALUE IN FRAME FldPicker.

    DO qbf_i = 1 TO NUM-ENTRIES(qbf_v, p_Dlmtr):
      /* Keep the position in the list highlighted if there was only one. */
      IF NUM-ENTRIES(qbf_v,p_Dlmtr) eq 1
      THEN RUN adecomm/_delitem.p (v_TargLst:HANDLE, 
                                   ENTRY (qbf_i, qbf_v, p_Dlmtr), 
                                   OUTPUT cnt).

      ELSE stat = v_TargLst:DELETE (ENTRY(qbf_i, qbf_v, p_Dlmtr)) NO-ERROR. 

      RUN adecomm/_collaps.p (v_SrcLst:HANDLE, ENTRY (qbf_i, qbf_v, p_Dlmtr)).
    END.
    
    
    /* If the target is empty then disable all the buttons. */
    empty_target = ( v_TargLst:SCREEN-VALUE eq ? ).
    ASSIGN
         qbf-rm:SENSITIVE IN FRAME FldPicker = NOT empty_target 
         qbf-up:SENSITIVE IN FRAME FldPicker = NOT empty_target AND bMove
         qbf-dn:SENSITIVE IN FRAME FldPicker = NOT empty_target AND bMove
         .
    /* Empty the screen-value if the list is empty. There seems to be
       a bug here that the SCREEN-VALUE becomes UNKNOWN and stops the
       next VALUE-CHANGED from firing. */
    IF empty_target THEN v_TargLst:SCREEN-VALUE = "".

  END.
END /*TRIGGER*/ .

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*----- HELP -----*/
ON HELP of FRAME FldPicker OR CHOOSE OF qbf-hlp IN FRAME FldPicker
   RUN "adecomm/_adehelp.p" (INPUT "comm", INPUT "CONTEXT", 
      	       	     	     INPUT {&Multi_Field_Selection},
      	       	     	     INPUT ?).


/*--------------------------------------------------------------------------*/

{ adecomm/okrun.i
    &FRAME  = "FRAME FldPicker"
    &BOX    = "rect_Btn_Box"
    &OK     = "qbf-ok"
    &HELP   = "qbf-hlp"
}

useDataObject = IF VALID-HANDLE(p_hDataObject) THEN TRUE ELSE FALSE.

/* JEP-UNFINISHED - _getdlst.p: should this support temp-tables? */
IF useDataObject THEN
DO:
   RUN adecomm/_getdlst.p
        (INPUT v_SrcLst:HANDLE,
         INPUT p_hDataObject,
         INPUT no,
         INPUT p_Items,
         INPUT ?,
         OUTPUT t_log).
   IF NOT t_log THEN
     RETURN.        
END.
/* Add temp-tables fields to the available list.
  This used to be an ELSE. I changed it to the IF check instead.
  jep-code */
IF (NOT useDataObject) OR (p_TT <> ?) THEN
DO:
    RUN adecomm/_mfldlst.p
        (INPUT v_SrcLst:HANDLE,
         INPUT p_TblLst,
         INPUT p_TT,
         INPUT yes,
         INPUT p_Items,
         INPUT 1,
         INPUT "",
         OUTPUT t_log).
    IF NOT useDataObject AND NOT t_log THEN
      RETURN.        
END.

/* Brynjar for JukeBox:  */
cFieldsInUse = DYNAMIC-FUNCTION("getFieldsInUse") NO-ERROR.
IF NOT ERROR-STATUS:ERROR AND cFieldsInUse NE "" AND cFieldsInUse NE ?
   AND NOT DYNAMIC-FUNCTION("getIsNotJukeBoxObject") THEN
  p_Result = REPLACE(cFieldsInUse,CHR(10),p_Dlmtr).

/* Remove array elements from v_SrcLst that might already be there */
IF p_Result NE "" THEN DO:  /* If first time, no need to check for elements */
  DO t_int = 1 TO v_SrcLst:NUM-ITEMS:
    /* Check if an array */
    cTemp = v_SrcLst:ENTRY(t_int).
    IF INDEX(cTemp,"[":U) > 0 THEN DO:  /* This is an array */
      ASSIGN cArrayList = ENTRY(2, cTemp, '[':u)
             cArrayList = ENTRY(1,cArrayList,']':u)
             cTemp      = ENTRY(1,cTemp,'[':U)
             first-time = TRUE.

      /* Loop through the list base on " " */
      DO i = 1 TO NUM-ENTRIES (cArrayList, ' '):
        ASSIGN cArrayEntry = ENTRY (i, cArrayList, ' ')
               iArrayLow   = INTEGER (ENTRY (1, cArrayEntry, '-'))
               iArrayHigh  = INTEGER (ENTRY (NUM-ENTRIES (cArrayEntry, '-'),
                                                          cArrayEntry, '-')).

        IF first-time THEN /* Clear existing brackets */
          ASSIGN  t_log = v_SrcLst:REPLACE(cTemp + "[]":U, t_int)
                  first-time = FALSE.
                       
        /* Loop through the list base on X-Y. X is the low number.
                                              Y is the high number.           */
        DO j = iArrayLow TO iArrayHigh:
          ASSIGN cArrayEntry = cTemp + '[' + STRING (j) + ']'.
          /* Add and "Collapse" cArrayEntry out if it is NOT
             in the results list                              */
          IF LOOKUP(cArrayEntry, p_Result, p_Dlmtr) = 0 THEN DO:
            RUN adecomm/_collaps.p (v_SrcLst:HANDLE, cArrayEntry).
          END.
        END. /* DO J */
      END. /* DO I */
      IF v_SrcLst:ENTRY(t_int) MATCHES "*[]" THEN
         t_log = v_SrcLst:DELETE(t_int).
    END.  /* This is an array */  
  END. /* DO t_int 1 to num-items */
END.  /* If possibility of array elements */
/*
** Make sure p_Result items are in the source list.  If they are, then
** remove them and put the item in the v_TargLst selection list.
*/
/* v_OrigSrc = v_SrcLst:LIST-ITEMS in frame FldPicker. */
DO t_int = 1 TO NUM-ENTRIES(p_Result, p_Dlmtr):
  ASSIGN t_log = v_SrcLst:DELETE(ENTRY(t_int,p_Result, p_Dlmtr)) IN FRAME FldPicker
         t_log = v_TargLst:ADD-LAST(ENTRY(t_int,p_Result, p_Dlmtr)) IN FRAME FldPicker.
END.

/* Remove any fields the caller wants excluded from available fields. */
IF p_Exclude <> "" THEN
DO t_int = 1 TO NUM-ENTRIES(p_Exclude, p_Dlmtr):
  ASSIGN t_log = v_SrcLst:DELETE(ENTRY(t_int, p_Exclude, p_Dlmtr)) IN FRAME FldPicker NO-ERROR.
  /* Handle cases where the selector must use the RowObject table name,
     such as when an object has other temp-tables defined for it. */
  ASSIGN t_log = v_SrcLst:DELETE("RowObject." + ENTRY(t_int, p_Exclude, p_Dlmtr)) IN FRAME FldPicker NO-ERROR.
END.

/* Brynjar - add datatypes: */
DO t_int = 1 TO v_SrcLst:NUM-ITEMS:
  cTmp = v_SrcLst:ENTRY(t_int).

  IF NUM-ENTRIES(cTmp,".") > 2 THEN
    ASSIGN cDbName = ENTRY(1,cTmp,".")
           cTableName = ENTRY(2,cTmp,".")
           cFieldName = ENTRY(3,cTmp,".").
  ELSE IF NUM-ENTRIES(cTmp,".") > 1 THEN
    ASSIGN cDbName    = LDBNAME("DICTDB")
           cTableName = ENTRY(1,cTmp,".")
           cFieldName = ENTRY(2,cTmp,".").
  ELSE
    ASSIGN cDbName    = LDBNAME("DICTDB")
           cTableName = (IF NUM-ENTRIES(p_TblLst,".") > 1 THEN ENTRY(2,p_TblLst,".") ELSE p_TblLst)
           cFieldName = cTmp.


  cDataType = DYNAMIC-FUNCTION("getFieldDataType",cTmp).
  IF cDataType = "" THEN 
    RUN adeuib/_fldinfo.p (INPUT cDbName,
                           INPUT cTableName,
                           INPUT cFieldName,
                           OUTPUT cLabel,
                           OUTPUT cDEF-LABEL-ATTR,
                           OUTPUT cFormat,
                           OUTPUT cDEF-FORMAT-ATTR,
                           OUTPUT cDataType,
                           OUTPUT cHelp,
                           OUTPUT cDEF-HELP-ATTR,
                           OUTPUT extnt,
                           OUTPUT intl,
                           OUTPUT cDesc,
                           OUTPUT cDEF-VALEXP,
                           OUTPUT valmsg,
                           OUTPUT valmsg-sa,
                           OUTPUT bMANDATORY).
  CASE cDataType:
      WHEN "character" THEN cDataType = "(char)".
      WHEN "integer"   THEN cDataType = "(int)".
      WHEN "decimal"   THEN cDataType = "(dec)".
      OTHERWISE cDataType = "(" + cDataType + ")".
  END CASE.
                           
  iSpaceWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cTmp, v_SrcLst:FRAME:FONT) / FONT-TABLE:GET-TEXT-WIDTH-PIXELS(" ", v_SrcLst:FRAME:FONT).

  cList = cList + (IF cList NE "" THEN p_Dlmtr ELSE "") 
        + cTmp 
        + FILL(" ",MAX(75 - iSpaceWidth,1)) 
        + cDataType.
END.
IF cList NE "" THEN v_SrcLst:LIST-ITEMS = cList.
cList = "".
DO t_int = 1 TO v_TargLst:NUM-ITEMS:
  cTmp = v_TargLst:ENTRY(t_int).

  IF NUM-ENTRIES(cTmp,".") > 2 THEN
    ASSIGN cDbName = ENTRY(1,cTmp,".")
           cTableName = ENTRY(2,cTmp,".")
           cFieldName = ENTRY(3,cTmp,".").
  ELSE IF NUM-ENTRIES(cTmp,".") > 1 THEN
    ASSIGN cDbName    = LDBNAME("DICTDB")
           cTableName = ENTRY(1,cTmp,".")
           cFieldName = ENTRY(2,cTmp,".").
  ELSE
    ASSIGN cDbName    = LDBNAME("DICTDB")
           cTableName = (IF NUM-ENTRIES(p_TblLst,".") > 1 THEN ENTRY(2,p_TblLst,".") ELSE p_TblLst)
           cFieldName = cTmp.

  cDataType = DYNAMIC-FUNCTION("getFieldDataType",cTmp).
  IF cDataType = "" THEN 
    RUN adeuib/_fldinfo.p (INPUT cDbName,
                           INPUT cTableName,
                           INPUT cFieldName,
                           OUTPUT cLabel,
                           OUTPUT cDEF-LABEL-ATTR,
                           OUTPUT cFormat,
                           OUTPUT cDEF-FORMAT-ATTR,
                           OUTPUT cDataType,
                           OUTPUT cHelp,
                           OUTPUT cDEF-HELP-ATTR,
                           OUTPUT extnt,
                           OUTPUT intl,
                           OUTPUT cDesc,
                           OUTPUT cDEF-VALEXP,
                           OUTPUT valmsg,
                           OUTPUT valmsg-sa,
                           OUTPUT bMANDATORY).
                           
  CASE cDataType:
      WHEN "character" THEN cDataType = "(char)".
      WHEN "integer"   THEN cDataType = "(int)".
      WHEN "decimal"   THEN cDataType = "(dec)".
      OTHERWISE cDataType = "(" + cDataType + ")".
  END CASE.
                           
  iSpaceWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cTmp, v_SrcLst:FRAME:FONT) / FONT-TABLE:GET-TEXT-WIDTH-PIXELS(" ", v_SrcLst:FRAME:FONT).

  cList = cList + (IF cList NE "" THEN p_Dlmtr ELSE "") 
        + cTmp 
        + FILL(" ",MAX(75 - iSpaceWidth,1)) 
        + cDataType.
END.
IF cList NE "" THEN v_TargLst:LIST-ITEMS = cList.

DYNAMIC-FUNCTION("NewMenuBand",v_SrcLst:HANDLE
                ,"ViewFieldInfo;More.."
                ,"").

/* End - adding data types */

ENABLE v_SrcLst 
       qbf-ad 
       v_TargLst 
       qbf-ok
       qbf-cn 
       qbf-hlp
       WITH FRAME FldPicker.
ASSIGN
   stat = qbf-rm:MOVE-AFTER-TAB-ITEM(qbf-ad:HANDLE IN FRAME FldPicker)
   stat = qbf-up:MOVE-AFTER-TAB-ITEM(qbf-rm:HANDLE IN FRAME FldPicker)
   stat = qbf-dn:MOVE-AFTER-TAB-ITEM(qbf-up:HANDLE IN FRAME FldPicker).

/* Select the first item in the source list */
IF v_SrcLst:NUM-ITEMS > 0 THEN v_SrcLst:SCREEN-VALUE = v_SrcLst:ENTRY(1).
 
/* Assume a cancel until the user hits OK. */
l_Cancel = yes.
&scoped-define CANCEL-EVENT U2
{adeuib/ide/dialogstart.i qbf-ok qbf-cn wintitle}
DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  APPLY "ENTRY"  TO v_SrcLst IN FRAME FldPicker.
  &if DEFINED(IDE-IS-RUNNING) = 0  &then
        WAIT-FOR "CHOOSE" OF qbf-ok IN FRAME FldPicker OR
           GO OF FRAME FldPicker. 
    &ELSE
        WAIT-FOR "choose" of qbf-ok in frame FldPicker or "u2" of this-procedure.       
        if cancelDialog THEN UNDO, LEAVE.  
    &endif
    
  /* No Cancel. */
  l_cancel = NO.
END.

HIDE FRAME FldPicker NO-PAUSE.

IF l_Cancel 
THEN RETURN "Cancel":U.
ELSE RETURN.

/* two.p - end of file */

/* This is a kludge for MS-WINDOWS  - see bug # 94-09-16-030.   */
/* When the user dblclicks on a table in the adecomm/_tblsel.p procedure this dialog
   (_mfldsel.p) is brought into context on the MOUSE-SELECT-DOWN of the 2nd click.
   However, the MOUSE-SELECT-UP of the second click "bleeds" through to the underlying
   frame of the UIB design window and attempts to fire the frame-select-up persistent
   trigger.  This fails with an error message that it can't find frame-select-up.  By
   putting this procedure in this file it will find it and supress the error message. */
PROCEDURE frame-select-up.
  RETURN.
END.

/* Brynjar - size the frame properly: */
PROCEDURE ExpandFrame:
  DEF VAR hFg     AS HANDLE NO-UNDO.
  DEF VAR hWidget AS HANDLE NO-UNDO.

  ASSIGN FRAME FldPicker:WIDTH-PIXELS  = FRAME FldPicker:WIDTH-PIXELS + 300
         FRAME FldPicker:HEIGHT-PIXELS = FRAME FldPicker:HEIGHT-PIXELS + 250
         .
  hFg = FRAME FldPicker:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hFg):
    hWidget = hFg:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(hWidget):
                
      IF hWidget:X > 135 THEN hWidget:X = hWidget:X + 145.
      IF hWidget:Y > 185 THEN hWidget:Y = hWidget:Y + 250.
      IF CAN-DO("selection-list",hWidget:TYPE) THEN 
        ASSIGN hWidget:WIDTH-PIXELS = hWidget:WIDTH-PIXELS + 145
               hWidget:HEIGHT-PIXELS = hWidget:HEIGHT-PIXELS + 250.

      hWidget = hWidget:NEXT-SIBLING.
    END.
    hFg = hFg:NEXT-SIBLING.
  END.

END PROCEDURE.

/* Brynjar: view field info: */
PROCEDURE ViewFieldInfoRecord:    

  DO WITH FRAME FldPicker:
    IF NUM-ENTRIES(v_SrcLst:SCREEN-VALUE,".") > 2 THEN
      ASSIGN cDbName = ENTRY(1,v_SrcLst:SCREEN-VALUE,".")
             cTableName = ENTRY(2,v_SrcLst:SCREEN-VALUE,".")
             cFieldName = ENTRY(3,v_SrcLst:SCREEN-VALUE,".").
    ELSE IF NUM-ENTRIES(v_SrcLst:SCREEN-VALUE,".") > 1 THEN
      ASSIGN cDbName    = LDBNAME("DICTDB")
             cTableName = ENTRY(1,v_SrcLst:SCREEN-VALUE,".")
             cFieldName = ENTRY(2,v_SrcLst:SCREEN-VALUE,".").
    ELSE 
        ASSIGN cDbName    = LDBNAME("DICTDB")
               cTableName = (IF NUM-ENTRIES(p_TblLst,".") > 1 THEN ENTRY(2,p_TblLst,".") ELSE p_TblLst)
               cFieldName = v_SrcLst:SCREEN-VALUE.

    cFieldName = TRIM(SUBSTRING(cFieldName,1,INDEX(cFieldName," ("))).

    RUN adeuib/_fldinfo.p (INPUT cDbName,
                           INPUT cTableName,
                           INPUT cFieldName,
                           OUTPUT cLabel,
                           OUTPUT cDEF-LABEL-ATTR,
                           OUTPUT cFormat,
                           OUTPUT cDEF-FORMAT-ATTR,
                           OUTPUT cDataType,
                           OUTPUT cHelp,
                           OUTPUT cDEF-HELP-ATTR,
                           OUTPUT extnt,
                           OUTPUT intl,
                           OUTPUT cDesc,
                           OUTPUT cDEF-VALEXP,
                           OUTPUT valmsg,
                           OUTPUT valmsg-sa, 
                           OUTPUT bMANDATORY).

    MESSAGE "Datatype: " "~t" cDataType SKIP
            "Label: " "~t~t" cLabel SKIP
            "Format: " "~t~t" cFormat SKIP
            "Help: " "~t~t" cHelp SKIP
            "Desc:" "~t~t" cDesc
        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Field info: " + cFieldName.
  END.
END PROCEDURE.
