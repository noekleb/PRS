/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*----------------------------------------------------------------------------

File: _ffqdlg.p

Description:
    Freeform Query dialog to set up freeform querys from the 
    query builder.  This "extra" call is necessary because the
    query builder doesn't have access to _query-u-rec (The
    recid of the current _U.

Input Parameters:
   <none>
   
Input-Output Parameters:
   ok-cancel - Character field that is either "Freeform" or "Cancel".

Output Parameters:
   oqcode     - Character field containing the open query statement

Author: D. Ross Hunter

Date Created: 1995

Modified for JukeBox 05.11.13 by brynjar@chemistry.no
Changes for JukeBox moved forward 10.sep.14 to ver 11.4 by Brynjar
08.nov.17 by brynjar for JukeBox
    capable to add query definition just to clipboard and return "exit" which i 
    captured by adeshar\qurytrig.i
---------------------------------------------------------------------------- */

DEF INPUT        PARAMETER _4GLQury  AS CHARACTER                      NO-UNDO.
DEF INPUT        PARAMETER TblLIst   AS CHARACTER                      NO-UNDO.
DEF INPUT-OUTPUT PARAMETER ok-cancel AS CHARACTER                      NO-UNDO.

{adeuib/uniwidg.i}
{adeuib/brwscols.i}
{adeuib/triggers.i}
{adeuib/sharvars.i}
{adeuib/uibhlp.i}
{adecomm/oeideservice.i}

DEF VAR never-again          AS LOGICAL                                NO-UNDO.

/* Added definitions for JukeBox: */
DEF VAR ix                   AS INT  NO-UNDO.
DEF VAR cPrimaryTable        AS CHAR NO-UNDO.
DEF VAR cDisplayFieldDef     AS CHAR NO-UNDO.
DEF VAR cTableFieldDef       AS CHAR NO-UNDO.
DEF VAR cNewDisplayFields    AS CHAR NO-UNDO.
DEF VAR p_fld_label          AS CHAR 		NO-UNDO.
DEF VAR p_fld_label_sa       AS CHAR 		NO-UNDO.
DEF VAR p_fld_format         AS CHAR 		NO-UNDO.
DEF VAR p_fld_format_sa      AS CHAR 		NO-UNDO.
DEF VAR p_fld_type           AS CHAR 		NO-UNDO.
DEF VAR p_fld_help           AS CHAR 		NO-UNDO.
DEF VAR p_fld_help_sa        AS CHAR 		NO-UNDO.
DEF VAR p_fld_extent         AS INTEGER     NO-UNDO.
DEF VAR p_fld_initial        AS CHAR        NO-UNDO.
DEF VAR p_fld_description    AS CHAR        NO-UNDO.
DEF VAR p_fld_valexp         AS CHAR        NO-UNDO.
DEF VAR p_fld_valmsg         AS CHAR        NO-UNDO.
DEF VAR p_fld_valmsg_sa      AS CHAR        NO-UNDO.
DEF VAR p_fld_mandatory      AS LOGICAL     NO-UNDO.
DEF VAR cTtFieldDef          AS CHAR        NO-UNDO.
DEF VAR cField               AS CHAR        NO-UNDO.
DEF VAR cTable               AS CHAR        NO-UNDO.
DEF VAR cFieldList           AS CHAR        NO-UNDO.
DEF VAR cTableList           AS CHAR        NO-UNDO.
DEF VAR cQueryJoin           AS CHAR        NO-UNDO.
DEF VAR cEnableFld           AS CHAR        NO-UNDO.
DEF VAR cTtExt               AS CHAR        NO-UNDO.
DEF VAR cDbExt               AS CHAR        NO-UNDO.
DEF VAR cRowIdentIndex       AS CHAR        NO-UNDO.
DEF VAR cTips                AS CHAR        NO-UNDO.
DEF VAR cFileName            AS CHAR        NO-UNDO.
DEF VAR bOutputToFile        AS LOG         NO-UNDO INIT YES.
DEF VAR iExt                 AS INT         NO-UNDO.
DEF VAR iy                   AS INT         NO-UNDO.
DEF VAR cInitObjCode         AS CHAR        NO-UNDO.
DEF VAR cCode1               AS CHAR NO-UNDO.
DEF VAR cCode2               AS CHAR NO-UNDO.
DEF VAR cProcList            AS CHAR NO-UNDO.
DEF VAR rOpenQuery           AS ROWID NO-UNDO.
DEF VAR rDisplay             AS ROWID NO-UNDO.

DEF STREAM JBoxStream.

DEF TEMP-TABLE ttTableFlds
    FIELD cTable      AS CHAR
    FIELD cTtFld      AS CHAR
    FIELD cDbFld      AS CHAR
    FIELD cTtExtent   AS CHAR
    FIELD cDbExtent   AS CHAR
    FIELD cDataType   AS CHAR
    FIELD cDisplayDef AS CHAR
    FIELD iSeq        AS INT.

/* End JukeBox definitions */

if OEIDE_CanLaunchDialog() THEN DO:
  IF NOT DYNAMIC-FUNCTION("getNewJukeBoxObject") THEN    
   RUN adeuib/ide/_dialog_advisor.p (
  /* TEXT */    INPUT "Freeform queries are edited using the Section Editor." +
                      "  This is an advanced feature, press 'Help' for details.",
  /* OPTIONS */ INPUT "Freeform.  Allow freeform editing of query.,Freeform," +
/*                       "JukeBox freeform query definition (temp-table definition added to clipboard),JukeBox," + /* Added JukeBox option */ */
                      "Cancel.  Use Query Builder.,Cancel",
                INPUT FALSE,
                INPUT "AB",
                INPUT {&Free_Form_Query_Dlg_Box},
                INPUT-OUTPUT ok-cancel,
                OUTPUT never-again).
  ELSE OK-CANCEL = "JukeBox".
END.
else
     RUN adeuib/_advisor.w (
     /* TEXT */    INPUT "Freeform queries are edited using the Section Editor." +
                      "  This is an advanced feature, press 'Help' for details.",
    /* OPTIONS */ INPUT "Freeform.  Allow freeform editing of query.,Freeform," +
                        "JukeBox freeform query definition (temp-table definition added to clipboard),JukeBox," + /* Added JukeBox option */
                        "Cancel.  Use Query Builder.,Cancel",
                   INPUT FALSE,
                   INPUT "AB",
                   INPUT {&Free_Form_Query_Dlg_Box},
                   INPUT-OUTPUT ok-cancel,
                   OUTPUT never-again).

IF CAN-DO("Freeform,JukeBox",ok-cancel) THEN DO:

  DEF VAR bOk AS LOG NO-UNDO.
  MESSAGE "Copy view + join-definition to clipboard (for use in JBoxQueryWrapper)?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOK THEN DO:
    RUN genViewDef.
    OK-CANCEL = "exit".
    RETURN.
  END.

  FIND _U WHERE RECID(_U) = _query-u-rec.
  FIND _C WHERE RECID(_C) = _U._x-recid.
  FIND _Q WHERE RECID(_Q) = _C._q-recid. 
  
  FIND _P WHERE _P._window-handle = _U._window-handle.

  ASSIGN _Q._4GLQury = _4GLQury
         _Q._TblList = TblList.

  /* Get the OPEN-QUERY statement that is going to be output into the preprocessor
     section. Note that this will have lines that end with tilde. Remove these
     tilde's. */
     
  RUN adeshar/_coddflt.p (INPUT "_OPEN-QUERY", INPUT _query-u-rec, OUTPUT _4GLQury).
  /* the previous code replaced all ":" with ".* messing with query values  (datetime-tz)
     this assumption is that this is to replace the colon at end as this will not compile (11.4).
     not sure if it is always there so also remove period to avoid double period   
  */
   
  assign  _4GLQury = right-trim(_4GLQury,":.":U) + ".":U  
          _4GLQury = REPLACE(_4GLQury," ~~":U + CHR(10) , CHR(10)).

  IF ok-cancel = "JukeBox" AND _4GLQury NE "" THEN DO:
    ASSIGN cPrimaryTable    = (IF _4GLQury BEGINS "FOR " THEN ENTRY(3,_4GLQury," ") ELSE ENTRY(NUM-ENTRIES(ENTRY(2,_4GLQury," "),"."),"."))
/*           cTtFieldDef      = "/* Code for Definitions: */" + CHR(10)
                            + "DEF VAR oBrw" + cPrimaryTable + " AS JBoxBrowse." + CHR(10) + CHR(10) 
                            + "DEF TEMP-TABLE " + cPrimaryTable + CHR(10) */
           cQueryJoin       = (IF NUM-ENTRIES(_4GLQury,CHR(10)) > 1 THEN SUBSTR(_4GLQury,INDEX(_4GLQury,CHR(10))) ELSE "")
           cQueryJoin       = REPLACE(cQueryJoin,CHR(10),"")
           cQueryJoin       = REPLACE(cQueryJoin,",      EACH",",EACH")
           cQueryJoin       = REPLACE(cQueryJoin,",      FIRST",",FIRST")
           cQueryJoin       = REPLACE(cQueryJoin,",      LAST",",LAST")
           cQueryJoin       = REPLACE(cQueryJoin,"INDEXED-REPOSITION.","")
           cQueryJoin       = TRIM(cQueryJoin)
           _4GLQury         = ENTRY(1,_4GLQury)
           .

    cTableList = cPrimaryTable.
    DO ix = 1 TO NUM-ENTRIES(cQueryJoin):
      IF CAN-DO("EACH,FIRST,LAST",ENTRY(1,ENTRY(ix,cQueryJoin)," ")) THEN
        cTableList = cTableList + "," + ENTRY(2,ENTRY(ix,cQueryJoin)," ").
    END.
  END.
  ELSE IF ok-cancel = "JukeBox" AND _4GLQury = "" THEN DO:
    MESSAGE "No tables are selected" VIEW-AS ALERT-BOX WARNING.
    ok-cancel = "Freeform".
  END.

  CREATE _TRG.
  ASSIGN _TRG._pRECID   = RECID(_P)
         _TRG._tSECTION = "_CONTROL":U
         _TRG._tEVENT   = "OPEN_QUERY":U
         _TRG._wRECID   = _query-u-rec
         _TRG._tSPECIAL = "_OPEN-QUERY":U
         _TRG._tCODE    = IF _4GLQury NE "":U THEN 
                          (IF _4GLQury BEGINS "OPEN QUERY":U
                              THEN _4GLQury
                              ELSE "OPEN QUERY ~{&SELF-NAME} ":U + _4GLQury)
                          ELSE "OPEN QUERY ~{&SELF-NAME}":U + 
                               " FOR EACH <record-phrase>.":U.
  rOpenQuery = ROWID(_TRG).
  
  IF VALID-HANDLE(_P._tv-proc) THEN
    RUN addCodeNode IN _P._tv-proc( _TRG._tSection, RECID(_U), _TRG._tEvent).

  IF _U._TYPE = "BROWSE":U THEN DO:
    CREATE _TRG.
    ASSIGN _TRG._pRECID   = RECID(_P)
           _TRG._tSECTION = "_CONTROL":U
           _TRG._tEVENT   = "DISPLAY":U
           _TRG._wRECID   = _query-u-rec
           _TRG._tSPECIAL = "_DISPLAY-FIELDS":U
           _TRG._tCODE    = "      ":U.
    RUN adeshar/_coddflt.p (INPUT "_DISPLAY-FIELDS", INPUT _query-u-rec, OUTPUT _TRG._tCODE).
    
    rDisplay = ROWID(_TRG).

    /* JukeBox option selected: */
    IF ok-cancel = "JukeBox" THEN DO:
      cTtFieldDef  = "/*** Start instance property definitions for JBoxBrowse object oBrw" + cPrimaryTable + " ***/" + CHR(10)
                   + "DEF VAR oBrw" + cPrimaryTable + " AS JBoxBrowse NO-UNDO." + CHR(10) + CHR(10) 
                   + "DEF TEMP-TABLE " + cPrimaryTable + CHR(10)
                   .

      DO ix = 1 TO NUM-ENTRIES(_TRG._tCODE,CHR(10)):
        cDisplayFieldDef = TRIM(ENTRY(ix,_TRG._tCODE,CHR(10))).
        IF NUM-ENTRIES(cDisplayFieldDef,".") > 1 THEN DO:
          ASSIGN cTable = TRIM(ENTRY(1,cDisplayFieldDef,"."))
                 cField = TRIM(ENTRY(1,ENTRY(2,cDisplayFieldDef,".")," "))
                 .
          IF SUBSTR(cField,LENGTH(cField)) = "]" THEN
            ASSIGN cDbExt = SUBSTR(cField,INDEX(cField,"["))
                   cTtExt = "_" + REPLACE(SUBSTR(cDbExt,2),"]","")
                   cField  = SUBSTR(cField,1,INDEX(cField,"[") - 1)
                   .
          ELSE 
            ASSIGN cDbExt = ""
                   cTtExt = "".

          RUN adeuib\_fldinfo.p(ldbname("DICTDB":U)
                                ,cTable
                                ,cField
                                ,OUTPUT p_fld_label      
                                ,OUTPUT p_fld_label_sa   
                                ,OUTPUT p_fld_format     
                                ,OUTPUT p_fld_format_sa  
                                ,OUTPUT p_fld_type       
                                ,OUTPUT p_fld_help       
                                ,OUTPUT p_fld_help_sa    
                                ,OUTPUT p_fld_extent     
                                ,OUTPUT p_fld_initial    
                                ,OUTPUT p_fld_description
                                ,OUTPUT p_fld_valexp     
                                ,OUTPUT p_fld_valmsg     
                                ,OUTPUT p_fld_valmsg_sa  
                                ,OUTPUT p_fld_mandatory
                                ).

          CREATE ttTableFlds.
          ASSIGN ttTableFlds.cTable      = cTable
                 ttTableFlds.cTtFld      = cField
                 ttTableFlds.cDbFld      = cField
                 ttTableFlds.cDbExtent   = cDbExt
                 ttTableFlds.cTtExtent   = cTtExt
                 ttTableFlds.cDataType   = p_fld_type
                 ttTableFlds.cDisplayDef = SUBSTR(cDisplayFieldDef,INDEX(cDisplayFieldDef," ")) + " LABEL '" + p_fld_label + "'"
                 ttTableFlds.iSeq        = ix
                 .
          IF CAN-DO(cFieldList,cField) AND LOOKUP(cTable,cTableList) > 1 AND cDbExt = "" THEN
            ttTableFlds.cTtFld = ttTableFlds.cTtFld + STRING(LOOKUP(cTable,cTableList)).

          cFieldList = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + cField.
        END.
      END.

      RUN BuildTtAndQueryDefintion("Browse").

      /*
      FIND LAST _U WHERE _U._NAME BEGINS "BROWSE-"
           NO-ERROR.
      IF AVAIL _U THEN 
        ASSIGN _U._NAME  = "brw" + cPrimaryTable + "_AssignedByJbox"
               .
      */

      
      IF cFileName = "" OR cFileName = ? THEN
        cFileName = SESSION:TEMP-DIR + cPrimaryTable + "_JukeBox_BrowseDef.txt".

      IF SEARCH(cFileName) NE ? THEN
        cFileName = REPLACE(cFileName,"_JukeBox_BrowseDef.txt","_JukeBox_BrowseDef" + STRING(TIME) + ".txt").

      MESSAGE "Code for additional definitions for JukeBox static browse was created successfully" SKIP(1)
              "Open textfile containing the code and tips for extensions?" SKIP
              "(No: Add just the relevant pieces to Definitions/InitializeObject)" 
              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOutputToFile.
    
      IF bOutputToFile THEN DO:
        OUTPUT STREAM JBoxStream TO VALUE(cFileName).
        PUT STREAM JBoxStream UNFORMATTED cTips + CHR(10) cTtFieldDef.
        OUTPUT STREAM JBoxStream CLOSE.
        OS-COMMAND NO-WAIT VALUE("notepad " + cFileName).
      END.
/*       ELSE CLIPBOARD:VALUE = cTtFieldDef. */
    
      _TRG._tCODE = cNewDisplayFields + CHR(10) + cEnableFld.

      DELETE _TRG.
      FIND _TRG WHERE ROWID(_TRG) = rOpenQuery.
      DELETE _TRG.
    END. /* JukeBox option */

    /* Not necessary yet 
    IF VALID-HANDLE(_P._tv-proc) THEN
      RUN addCodeNode IN _P._tv-proc( _TRG._tSection, RECID(_U), _TRG._tEvent).
    */

    /* Remove _BC records and their triggers */
    FOR EACH _BC WHERE _BC._x-recid = _query-u-rec:
      FOR EACH _TRG WHERE _TRG._wRECID = RECID(_BC):
        DELETE _TRG.
      END.
      DELETE _BC.
    END.  /* FOR EACH _BC */
  END. /* End browse */
  
  /* JukeBox query definition */
  ELSE IF ok-cancel = "JukeBox" THEN DO:
    cTtFieldDef  = "/* Code for Definitions: */" + CHR(10) + CHR(10)
                 + "/*** Start instance property definitions for JBoxQuery object oQry" + cPrimaryTable + " ***/" + CHR(10)
                 + "DEF VAR oQry" + cPrimaryTable + " AS JBoxQuery NO-UNDO." + CHR(10) + CHR(10) 
                 + "DEF TEMP-TABLE " + cPrimaryTable + CHR(10)
                 .

    DEFINE QUERY qDb FOR DICTDB._db   FIELDS(), 
                         DICTDB._file FIELDS(), 
                         DICTDB._field. 
    
    DO ix = 1 TO NUM-ENTRIES(cTableList):

      OPEN QUERY qDB FOR 
           EACH DICTDB._db  
           WHERE DICTDB._db._db-name = ? 
           NO-LOCK,
           EACH DICTDB._file OF DICTDB._db 
                WHERE DICTDB._file._file-name = ENTRY(ix,cTableList)
/*                WHERE CAN-DO(cTableList,DICTDB._file._file-name) */
                AND   LOOKUP(DICTDB._FILE._OWNER,"PUB,_FOREIGN":U) > 0 
                NO-LOCK,
           EACH DICTDB._field OF DICTDB._file 
                NO-LOCK.
  
      GET FIRST qDB.
      REPEAT WHILE NOT QUERY qDB:QUERY-OFF-END:
        
        ASSIGN cTable = DICTDB._file._file-name
               cField = DICTDB._field._field-name
               iy     = iy + 1
               .
  
        IF DICTDB._field._Extent > 0 THEN
          DO iExt = 1 TO DICTDB._field._Extent:
            CREATE ttTableFlds.
            ASSIGN ttTableFlds.cTable     = cTable
                   ttTableFlds.cTtFld     = cField
                   ttTableFlds.cDbFld     = cField
                   ttTableFlds.cDbExtent  = "[" + STRING(iExt) + "]"
                   ttTableFlds.cTtExtent  = "_" + STRING(iExt)
                   ttTableFlds.cDataType  = DICTDB._field._data-type
                   ttTableFlds.iSeq       = iy
                   iy = iy + 1
                   .
          END.
        ELSE DO:
          CREATE ttTableFlds.
          ASSIGN ttTableFlds.cTable     = cTable
                 ttTableFlds.cTtFld     = cField
                 ttTableFlds.cDbFld     = cField
                 ttTableFlds.cDataType  = DICTDB._field._data-type
                 ttTableFlds.iSeq       = iy
                 .
        END.
    
        IF CAN-DO(cFieldList,cField) AND LOOKUP(cTable,cTableList) > 1 AND DICTDB._field._Extent = 0 THEN
            ttTableFlds.cTtFld = ttTableFlds.cTtFld + STRING(LOOKUP(cTable,cTableList)).
  
        cFieldList = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + cField.
        GET NEXT qDB.
      END.
    END.
    RUN BuildTtAndQueryDefintion("Query").

    IF cFileName = "" OR cFileName = ? THEN
      cFileName = SESSION:TEMP-DIR + cPrimaryTable + "_JukeBox_QueryDef.txt".

    IF SEARCH(cFileName) NE ? THEN
      cFileName = REPLACE(cFileName,"_JukeBox_QueryDef.txt","_JukeBox_QueryDef" + STRING(TIME) + ".txt").

    MESSAGE "Code for additional definitions for JukeBox static query was created successfully" SKIP(1)
            "Open textfile containing the code and tips for extensions?" SKIP
            "(No: Add just the relevant pieces to Definitions/InitializeObject)" SKIP(1)
            "Note: A JukeBox query is always freeform and must be maintained by editing the source code"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE bOutputToFile.

  END.

END.

IF ok-cancel = "JukeBox" THEN DO:

  IF bOutputToFile THEN DO:
    OUTPUT STREAM JBoxStream TO VALUE(cFileName).
    PUT STREAM JBoxStream UNFORMATTED 
               cTips + CHR(10) cTtFieldDef + CHR(10)
             + "/* End Definitions */" + CHR(10) + CHR(10)
             + "/* Code for InitializeObject (in DO WITH FRAME.. block): */" + CHR(10)
	     + cInitObjCode
	     .
    OUTPUT STREAM JBoxStream CLOSE.
    OS-COMMAND NO-WAIT VALUE("notepad " + cFileName).
  END.

  /* Insert the code for Definitions */
  FIND FIRST _TRG
       WHERE _TRG._tSECTION = "_CUSTOM":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._tEVENT   = "_DEFINITIONS":U
       NO-ERROR.
  IF AVAIL _TRG THEN 
    _TRG._tCODE = _TRG._tCODE + CHR(10) + cTtFieldDef.        	

  /* Insert the code for InitializeObject */
  FIND FIRST _TRG
       WHERE _TRG._tSECTION = "_PROCEDURE":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._tEVENT   = "InitializeObject":U
       NO-ERROR.

  IF NOT AVAIL _TRG THEN DO:
    FOR EACH _TRG 
        WHERE _TRG._tSECTION = "_PROCEDURE":U
          AND _TRG._pRECID   = RECID(_P):
      cProcList = cProcList + (IF cProcList NE "" THEN "|" ELSE "") + _TRG._tEvent.
    END.
    IF cProcList NE "" THEN DO:
      MESSAGE "Procedure InitializeObject is missing from the container" SKIP 
              "Select procedure to insert code for initialization of object"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      JBoxSession:Instance:SimpleSelectList(cProcList).
      IF JBoxSession:Instance:SelectListOk THEN
        FIND FIRST _TRG
             WHERE _TRG._tSECTION = "_PROCEDURE":U
               AND _TRG._pRECID   = RECID(_P)
               AND _TRG._tEVENT   = JBoxSession:Instance:SelectListValue
             NO-ERROR.
    END.
  END.
  IF AVAIL _TRG THEN DO:
    IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
      _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObjCode + CHR(10) + "END.").              
    ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
      ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
             cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
             cCode2 = REPLACE(cCode2,"END.",cInitObjCode + CHR(10) + "END." + CHR(10))
             _TRG._tCODE = cCode1 + cCode2
             .
    ELSE 
      _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObjCode.         
  END.
   
/*
  IF AVAIL _TRG AND _TRG._tCODE MATCHES "*END.*" THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObjCode + CHR(10) + "END.").        	
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObjCode.        	
*/

  ok-cancel = "Freeform". /* Set back to "Freeform" for AppBuilder compliance */
END.

PROCEDURE BuildTtAndQueryDefintion:
    DEF INPUT PARAM icType AS CHAR NO-UNDO.

    FOR EACH ttTableFlds BY ttTableFlds.iSeq:
      ASSIGN cDisplayFieldDef  = cPrimaryTable + "." + ttTableFlds.cTtFld + ttTableFlds.cTtExtent + ttTableFlds.cDisplayDef
             cTtFieldDef       = cTtFieldDef + "    FIELD " + ttTableFlds.cTtFld + ttTableFlds.cTtExtent + " AS " + ttTableFlds.cDataType + CHR(10)
             cNewDisplayFields = cNewDisplayFields + (IF cNewDisplayFields NE "" THEN CHR(10) ELSE "") + cDisplayFieldDef
             .
      IF ttTableFlds.cTtExtent NE "" THEN
        cTtFieldDef  = cTtFieldDef + "    FIELD jbextent" + ttTableFlds.cTtExtent + "_" + ttTableFlds.cTtFld + " AS " + ttTableFlds.cDataType + " /* placeholder for calculation */" + CHR(10).
    
      IF cEnableFld = "" THEN cEnableFld = "ENABLE " + cPrimaryTable + "." + ttTableFlds.cTtFld + ttTableFlds.cTtExtent. 
    END.
    DO ix = 1 TO NUM-ENTRIES(cTableList):
      cTableFieldDef = cTableFieldDef + (IF cTableFieldDef NE "" THEN CHR(10) + "    + '," ELSE CHR(10) + "      '") + ENTRY(ix,cTableList) + "'".
      IF icType = "browse" THEN
        FOR EACH ttTableFlds
            WHERE ttTableFlds.cTable = ENTRY(ix,cTableList)
            BY iSeq:
          cTableFieldDef = cTableFieldDef + CHR(10) + "      + ';" + ttTableFlds.cDbFld + ttTableFlds.cDbExtent + "'".
        END.
      cTtFieldDef  = cTtFieldDef + "    FIELD RowIdent" + STRING(ix) + " AS CHARACTER " + CHR(10).
      cRowIdentIndex = cRowIdentIndex + " RowIdent" + STRING(ix).  
    END.

    CLIPBOARD:VALUE = cTableFieldDef.

    cTtFieldDef = cTtFieldDef 
                + "    FIELD RowCount AS INTEGER" + CHR(10)
                + (IF icType = "browse" THEN
                    "    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1" + CHR(10)
                  + "    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'" + CHR(10)
                   ELSE "")
                + "    INDEX idxRowids " + cRowIdentIndex + CHR(10)
                + "    ." + CHR(10) 
		+ "DEFINE BUFFER v_" + cPrimaryTable + " FOR " + cPrimaryTable + "." + CHR(10) + CHR(10)
                + (IF icType = "query" THEN
                    "FUNCTION getTableHandleQry" + cPrimaryTable + " RETURNS HANDLE()." + CHR(10)
                  + "  RETURN BUFFER " + cPrimaryTable + ":HANDLE:TABLE-HANDLE." + CHR(10)
                  + "END FUNCTION." + CHR(10)
                   ELSE "")
                + "FUNCTION getBuffersAndFields"
                  + (IF icType = "browse" THEN "Brw" ELSE "Qry") 
                  + cPrimaryTable + " RETURNS CHARACTER()." + CHR(10)
                + "  RETURN " + TRIM(cTableFieldDef,",") + "." + CHR(10)
                + "END FUNCTION." + CHR(10)
                + "FUNCTION getQueryJoin" 
                  + (IF icType = "browse" THEN "Brw" ELSE "Qry") 
                  + cPrimaryTable + " RETURNS CHARACTER()." + CHR(10)
                + "  RETURN '" + cQueryJoin + "'." + CHR(10)
                + "END FUNCTION." + CHR(10)
                + "/*** End instance property settings for JBox" + icType + " object " + (IF icType = "query" THEN "oQry" ELSE "oBrw") + cPrimaryTable + " ***/" + CHR(10)
		.
    cInitObjCode =
                  (IF icType = "browse" THEN
                    "  oBrw" + cPrimaryTable + " = NEW JBoxBrowse(brw" + cPrimaryTable + ":HANDLE)." + CHR(10)
                   ELSE
                    "  oQry" + cPrimaryTable + " = NEW JBoxQuery('" + cPrimaryTable + "')." + CHR(10)
                   )
                  .

    IF icType = "browse" THEN
      cTips     = "/********************************************************************"  + CHR(10)
                + "This code is generated to support a static browse with primary table " + cPrimaryTable + CHR(10)
                + "in a JukeBox browse class. The first section should be copied to Definitions" + CHR(10)
                .
    ELSE
      cTips     = "/********************************************************************"  + CHR(10)
                + "This code is generated to support a static query with primary table " + cPrimaryTable + CHR(10)
                + "in a JukeBox query class. The first section should be copied to Definitions" + CHR(10)
                .
    cTips = cTips 
                + "You may edit the code:" + CHR(10)
                + "  - To add support for multiple joins to the same table" + CHR(10)
                + "    add 'buf[1-9]_<table name>' with corresponding fields to the getBuffersAndFields function" + CHR(10)
                + "    and also the 'buf[1-9]_<table name>' to getQueryJoin" + CHR(10)
                + "    Fields from the new table must also be added to the temp-table definition and browse display trigger:" + CHR(10)
                + "    - If the field is duplicate it must get the suffix representing the sequence of the added" + CHR(10)
                + "      table in the table-sequence, f,ex RepName4 if the BuffersAndFields looks like this: " + CHR(10)
                + "      Customer;CustNum;SalesRep,SalesRep;Repname,Order;OrderNum;SalesRep,buf1_SalesRep;RepName" + CHR(10)
                + "      (note: The suffix must not be added in the list of BuffersAndFields!)" + CHR(10)
                + "    FIELD RepName4 AS CHAR" + CHR(10) + CHR(10)
                + "  - To add a calculated field: " + CHR(10)
                + "    Add your new field to the temp-table definition" + (IF icType = "browse" THEN " (and display-trigger)." ELSE ".") + CHR(10)
                + "    Add it also to BuffersAndFields after the buffer-name from where it should " + CHR(10)
                + "    get it's ROWID parameter and/or field value parameter." + CHR(10)
                + "    (You can also set the parameter as a constant using the CalcFieldParam property.)" + CHR(10) + CHR(10)
                + "    Sample field def:" + CHR(10)
                + "      + ';+CustOrderTotal|DECIMAL||CustOrderTotal(CustNum)'" + CHR(10)
                + "    Assign the name of the .p procedure that contains CustOrderTotal to the browse/query object:" + CHR(10)
                + '      oBrwCustomer:calcFieldProc = "customer_browsecalc.p."' + CHR(10)
                + "*********************************************************************/" + CHR(10) + CHR(10)
		+ "/* Code for Definitions: */" + CHR(10) + CHR(10)
                . 

END PROCEDURE.


PROCEDURE genViewDef:

DEF VAR cDisplTrg         AS CHAR NO-UNDO.
DEF VAR cTtFieldDef       AS CHAR NO-UNDO.
DEF VAR cViewDef          AS CHAR NO-UNDO.
DEF VAR cFldName          AS CHAR NO-UNDO.
DEF VAR cDbExt            AS CHAR NO-UNDO.
DEF VAR cTtExt            AS CHAR NO-UNDO.
DEF VAR cTblList          AS CHAR NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iy                AS INT  NO-UNDO.
DEF VAR cRowIdentIdx      AS CHAR NO-UNDO.
DEF VAR cCurrDb           AS CHAR NO-UNDO.
DEF VAR cFldList          AS CHAR NO-UNDO.
DEF VAR cQueryJoin        AS CHAR NO-UNDO.
DEF VAR cTbl              AS CHAR NO-UNDO.
DEF VAR cCalcProcs        AS CHAR NO-UNDO.
DEF VAR cCalcFld          AS CHAR NO-UNDO.
DEF VAR cCalcProcFlds     AS CHAR NO-UNDO.
DEF VAR cCalcProcFunc     AS CHAR NO-UNDO.
DEF VAR cCustomCalcProcs  AS CHAR NO-UNDO.
DEF VAR bFreeform         AS LOG  NO-UNDO.
DEF VAR cInitObjCode      AS CHAR NO-UNDO.
DEF VAR cCode1            AS CHAR NO-UNDO.
DEF VAR cCode2            AS CHAR NO-UNDO.
DEF VAR cInitProc         AS CHAR NO-UNDO.
DEF VAR cFirstTable       AS CHAR NO-UNDO.


FIND _P WHERE _P._WINDOW-HANDLE = _h_win NO-ERROR.
FIND _U WHERE RECID(_U) = _query-u-rec NO-ERROR.
FIND _C WHERE RECID(_C) = _U._x-recid NO-ERROR.
FIND _Q WHERE RECID(_Q) = _C._q-recid NO-ERROR.

IF NOT AVAIL _P OR NOT AVAIL _U OR NOT AVAIL _C OR NOT AVAIL _Q THEN RETURN.

cCurrDb = IF _db_name NE "" THEN _db_name ELSE LDBNAME("DICTDB").

assign  _4GLQury = right-trim(_4GLQury,":.":U) + ".":U  
        _4GLQury = REPLACE(_4GLQury," ~~":U + CHR(10) , CHR(10)).


IF _4GLQury NE "" THEN DO:
  ASSIGN cPrimaryTable    = (IF _4GLQury BEGINS "FOR " THEN ENTRY(3,_4GLQury," ") ELSE ENTRY(NUM-ENTRIES(ENTRY(2,_4GLQury," "),"."),"."))
         cQueryJoin       = (IF NUM-ENTRIES(_4GLQury,CHR(10)) > 1 THEN SUBSTR(_4GLQury,INDEX(_4GLQury,CHR(10))) ELSE "")
         cQueryJoin       = REPLACE(cQueryJoin,CHR(10),"")
         cQueryJoin       = REPLACE(cQueryJoin,",      EACH",",EACH")
         cQueryJoin       = REPLACE(cQueryJoin,",      FIRST",",FIRST")
         cQueryJoin       = REPLACE(cQueryJoin,",      LAST",",LAST")
         cQueryJoin       = REPLACE(cQueryJoin,"INDEXED-REPOSITION.","")
         cQueryJoin       = TRIM(cQueryJoin)
         _4GLQury         = ENTRY(1,_4GLQury)
         .

  cTableList = cPrimaryTable.
  DO ix = 1 TO NUM-ENTRIES(cQueryJoin):
    IF CAN-DO("EACH,FIRST,LAST",ENTRY(1,ENTRY(ix,cQueryJoin)," ")) THEN
      cTableList = cTableList + "," + ENTRY(2,ENTRY(ix,cQueryJoin)," ").
  END.
END.

RUN adeshar/_coddflt.p (INPUT "_DISPLAY-FIELDS", INPUT _query-u-rec, OUTPUT cDisplTrg).

cTblList = cTableList.

DO ix = 1 TO NUM-ENTRIES(cTblList):
  cTbl = ENTRY(NUM-ENTRIES(ENTRY(1,ENTRY(ix,cTblList)," "),"."),ENTRY(1,ENTRY(ix,cTblList)," "),".").
  IF cFirstTable = "" THEN cFirstTable = cTbl.

  cViewDef = cViewDef + CHR(10) + "  " + (IF ix = 1 THEN "  '" ELSE "+ ',") + cTbl + "'".
  FOR EACH _BC 
      WHERE _BC._x-recid = RECID(_U)
        AND _BC._TABLE   = cTbl
      BY _BC._SEQUENCE:
    cViewDef = cViewDef + CHR(10) + "     + ';" + _BC._NAME + "'". 
    IF CAN-DO(cFldList,_BC._NAME) THEN 
      ASSIGN _BC._DEF-VALEXP = _BC._NAME + STRING(ix)
             _BC._DISP-NAME  = _BC._DISP-NAME + STRING(ix).
    ELSE _BC._DEF-VALEXP = _BC._NAME.
    cFldList = cFldList + (IF cFldList NE "" THEN "," ELSE "") + _BC._DEF-VALEXP.
  END.
  FOR EACH _BC 
      WHERE _BC._x-recid = RECID(_U)
        AND _BC._TABLE   = ?
        AND _BC._DBNAME  = "_<CALC>" 
        AND ENTRY(1,_BC._DISP-NAME,".") = cTbl
        AND _BC._DEF-VALEXP NE ""
      BY _BC._SEQUENCE:
    cCalcFld = _BC._DEF-VALEXP.

    cCalcFld = ENTRY(NUM-ENTRIES(cCalcFld,"|"),cCalcFld,"|").

    cViewDef = cViewDef + CHR(10) + "     + ';+" + ENTRY(NUM-ENTRIES(ENTRY(1,_BC._DISP-NAME),"."),ENTRY(1,_BC._DISP-NAME," "),".")
             + "|" + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHARACTER")
             + "||" + cCalcFld + "|" + _BC._LABEL + "'"
             . 
  END.
END.

FOR EACH _BC 
    WHERE _BC._x-recid   = RECID(_U)
       BY _BC._SEQUENCE:         

  IF _BC._DEF-VALEXP NE "" AND SUBSTR(_BC._DEF-VALEXP,LENGTH(_BC._DEF-VALEXP)) = "]" THEN
    ASSIGN cDbExt    = SUBSTR(_BC._DEF-VALEXP,INDEX(_BC._DEF-VALEXP,"["))
           cTtExt    = "_" + REPLACE(SUBSTR(cDbExt,2),"]","")
           cFldName  = SUBSTR(_BC._DEF-VALEXP,1,INDEX(_BC._DEF-VALEXP,"[") - 1)
          .
  ELSE IF _BC._DEF-VALEXP = "" OR (_BC._TABLE = ? AND _BC._DBNAME = "_<CALC>") THEN DO:
    ASSIGN cFldName = ENTRY(NUM-ENTRIES(ENTRY(1,_BC._DISP-NAME," "),"."),ENTRY(1,_BC._DISP-NAME," "),".") 
           cDbExt   = ""
           cTtExt   = ""
           .
  END.
  ELSE 
    ASSIGN cFldName = _BC._DEF-VALEXP
           cDbExt   = ""
           cTtExt   = ""
           .
/*
  IF _BC._VISIBLE THEN
    cDisplTrg = cDisplTrg + (IF cDisplTrg NE '' THEN CHR(10) ELSE '')
              + cFirstTable + '.' + cFldName + cTtExt
              + (IF _BC._FORMAT NE "" AND _BC._FORMAT NE ? THEN ' FORMAT "' + _BC._FORMAT + '"' 
                 ELSE IF _BC._DEF-FORMAT NE "" AND _BC._DEF-FORMAT NE ? THEN ' FORMAT "' + _BC._DEF-FORMAT + '"'
                 ELSE "")
              + (IF _BC._LABEL  NE "" AND _BC._LABEL  NE ? THEN ' LABEL "'  + _BC._LABEL  + '"' 
                 ELSE IF _BC._DEF-LABEL  NE "" AND _BC._DEF-LABEL  NE ? THEN ' LABEL "'  + _BC._DEF-LABEL  + '"' 
                 ELSE "")
              + (IF _BC._WIDTH NE 0 AND _BC._WIDTH NE ? THEN " WIDTH " + REPLACE(STRING(_BC._WIDTH),",",".") ELSE "")
              .
  cTtFieldDef = cTtFieldDef + "    FIELD " + cFldName + cTtExt
              + " AS " + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHAR") + CHR(10)
              .
  IF cTtExt NE "" THEN
    cTtFieldDef  = cTtFieldDef + "    FIELD jbextent" + cTtExt + "_" + cFldName + " AS " + _BC._DATA-TYPE + " /* placeholder for calculation */" + CHR(10).
*/    
  DELETE _BC.
END.

ASSIGN /* cQueryJoin  = (IF NUM-ENTRIES(_4GLQury,CHR(10)) > 1 THEN SUBSTR(_4GLQury,INDEX(_4GLQury,CHR(10))) ELSE "") */
       cQueryJoin  = REPLACE(cQueryJoin,cCurrDb + ".","")
       .
REPEAT WHILE INDEX(cQueryJoin,"  ") > 0:
  cQueryJoin = REPLACE(cQueryJoin,"  "," ").
END.


ASSIGN cQueryJoin       = REPLACE(cQueryJoin,CHR(10),"")
       cQueryJoin       = REPLACE(cQueryJoin,", EACH",",EACH")
       cQueryJoin       = REPLACE(cQueryJoin,", FIRST",",FIRST")
       cQueryJoin       = REPLACE(cQueryJoin,", LAST",",LAST")
       cQueryJoin       = REPLACE(cQueryJoin," NO-LOCK, NO-LOCK,"," NO-LOCK,")
       cQueryJoin       = REPLACE(cQueryJoin,"INDEXED-REPOSITION","")
       cQueryJoin       = REPLACE(cQueryJoin," NO-LOCK  NO-LOCK"," NO-LOCK")
       cQueryJoin       = TRIM(cQueryJoin)
       cQueryJoin       = DYNAMIC-FUNCTION("FixQueryOperators",cQueryJoin)
       cQueryJoin       = DYNAMIC-FUNCTION("FixQueryJoin",cQueryJoin)
      .

CLIPBOARD:VALUE = cViewDef + CHR(10) + "     ,'," + cQueryJoin + "'".

END PROCEDURE.
