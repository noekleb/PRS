 
 /*------------------------------------------------------------------------
    File        : JBoxBrowse
    Purpose     : 
    Syntax      : 
    Description : OO Wrapper for library function NewBrowse
    Author(s)   : brynjar
    Created     : Wed Oct 02 10:47:14 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxBrowse: 
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF VAR hSourceProc       AS HANDLE NO-UNDO.
  DEF VAR bOk               AS LOG    NO-UNDO.
  DEF VAR bTryFilter        AS LOG    NO-UNDO. /* Clear the filter if no records were found */
  DEF VAR bNoExecFilter     AS LOG    NO-UNDO.
  DEF VAR cLastFilter       AS CHAR   NO-UNDO.
  DEF VAR cInitAttr         AS CHAR   NO-UNDO.
  DEF VAR cSortExpr         AS CHAR   NO-UNDO.
  DEF VAR cQueryJoin        AS CHAR   NO-UNDO.
  DEF VAR cBufferList       AS CHAR   NO-UNDO.
  DEF VAR cBuffersAndFields AS CHAR   NO-UNDO.
  DEF VAR oToolbar          AS JBoxToolbar   NO-UNDO.
  DEF VAR oPopupMenu        AS JBoxPopupMenu NO-UNDO.
  DEF VAR cSearchColumn     AS CHAR   NO-UNDO.
  DEF VAR hColumnFieldMap   AS HANDLE NO-UNDO.  
  DEF VAR hBrowse           AS HANDLE NO-UNDO.
  DEF VAR cCallFromMethod   AS CHAR   NO-UNDO.

  DEF TEMP-TABLE ttOverlay
      FIELD oOverlay      AS Progress.Lang.Object
      FIELD cType         AS CHAR
      FIELD cBrowseColumn AS CHAR
      FIELD cBufferColumn AS CHAR 
      FIELD hOverlay      AS HANDLE
      FIELD bInitRefrRow  AS LOG
      FIELD iSeq          AS INT
      .

  DEF PUBLIC PROPERTY DESIGN-WIDGET-HANDLE AS HANDLE GET. PROTECTED SET.
  DEF PUBLIC PROPERTY BROWSE-HANDLE        AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY QUERY-HANDLE         AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY BUFFER-HANDLE        AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY TEMP-TABLE-HANDLE    AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY PARENT-BROWSE-OBJECT AS JBoxBrowse  GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY PARENT-QUERY-OBJECT  AS JBoxQuery  GET. PROTECTED SET.
   
  DEF PUBLIC PROPERTY PARENT-BUFFER-HANDLE AS HANDLE 
      GET():
         DEF VAR hParent AS HANDLE NO-UNDO.
         hParent = DYNAMIC-FUNCTION("getLinkedObject",BROWSE-HANDLE,"parent","from").
         IF VALID-HANDLE(hParent) THEN DO:
           IF hParent:TYPE = "browse" THEN RETURN hParent:QUERY:GET-BUFFER-HANDLE(1).
           ELSE RETURN hParent:GET-BUFFER-HANDLE(1).
         END.  
         ELSE RETURN ?.  
      END GET.
      PROTECTED SET.
      
  DEF PUBLIC PROPERTY SEARCH-FIELD-HANDLE  AS HANDLE 
      GET():
        RETURN DYNAMIC-FUNCTION("GetLinkedObject",BROWSE-HANDLE,"browse-search-field","from").
      END GET.   
      PROTECTED SET.

  {JBoxBrowseQueryProp.i BROWSE-HANDLE}
   
  DEF PUBLIC PROPERTY noColumnSort AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"noColumnSort"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"noColumnSort",icAttr).
      END SET.

  DEF PUBLIC PROPERTY allViewFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"allViewFields"). END GET.
      PROTECTED SET.
                              
  DEF PUBLIC PROPERTY POPUP-MENU-OBJECT AS JBoxPopupMenu GET. 
      SET(ioPopupMenu AS JBoxPopupMenu):
        IF NOT VALID-OBJECT(ioPopupMenu) THEN DO:
          IF VALID-OBJECT(oPopupMenu) THEN DO:
            BROWSE-HANDLE:POPUP-MENU = ?.
            DYNAMIC-FUNCTION("deletObjectLink",BROWSE-HANDLE,ioPopupMenu:POPUP-MENU-HANDLE).
          END.  
        END.
        ELSE IF BROWSE-HANDLE:POPUP-MENU = ? THEN DO:
          BROWSE-HANDLE:POPUP-MENU = ioPopupMenu:POPUP-MENU-HANDLE.
          DYNAMIC-FUNCTION("createObjectLink",BROWSE-HANDLE,ioPopupMenu:POPUP-MENU-HANDLE).
          DYNAMIC-FUNCTION("setObjectName",ioPopupMenu:POPUP-MENU-HANDLE,"popupMenu" + BROWSE-HANDLE:NAME).
        END.
        ELSE
          DYNAMIC-FUNCTION("setObjectName",ioPopupMenu:POPUP-MENU-HANDLE,"popupMenu" + BROWSE-HANDLE:NAME).
        oPopupMenu = ioPopupMenu.  
      END SET.
                      
  DEF PUBLIC PROPERTY MSWinMultiSelect AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"windowsBrowse") = "yes". END GET.
      SET(ibSet AS LOG): 
        IF ibSet THEN
          DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"windowsBrowse","yes").
        ELSE
          DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"windowsBrowse","").
      END SET.
      
  DEF PUBLIC PROPERTY browseSearchAsFilter AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"searchDefault") = "filter". END GET.
      SET(ibSet AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"searchDefault",IF ibSet THEN "filter" ELSE "").
      END SET.
        
  DEF PUBLIC PROPERTY rowsToBatch      AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",
                       IF VALID-HANDLE(BROWSE-HANDLE) THEN BROWSE-HANDLE ELSE DESIGN-WIDGET-HANDLE,"rowsToBatch")). 
      END GET. 
      SET(iiRowsToBatch AS INT):
        DYNAMIC-FUNCTION("setAttribute",
                          IF VALID-HANDLE(BROWSE-HANDLE) THEN BROWSE-HANDLE ELSE DESIGN-WIDGET-HANDLE,"rowsToBatch",STRING(iiRowsToBatch)).
      END SET.

  DEF PUBLIC PROPERTY queryJoin AS CHAR
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"queryJoin"). END GET.
      SET(icQueryJoin AS CHAR):
        DEF VAR ix          AS INT NO-UNDO.
        DEF VAR cBufferJoin AS CHAR NO-UNDO.
        DEF VAR cBuffer     AS CHAR NO-UNDO.
        icQueryJoin = TRIM(icQueryJoin).
        IF NOT icQueryJoin BEGINS "," THEN icQueryJoin = "," + icQueryJoin.
        DO ix = 2 TO NUM-ENTRIES(icQueryJoin):
          cBufferJoin = TRIM(ENTRY(ix,icQueryJoin)).
          IF SUBSTR(cBufferJoin,1,6) = "first " OR
             SUBSTR(cBufferJoin,1,5) = "last " OR
             SUBSTR(cBufferJoin,1,5) = "each " THEN
            cBuffer = ENTRY(1,TRIM(SUBSTR(cBufferJoin,6))," ").
          ELSE
            cBuffer = ENTRY(1,cBufferJoin," ").
          DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"bufferJoin" + cBuffer, "," + cBufferJoin).
        END.
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"queryJoin",icQueryJoin).
        IF BROWSE-HANDLE:TYPE NE "browse" THEN cQueryJoin = icQueryJoin.
      END SET.

  DEF PUBLIC PROPERTY buffersAndFields AS CHAR
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"buffersAndFields"). END GET.
      SET(icBuffersAndFields AS CHAR):
        DEF VAR ix           AS INT    NO-UNDO.
        DEF VAR cTtField     AS CHAR   NO-UNDO.
        DEF VAR cTtFieldList AS CHAR   NO-UNDO.
        DEF VAR hColumn      AS HANDLE NO-UNDO.
        IF NOT (icBuffersAndFields NE "" AND DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"tempTableHandle") NE "") THEN DO:
          DO ix = 2 TO NUM-ENTRIES(icBuffersAndFields):
            cBufferList = cBufferList + "," + ENTRY(1,ENTRY(ix,icBuffersAndFields),";").
          END.  
          IF VALID-HANDLE(BROWSE-HANDLE) AND BROWSE-HANDLE:TYPE = "browse" THEN DYNAMIC-FUNCTION("deleteObject",BROWSE-HANDLE).
          DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
          BROWSE-HANDLE = DYNAMIC-FUNCTION("NewBrowse",DESIGN-WIDGET-HANDLE,rowsToBatch,cInitAttr,icBuffersAndFields,"where false" + cBufferList,"").
          IF DESIGN-WIDGET-HANDLE:TYPE NE "BROWSE" THEN DO:
            DYNAMIC-FUNCTION("deleteObject",DESIGN-WIDGET-HANDLE).
            DYNAMIC-FUNCTION("createObjectLink",BROWSE-HANDLE,hSourceProc).
          END.  
        END.
        ELSE DO:
          DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields,";"):
            cTtField = ENTRY(ix,icBuffersAndFields,";").
            IF NUM-ENTRIES(cTtField) > 1 THEN
              cTtField = ENTRY(2,cTtField).
            cTtFieldList = cTtFieldList + (IF cTtFieldList NE "" THEN "," ELSE "") + ENTRY(1,cTtField,"|").
          END.    
          DO ix = 1 TO BROWSE-HANDLE:NUM-COLUMNS:
            hColumn = BROWSE-HANDLE:GET-BROWSE-COLUMN(ix).
            IF NOT CAN-DO(cTtFieldList,hColumn:NAME) THEN hColumn:VISIBLE = NO.
          END.
        END.
        ASSIGN BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
               QUERY-HANDLE      = BROWSE-HANDLE:QUERY
               TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
               cBuffersAndFields = icBuffersAndFields
               hBrowse           = BROWSE-HANDLE 
               .
        IF cSortExpr NE "" THEN
          DYNAMIC-FUNCTION("setSortString",BROWSE-HANDLE,cSortExpr).     
        IF VALID-HANDLE(SEARCH-FIELD-HANDLE) THEN
          THIS-OBJECT:setSearchField(SEARCH-FIELD-HANDLE,cSearchColumn).
        IF cQueryJoin NE "" THEN
          queryJoin = cQueryJoin.  
        cBufferList = ENTRY(1,ENTRY(1,icBuffersAndFields),";") + cBufferList.  
      END SET.  

  DEF PUBLIC PROPERTY enabledColumns AS CHAR GET.
      SET(icFieldList AS CHAR):
        DEF VAR ix      AS INT    NO-UNDO.
        DEF VAR hColumn AS HANDLE NO-UNDO.
        DEF VAR oFillIn AS JBoxBrowseFillIn NO-UNDO.
        DO ix = 1 TO BROWSE-HANDLE:NUM-COLUMNS:
          IF CAN-DO(icFieldList,BROWSE-HANDLE:GET-BROWSE-COLUMN(ix):NAME) AND (IF icFieldList = "*" THEN BROWSE-HANDLE:GET-BROWSE-COLUMN(ix):VISIBLE ELSE TRUE) THEN 
            oFillIn = NEW JBoxBrowseFillIn(THIS-OBJECT,BROWSE-HANDLE:GET-BROWSE-COLUMN(ix):NAME,hSourceProc,
                      icFieldList NE "*" /* Refresh after each update if all columns are enabled */
                      ).
        END. 
      END SET.

  DEF PUBLIC PROPERTY enableOnDblClick AS LOG
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"enableOnDblClick") = "yes". END GET.
      SET(ibEnable AS LOG):
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"enableOnDblClick",STRING(ibEnable)).        
      END SET.

  DEF PUBLIC PROPERTY enableOnToolbarClick AS LOG
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"enableOnToolbarClick") = "yes". END GET.
      SET(ibEnable AS LOG):
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"enableOnToolbarClick",STRING(ibEnable)).        
      END SET.

  DEF PUBLIC PROPERTY setReadOnlyOnReturn AS LOG
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"setReadOnlyOnReturn") = "yes". END GET.
      SET(ibEnable AS LOG):
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"setReadOnlyOnReturn",STRING(ibEnable)).        
      END SET.

  DEF PUBLIC PROPERTY customCreateProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"customCreateProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"customCreateProc",icSetting).  
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"customCreateProc",icSetting). /* Duplicated since the buffer might function as a field-map */  
      END SET.
      
  DEF PUBLIC PROPERTY customUpdateValProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"customUdateValProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"customUpdateValProc",  
                         (IF icSetting = "ignore" OR icSetting BEGINS "+" THEN icSetting ELSE "=" + TRIM(icSetting,"="))).
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"customUpdateValProc",DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"customUpdateValProc")).            
      END SET.
      
  DEF PUBLIC PROPERTY customDeleteValProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"customDeleteValProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"customDeleteValProc",
                         (IF icSetting = "ignore" OR icSetting BEGINS "+" THEN icSetting ELSE "=" + TRIM(icSetting,"="))).  
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"customDeleteValProc",DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"customDeleteValProc")).            
      END SET.
       
  DEF PUBLIC PROPERTY postUpdateProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"postUdateProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"postUpdateProc",icSetting).
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"postUpdateProc",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY bufferExtraFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"bufferExtraFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"bufferExtraFields",icSetting).
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraFields",icSetting).
      END SET.
       
  DEF PUBLIC PROPERTY bufferExtraValues AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"bufferExtraValues"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"bufferExtraValues",icSetting).
        DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraValues",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY serverTransInputParam AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"serverTransInputParam"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"serverTransInputParam",icSetting).
      END SET.

  DEF PUBLIC PROPERTY serverTransReturnParam AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"serverTransReturnParam"). END GET. 
      PROTECTED SET.
      
  DEF PUBLIC PROPERTY serverTransReturnMessage AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"serverTransReturnMessage"). END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY placeholderName AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"placeholderName"). END GET. 
      PROTECTED SET.
        
   DEF PUBLIC PROPERTY editUsingOverlayFieldMap AS LOG
      GET(): END GET.
      SET(ibSetting AS LOG):
        DEF VAR hToolbar  AS HANDLE NO-UNDO.
        
        DEF VAR cUpdFlds     AS CHAR NO-UNDO.
        DEF VAR cCorrUpdFlds AS CHAR NO-UNDO.
        DEF VAR ix           AS INT  NO-UNDO.
        DEF VAR hCol         AS HANDLE NO-UNDO.
        
        IF ibSetting AND NOT VALID-HANDLE(hColumnFieldMap) THEN DO:
          DO ix = 1 TO THIS-OBJECT:BROWSE-HANDLE:NUM-COLUMNS:
            hCol = THIS-OBJECT:BROWSE-HANDLE:GET-BROWSE-COLUMN(ix).
            FIND FIRST ttOverlay WHERE ttOverlay.cBrowseColumn = hCol:NAME NO-ERROR.
            IF AVAIL ttOverlay THEN DO: 
              ttOverlay.iSeq = -1.
              LEAVE.
            END.
          END.      

          hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",THIS-OBJECT:BROWSE-HANDLE,"toolbar","from").
        
          FOR EACH ttOverlay BY ttOverlay.iSeq:
            ASSIGN cUpdFlds = cUpdFlds + (IF cUpdFlds NE "" THEN "," ELSE "") + ttOverlay.cBrowseColumn
                   cCorrUpdFlds = cCorrUpdFlds + (IF cCorrUpdFlds NE "" THEN "," ELSE "") + ttOverlay.cBufferColumn
                   .  
            DYNAMIC-FUNCTION("setAttribute",ttOverlay.hOverlay,"refreshRow","no").       
          END.    
          
          IF cUpdFlds NE "" AND VALID-HANDLE(hToolbar) THEN DO: 
            hColumnFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
                         ,THIS-OBJECT:BROWSE-HANDLE:QUERY
                         ,THIS-OBJECT:BROWSE-HANDLE:FRAME
                         ,cCorrUpdFlds,cUpdFlds
                         ,"",""
                         ,";UseForBrwOverlays").      
            DYNAMIC-FUNCTION("CreateObjectLink",hColumnFieldMap,THIS-OBJECT:BROWSE-HANDLE).
            DYNAMIC-FUNCTION("CreateObjectLink",hColumnFieldMap,hToolbar).
            
            DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"insertBrowseRow","yes").  
            DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"editWithFieldMap","yes").  
            DYNAMIC-FUNCTION("setAttribute",hColumnFieldMap,"fieldMapIsBrowse","yes").
            DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"editUsingOverlayFieldMap","yes").
            DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"enableOnToolbarClick","yes").
          END.
          ELSE IF NOT VALID-HANDLE(hToolbar) THEN
            MESSAGE 'Failed to set property "editUsingOverlayFieldMap" - no toolbar is defined'
                     VIEW-AS ALERT-BOX ERROR.   
          ELSE 
            MESSAGE 'Failed to set property "editUsingOverlayFieldMap" - no overlay fields are defined'
                     VIEW-AS ALERT-BOX ERROR.   
        END.
        ELSE
          hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",THIS-OBJECT:BROWSE-HANDLE,"toolbar","from").
                  
      END SET.
      
  DEFINE PUBLIC PROPERTY disabledTools AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"disabledEvents").
    END GET.
    SET(icSetting AS CHARACTER): 
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"disabledEvents",icSetting).
    END SET.
        
  DEFINE PUBLIC PROPERTY enabledTools AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"enabledEvents").
    END GET.
    SET(icSetting AS CHARACTER): 
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"enabledEvents",icSetting).
    END SET.  

  DEFINE PUBLIC PROPERTY dataBrowseObject AS LOG
    GET():
      RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"dataBrowseObject") = "yes".
    END GET.
    SET(ibSetting AS LOG): 
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"dataBrowseObject",IF ibSetting THEN "yes" ELSE "").
    END SET.  
                 	
  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    IF ihDesignWidget:TYPE = "browse" AND DYNAMIC-FUNCTION("getObjectType",ihDesignWidget) = "browse" THEN    
      ASSIGN BROWSE-HANDLE     = ihDesignWidget
             BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
             QUERY-HANDLE      = BROWSE-HANDLE:QUERY
             TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
             cBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihDesignWidget,"buffersAndFields")
             hBrowse           = BROWSE-HANDLE
             . 
    ELSE
      InitializeComponent(ihDesignWidget,NO,"",NO,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ihSourceProc   AS HANDLE):
      SUPER ().

    hSourceProc = ihSourceProc.

    IF ihDesignWidget:TYPE = "browse" AND DYNAMIC-FUNCTION("getObjectType",ihDesignWidget) = "browse" THEN    
      ASSIGN BROWSE-HANDLE     = ihDesignWidget
             BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
             QUERY-HANDLE      = BROWSE-HANDLE:QUERY
             TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
             cBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihDesignWidget,"buffersAndFields")
             hBrowse           = BROWSE-HANDLE
             . 
    ELSE
      InitializeComponent(ihDesignWidget,NO,"",NO,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ihSourceProc   AS HANDLE
                                ,ibMultiple     AS LOG):
      SUPER ().

    hSourceProc = ihSourceProc.

    IF ihDesignWidget:TYPE = "browse" AND DYNAMIC-FUNCTION("getObjectType",ihDesignWidget) = "browse" THEN    
      ASSIGN BROWSE-HANDLE     = ihDesignWidget
             BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
             QUERY-HANDLE      = BROWSE-HANDLE:QUERY
             TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
             cBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihDesignWidget,"buffersAndFields")
             hBrowse           = BROWSE-HANDLE
             . 
    ELSE
      InitializeComponent(ihDesignWidget,ibMultiple,"",NO,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ihSourceProc   AS HANDLE
                                ,ibMultiple     AS LOG
                                ,ihTEMP-TABLE   AS HANDLE):
      SUPER ().

    hSourceProc = ihSourceProc.

    IF ihDesignWidget:TYPE = "browse" AND DYNAMIC-FUNCTION("getObjectType",ihDesignWidget) = "browse" THEN    
      ASSIGN BROWSE-HANDLE     = ihDesignWidget
             BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
             QUERY-HANDLE      = BROWSE-HANDLE:QUERY
             TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
             cBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihDesignWidget,"buffersAndFields")
             hBrowse           = BROWSE-HANDLE
             . 
    ELSE
      InitializeComponent(ihDesignWidget,ibMultiple,"",NO,ihTEMP-TABLE).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ihSourceProc   AS HANDLE
                                ,ibMultiple     AS LOG
                                ,ihTEMP-TABLE   AS HANDLE
                                ,ibDataBrowse   AS LOG):
      SUPER ().

    hSourceProc = ihSourceProc.

    THIS-OBJECT:dataBrowseObject = ibDataBrowse.
  
    IF ihDesignWidget:TYPE = "browse" AND DYNAMIC-FUNCTION("getObjectType",ihDesignWidget) = "browse" THEN    
      ASSIGN BROWSE-HANDLE     = ihDesignWidget
             BUFFER-HANDLE     = BROWSE-HANDLE:QUERY:GET-BUFFER-HANDLE(1)
             QUERY-HANDLE      = BROWSE-HANDLE:QUERY
             TEMP-TABLE-HANDLE = BUFFER-HANDLE:TABLE-HANDLE
             cBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihDesignWidget,"buffersAndFields")
             hBrowse           = BROWSE-HANDLE
             . 
    ELSE
      InitializeComponent(ihDesignWidget,ibMultiple,"",NO,ihTEMP-TABLE).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE,ibMultiple AS LOG):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihDesignWidget,ibMultiple,"",NO,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ibMultiple     AS LOG
                                ,icTitle        AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    InitializeComponent(ihDesignWidget,ibMultiple,icTitle,NO,?).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget AS HANDLE
                                ,ibMultiple     AS LOG
                                ,icTitle        AS CHAR
                                ,ibRowMarkers   AS LOG):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    InitializeComponent(ihDesignWidget,ibMultiple,icTitle,ibRowMarkers,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowse (ihDesignWidget     AS HANDLE
                                ,ibMultiple         AS LOG
                                ,icTitle            AS CHAR
                                ,ibRowMarkers       AS LOG
                                ,iTEMP-TABLE-HANDLE AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihDesignWidget,ibMultiple,icTitle,ibRowMarkers,iTEMP-TABLE-HANDLE).                 
      
  END CONSTRUCTOR.

	METHOD PRIVATE CHARACTER getFilterOper(INPUT icOper AS CHAR):
		
		DEFINE VARIABLE ocOper AS CHARACTER NO-UNDO.
  
    CASE TRIM(icOper):
      WHEN "EQ" THEN ocOper = "=".
      WHEN "NE" THEN ocOper = "<>".
      WHEN "GE" THEN ocOper = ">=".
      WHEN "LE" THEN ocOper = "<=".
      WHEN "GT" THEN ocOper = ">".
      WHEN "LT" THEN ocOper = "<".
      OTHERWISE ocOper = icOper.
    END CASE.  

		RETURN ocOper.

	END METHOD.

  METHOD PRIVATE VOID initializeComponent(ihDesignWidget     AS HANDLE 
                                         ,ibMultiple         AS LOG
                                         ,icTitle            AS CHAR
                                         ,ibRowMarkers       AS LOG
                                         ,iTEMP-TABLE-HANDLE AS HANDLE):
    DEF VAR ix            AS INT  NO-UNDO.
    DEF VAR cBuffsAndFlds AS CHAR NO-UNDO.
    DEF VAR cQueryJoin    AS CHAR NO-UNDO.
    DEF VAR cBufferList   AS CHAR NO-UNDO.
    DEF VAR cCalcFldProc  AS CHAR NO-UNDO.
 
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).
    DYNAMIC-FUNCTION("NewObject",
                     IF VALID-HANDLE(hSourceProc:CURRENT-WINDOW) THEN hSourceProc:CURRENT-WINDOW ELSE ?,
                     hSourceProc,"procedure").

    DESIGN-WIDGET-HANDLE  = ihDesignWidget.
    IF ihDesignWidget:TYPE = "browse" THEN DO: 
      IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"getBuffersAndFieldsBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN  
        cBuffsAndFlds = DYNAMIC-FUNCTION("getBuffersAndFieldsBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME IN hSourceProc) NO-ERROR.
      IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"getQueryJoinBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN DO:  
        cQueryJoin = TRIM(DYNAMIC-FUNCTION("getQueryJoinBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME IN hSourceProc),",").
        DYNAMIC-FUNCTION("setAttribute",ihDesignWidget,"queryJoin","," + cQueryJoin) NO-ERROR.
      END.
      IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"getCalcFieldProcBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN  
        DYNAMIC-FUNCTION("setAttribute",ihDesignWidget,"calcFieldProc",DYNAMIC-FUNCTION("getCalcFieldProcBrw" + ihDesignWidget:QUERY:GET-BUFFER-HANDLE(1):NAME IN hSourceProc)) NO-ERROR.
    END.  
    ELSE DO:
      DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).
      IF NOT dataBrowseObject THEN
        DYNAMIC-FUNCTION("NewObject",
                         IF VALID-HANDLE(hSourceProc:CURRENT-WINDOW) THEN hSourceProc:CURRENT-WINDOW ELSE ?,
                         ihDesignWidget,"browse").
    END.                     
    
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch") NE "" THEN
      rowsToBatch = int(DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch")).  

    IF ibMultiple THEN 
      cInitAttr = "MULTIPLE".
    IF icTitle NE "" THEN
      cInitAttr = cInitAttr + (IF cInitAttr NE "" THEN "," ELSE "") + "TITLE|" + icTitle.               
    IF ibRowMarkers THEN
      cInitAttr = cInitAttr + (IF cInitAttr NE "" THEN "," ELSE "") + "ROW-MARKERS".               
               
    BROWSE-HANDLE = DESIGN-WIDGET-HANDLE.
    
    IF VALID-HANDLE(iTEMP-TABLE-HANDLE) THEN DO:
      IF iTEMP-TABLE-HANDLE:TYPE = "buffer" THEN DO:
        TEMP-TABLE-HANDLE = iTEMP-TABLE-HANDLE:TABLE-HANDLE.
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"tempTableHandle",STRING(TEMP-TABLE-HANDLE)).
      END.  
      ELSE DO:
        TEMP-TABLE-HANDLE = iTEMP-TABLE-HANDLE.
        DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"tempTableHandle",STRING(iTEMP-TABLE-HANDLE)).
      END.  
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"basetable",iTEMP-TABLE-HANDLE:NAME).
      IF NOT dataBrowseObject THEN
        buffersAndFields = "". /* Creates the browse - no need to set buffers and fields from container */
    END.  
    ELSE IF cBuffsAndFlds NE "" THEN DO: 
      buffersAndFields = cBuffsAndFlds.
      cBufferList = DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"bufferList").
    END.
    
    IF ihDesignWidget:TYPE = "browse" THEN
      DYNAMIC-FUNCTION("CreateObjectLink",BROWSE-HANDLE,hSourceProc).
                 
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch") NE "" THEN
      THIS-OBJECT:rowsToBatch = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch")).
    ELSE               
      THIS-OBJECT:rowsToBatch = 200.      
      
    PUBLISH "setChildBrowseObject" FROM hSourceProc (hSourceProc,THIS-OBJECT). /* publish to tabfolder class */                             
  END METHOD.                                       

  METHOD PUBLIC LOGICAL applyFilter(icExprList AS CHAR):
    DEF VAR cFields    AS CHAR NO-UNDO.
    DEF VAR cOperators AS CHAR NO-UNDO.
    DEF VAR cValues    AS CHAR NO-UNDO.
    DEF VAR hToolbar AS HANDLE NO-UNDO.
    
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",BROWSE-HANDLE,"toolbar","from").
    bOK = NO.                                  
    IF resolveFilterExpr(IF icExprList = "" THEN cLastFilter ELSE icExprList,OUTPUT cFields,OUTPUT cOperators,OUTPUT cValues) THEN DO:                                  
      IF BROWSE-HANDLE:TYPE = "browse" THEN DO:                                  
        bOK = DYNAMIC-FUNCTION("InitDynFilter",BROWSE-HANDLE,cFields,cOperators,cValues,
                              (IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static" ELSE "") +
                              (IF bNoExecFilter THEN "noexec" ELSE IF bTryFilter THEN "try" ELSE "")).
        DYNAMIC-FUNCTION("setCurrentObject",BROWSE-HANDLE).
      END.  
      ELSE
        MESSAGE "Filter cannot be applied before object is initialized, ie buffers and fields are defined (buffersAndFields property)"
        VIEW-AS ALERT-BOX.
    END.
    ELSE
      MESSAGE "Error in filter definition: " + icExprList.
      
    ASSIGN bTryFilter = NO
           bNoExecFilter = NO
           cLastFilter = "".

    RETURN bOk.      
  END METHOD.
  
  METHOD PUBLIC LOGICAL applyFilter(icExprList AS CHAR,ibTry AS LOG):
    bTryfilter = ibTry.
    RETURN applyFilter(IF icExprList = "" THEN cLastFilter ELSE icExprList).
  END METHOD.
  
  METHOD PUBLIC LOGICAL applyFilter(icFieldList AS CHAR
                                   ,icOperList  AS CHAR
                                   ,icValueList AS CHAR):
    DEF VAR hToolbar AS HANDLE NO-UNDO.
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",BROWSE-HANDLE,"toolbar","from").
    
    bOK = NO.                                  
    IF QUERY-HANDLE:TYPE = "Browse" THEN DO:                                  
      bOK = DYNAMIC-FUNCTION("InitDynFilter",BROWSE-HANDLE,icFieldList,icOperList,icValueList,
                             (IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static" ELSE "") +
                             (IF bNoExecFilter THEN "noexec" ELSE IF bTryFilter THEN "try" ELSE "")).
      DYNAMIC-FUNCTION("setCurrentObject",BROWSE-HANDLE).
    END.                         
    ELSE
      MESSAGE "Filter cannot be applied before object is initialized, ie buffers and fields are defined (buffersAndFields property)"
      VIEW-AS ALERT-BOX.
      
    ASSIGN bTryFilter = NO
           bNoExecFilter = NO
           cLastFilter = "".

    RETURN bOk.      
  END METHOD.
  
  METHOD PUBLIC VOID setFilter(icExprList AS CHAR):
    ASSIGN bNoExecFilter = YES
           cLastFilter    = icExprList.
    applyFilter(icExprList).
  END METHOD.  

  METHOD PUBLIC VOID setFilter(icFieldList AS CHAR
                              ,icOperList  AS CHAR
                              ,icValueList AS CHAR):
    DEF VAR hToolbar AS HANDLE NO-UNDO.
    
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",BROWSE-HANDLE,"toolbar","from").
    
    IF BROWSE-HANDLE:TYPE = "browse" THEN DO:                                  
      bOK = DYNAMIC-FUNCTION("InitDynFilter",BROWSE-HANDLE,icFieldList,icOperList,icValueList,
                             IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static,noexec" ELSE "noexec").
      DYNAMIC-FUNCTION("setCurrentObject",BROWSE-HANDLE).
    END.                         
    ELSE
      MESSAGE "Filter cannot be applied before object is initialized, ie buffers and fields are defined (buffersAndFields property)"
      VIEW-AS ALERT-BOX.
  END METHOD.  

  METHOD PUBLIC VOID setQuerySort(icSortExpr AS CHAR): /* "field1;[desc],field2.." or "field1 [desc] by field2.." */
    DEF VAR ix         AS INT  NO-UNDO.
    DEF VAR cEntry     AS CHAR NO-UNDO.
    DEF VAR cPrevEntry AS CHAR NO-UNDO.
    DEF VAR cNewString AS CHAR NO-UNDO.
    
    icSortExpr = REPLACE(icSortExpr,"descending","desc").
    
    IF NUM-ENTRIES(icSortExpr," ") > 1 AND NUM-ENTRIES(icSortExpr) = 1 THEN DO:
      DO ix = 1 TO NUM-ENTRIES(icSortExpr," "):
        cEntry = ENTRY(ix,icSortExpr," ").
        IF cEntry = "BY" THEN
          cNewString = cNewString + (IF cNewString NE "" THEN "," ELSE "").
        ELSE IF (cEntry = "desc" OR cEntry = "descending") AND cPrevEntry NE "BY" AND cNewString NE "" THEN
          cNewString = cNewString + ";desc".
        ELSE 
          cNewString = cNewString + (IF cNewString NE "" THEN "," ELSE "") + cEntry.       
        cPrevEntry = cEntry.  
      END.
      icSortExpr = cNewString.
    END.    
    
    IF VALID-HANDLE(BROWSE-HANDLE) AND BROWSE-HANDLE:TYPE = "browse" THEN
      DYNAMIC-FUNCTION("setSortString",BROWSE-HANDLE,icSortExpr).
    ELSE cSortExpr = icSortExpr.
  END METHOD.

  METHOD PUBLIC VOID addCalcField(icSourceBuffer AS CHAR
                                 ,icFieldName    AS CHAR
                                 ,icProcedure    AS CHAR
                                 ,icParameter    AS CHAR):
    DEF VAR ix            AS INT    NO-UNDO.
    DEF VAR cBufferFields AS CHAR   NO-UNDO.
    DEF VAR cFieldDef     AS CHAR   NO-UNDO.
    DEF VAR cBuffsAndFlds AS CHAR   NO-UNDO.
    DEF VAR cBuffFldsDef  AS CHAR   NO-UNDO.
    DEF VAR cAllCalcFlds  AS CHAR   NO-UNDO.
    DEF VAR hColumn       AS HANDLE NO-UNDO.
    
    hColumn = BUFFER-HANDLE:BUFFER-FIELD (icFieldName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
              "(Field " icFieldName  " needs to be defined in the static temp-table " buffer-handle:NAME ")"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    
    IF icSourceBuffer = "" THEN icSourceBuffer = ENTRY(1,cBufferList).
    cFieldDef = "+" + icFieldName + "|" + hColumn:DATA-TYPE + "||" + icProcedure 
              + (IF icParameter = "calcParam" THEN
                   "ROWID"          
                 ELSE IF CAN-DO("jb_can-find,jb_total",icProcedure) THEN  
                   "(" + icParameter + ")"
                 ELSE IF icParameter NE "" AND DYNAMIC-FUNCTION ("getIsFieldInTable",icSourceBuffer + "." + icParameter) THEN 
                   "(" + icParameter + ")"
                 ELSE IF icParameter NE "" THEN
                   "ROWID" + icParameter  
                 ELSE "").
    
    cBufferFields = DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"bufferFields" + icSourceBuffer).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"fieldBuffer" + icFieldName,icSourceBuffer).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"orgDbField" + icFieldName,icFieldName).
    cAllCalcFlds = DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"allCalcFields").
    IF NOT CAN-DO(cAllCalcFlds,icFieldName) THEN DO:
      cAllCalcFlds = icFieldName + (IF cAllCalcFlds NE "" THEN "," ELSE "") + cAllCalcFlds.
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"allCalcFields",cAllCalcFlds).
    END.
    IF NOT CAN-DO(cBufferFields,icFieldName) THEN DO:
      cBufferFields = cBufferFields + (IF cBufferFields NE "" THEN "," ELSE "") + icFieldName.
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"bufferFields" + icSourceBuffer,cBufferFields).
    END.  
    cBufferFields = "".
    DO ix = 1 TO NUM-ENTRIES(cBuffersAndFields):
      cBuffFldsDef = ENTRY(ix,cBuffersAndFields).
      IF ENTRY(1,cBuffFldsDef,";") = icSourceBuffer AND lookup(cFieldDef,cBuffFldsDef,";") = 0 THEN
        cBuffFldsDef = cBuffFldsDef + ";" + cFieldDef.
      cBufferFields = cBufferFields + (IF cBufferFields NE "" THEN "," ELSE "") + cBuffFldsDef. 
    END.           
    cBuffersAndFields = cBufferFields.                        
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"buffersAndFields",cBufferFields).
  END METHOD.
  METHOD PUBLIC VOID addCalcField(icFieldName    AS CHAR
                                 ,icProcedure    AS CHAR
                                 ,icParameter    AS CHAR):
    addCalcField("",icFieldName,icProcedure,icParameter).
  END METHOD.
  METHOD PUBLIC VOID addCalcField(icFieldName    AS CHAR
                                 ,icProcedure    AS CHAR):
    addCalcField("",icFieldName,icProcedure,"").
  END METHOD.
  
  METHOD PUBLIC VOID calcCan-Find(icSourceBuffer AS CHAR
                                 ,icFieldName    AS CHAR
                                 ,icExpression   AS CHAR):
    DEF VAR hColumn AS HANDLE NO-UNDO.
                                  
    hColumn = BUFFER-HANDLE:BUFFER-FIELD (icFieldName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
              "(Field " icFieldName  " needs to be defined as type LOGICAL in the temp-table " buffer-handle:NAME ")"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE IF hColumn:DATA-TYPE NE "LOGICAL" THEN DO:
      MESSAGE "Field " icFieldName  " needs to be defined as type LOGICAL in the temp-table " buffer-handle:NAME
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    IF icExpression BEGINS "(" THEN icExpression = SUBSTR(icExpression,2).
    IF SUBSTR(icExpression,LENGTH(icExpression)) = ")" THEN
      icExpression = SUBSTR(icExpression,1,LENGTH(icExpression) - 1).
    addCalcField(icSourceBuffer,icFieldName,"jb_can-find",icExpression).
  END METHOD.
  METHOD PUBLIC VOID calcCan-Find(icFieldName    AS CHAR
                                 ,icExpression   AS CHAR):
    calcCan-Find("",icFieldName,icExpression).                               
  END METHOD.

  METHOD PUBLIC VOID calcTotal(icSourceBuffer AS CHAR
                              ,icFieldName    AS CHAR
                              ,icQuery        AS CHAR
                              ,icExpression   AS CHAR):
    DEF VAR hColumn AS HANDLE NO-UNDO.
                                  
    hColumn = BUFFER-HANDLE:BUFFER-FIELD (icFieldName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
              "(Field " icFieldName  " needs to be defined as type DECIMAL or INTEGER in the temp-table " buffer-handle:NAME ")"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE IF NOT CAN-DO("DECIMAL,INTEGER",hColumn:DATA-TYPE) THEN DO:
      MESSAGE "Field " icFieldName  " needs to be defined as type DECIMAL or INTEGER in the temp-table " buffer-handle:NAME
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    addCalcField(icSourceBuffer,icFieldName,"jb_total",icQuery + "¤" + icExpression).
  END METHOD.
  METHOD PUBLIC VOID calcTotal(icFieldName    AS CHAR
                              ,icQuery        AS CHAR
                              ,icExpression   AS CHAR):
    calcTotal("",icFieldName,icQuery,icExpression).                               
  END METHOD.


  METHOD PUBLIC VOID calcWeek(icSourceBuffer AS CHAR
                             ,icFieldName    AS CHAR
                             ,icParamField   AS CHAR):
    DEF VAR hColumn AS HANDLE NO-UNDO.
                
    IF icSourceBuffer = "" THEN icSourceBuffer = ENTRY(1,cBufferList).
                                  
    hColumn = BUFFER-HANDLE:BUFFER-FIELD (icFieldName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
              "(Field " icFieldName  " needs to be defined as type CHARACTER in the temp-table " buffer-handle:NAME ")"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE IF hColumn:DATA-TYPE NE "CHARACTER" THEN DO:
      MESSAGE "Field " icFieldName  " needs to be defined as type CHARACTER in the temp-table " buffer-handle:NAME
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    
    IF DYNAMIC-FUNCTION ("getFieldDataType",icSourceBuffer + "." + icParamField) NE "date" THEN DO:
      MESSAGE ERROR-STATUS:GET-MESSAGE(1) SKIP
              "Parameterfield " icParamField  " needs to be of type DATE."
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    addCalcField(icSourceBuffer,icFieldName,"jb_weeknum",icParamField).
  END METHOD.

  METHOD PUBLIC VOID setCalcFieldParam(icFieldName AS CHAR
                                      ,icParameter AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"calcParam" + icFieldName,icParameter).        
  END METHOD.
           	
  METHOD PUBLIC VOID openQuery():
    RUN InvokeMethod(BROWSE-HANDLE,"OpenQuery").
  END METHOD.
  
  METHOD PUBLIC VOID openQuery(INPUT icQueryString AS CHAR):
    icQueryString = TRIM(icQueryString).
    IF (DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"baseQuery") NE "" OR
        DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"queryFilter") NE "") AND 
       icQueryString BEGINS "WHERE " THEN DO:
      icQueryString = " AND (" + SUBSTR(icQueryString,6) + ")".      
    END.
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"queryWhere",icQueryString).
    RUN InvokeMethod(BROWSE-HANDLE,"OpenQuery").
  END METHOD.
  
  METHOD PUBLIC VOID displayRecord():
    /* Check if current row is modified before displaying the record, refreshing child records and setting the toolbar */
    RUN InvokeMethod(BROWSE-HANDLE,"DisplayRecord").
  END METHOD.

  METHOD PUBLIC VOID excelRecord():
    RUN InvokeMethod(BROWSE-HANDLE,"ExcelRecord").
  END METHOD.
  
  METHOD PUBLIC VOID excelRecord(icFileName AS CHAR):
    DEF VAR cType AS CHAR NO-UNDO.
    
    cType = ENTRY(NUM-ENTRIES(icFileName,"."),icFileName,".").
    IF cType = "csv" THEN
      DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"ExcelDelimiter",";").
    
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"ExcelFileName",icFileName).
        
    RUN InvokeMethod(BROWSE-HANDLE,"ExcelRecord").
    
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"ExcelDelimiter","").
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"ExcelFileName","").
  END METHOD.

  METHOD PUBLIC LOGICAL refreshRow():
    /* Refresh row from database */
    RETURN DYNAMIC-FUNCTION("RefreshRowids",THIS-OBJECT:BROWSE-HANDLE,IF THIS-OBJECT:BUFFER-HANDLE:AVAIL THEN THIS-OBJECT:BUFFER-HANDLE::RowIdent1 ELSE "") > 0. 
  END METHOD.

  METHOD PUBLIC LOGICAL refreshSelectedRows():
    /* Refresh rows from database */
    DEF VAR ix         AS INT  NO-UNDO.
    DEF VAR cRowidList AS CHAR NO-UNDO.
    DO ix = 1 TO THIS-OBJECT:BROWSE-HANDLE:NUM-SELECTED-ROWS:
      IF THIS-OBJECT:BROWSE-HANDLE:FETCH-SELECTED-ROW(ix) THEN
        cRowidList = cRowidList + (IF cRowidList NE "" THEN "," ELSE "") + THIS-OBJECT:BUFFER-HANDLE::RowIdent1.
    END.    
      
    RETURN DYNAMIC-FUNCTION("RefreshRowids",THIS-OBJECT:BROWSE-HANDLE,cRowidList) > 0. 
  END METHOD.

  METHOD PUBLIC LOGICAL DisplayColumns():
    /* Refresh screen-values for current row only */
    RETURN DYNAMIC-FUNCTION("DisplayRow",THIS-OBJECT:BROWSE-HANDLE). 
  END METHOD.

  METHOD PUBLIC VOID refresh():
    /* Refresh all records in the query */
    RUN InvokeMethod(THIS-OBJECT:BROWSE-HANDLE,"RefreshBrowseRecord").	
  END METHOD.
    
  METHOD PUBLIC VOID setFilterDropDownQuery(icFieldName AS CHAR
                                           ,icBuffersAndFields AS CHAR
                                           ,icQuery AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropdownFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropdownQuery_" + icFieldName,icQuery).
  END METHOD.  

  METHOD PUBLIC VOID setFilterDropDownList(icFieldName  AS CHAR
                                      ,icValuePairs AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropdownValuelist_" + icFieldName,icValuePairs).
  END METHOD.  

  METHOD PUBLIC VOID setFilterLookup(icFieldName  AS CHAR
                                    ,icBuffersAndFields AS CHAR
                                    ,icQuery AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupQuery_" + icFieldName,icQuery).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupReturnField_" + icFieldName,icFieldName).
  END METHOD.  

  METHOD PUBLIC VOID setFilterLookup(icFieldName  AS CHAR
                                    ,icBuffersAndFields AS CHAR
                                    ,icQuery AS CHAR
                                    ,icReturnField AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupQuery_" + icFieldName,icQuery).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterLookupReturnField_" + icFieldName,icReturnField).
  END METHOD.  

  /*----------------------------------------------------------------------------
    Purpose: Create a parent relationship for the browse 
    Notes:   If field-names are different in parent object the entry in relationship
             list must be split by ; stating the child field-name first, eg
             custnum;customernum 
    @param ioParentBrowse Parent browse object
    @param icRelationShip Comma-sep. list of fields to define the relationship             
  ------------------------------------------------------------------------------*/    
  METHOD PUBLIC VOID setParentBrowseObject(ioParentBrowse AS JBoxBrowse
                                          ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentBrowse) THEN DO:
      IF VALID-OBJECT(PARENT-BROWSE-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",BROWSE-HANDLE,PARENT-BROWSE-OBJECT:BROWSE-HANDLE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION ("deleteObjectLink",BROWSE-HANDLE,ioParentBrowse:BROWSE-HANDLE).
      DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:BROWSE-HANDLE,ioParentBrowse:BROWSE-HANDLE,icRelationShip).
    END.  

    PARENT-BROWSE-OBJECT = ioParentBrowse.
                                                                                        
  END METHOD.  

  METHOD PUBLIC VOID setParentQueryObject(ioParentQuery  AS JBoxQuery
                                         ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentQuery) THEN DO:
      IF VALID-OBJECT(PARENT-QUERY-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",BROWSE-HANDLE,PARENT-QUERY-OBJECT:QUERY-HANDLE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION ("deleteObjectLink",BROWSE-HANDLE,ioParentQuery:QUERY-HANDLE).
      DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:BROWSE-HANDLE,ioParentQuery:QUERY-HANDLE,icRelationShip).
    END.  

    PARENT-QUERY-OBJECT = ioParentQuery.
  END METHOD.  

  /*----------------------------------------------------------------------------
    Purpose: Link a fill-in as column search field for the browse. 
    Notes:   The fill-in will be re-created to match the datatype of the column
             If sorting isn's specified for the browse it will be set according to 
             the search column.
    @param ihSearchField Handle for either a rectangle or a fill-in field
    @param icSearchColumn Name of initial column. If ommitted first column is assumed
  ------------------------------------------------------------------------------*/   
  METHOD PUBLIC VOID setSearchField(ihSearchField  AS HANDLE
                                   ,icSearchColumn AS CHAR):
      
    DEF VAR iColumnNum    AS INT NO-UNDO INIT 1.  
    DEF VAR hSearchField  AS HANDLE NO-UNDO.
                                     
    ASSIGN SEARCH-FIELD-HANDLE  = ihSearchField
           cSearchColumn = icSearchColumn.
    IF NOT VALID-HANDLE(BROWSE-HANDLE) OR NOT VALID-HANDLE(ihSearchField) THEN 
      RETURN.

    IF icSearchColumn NE "" THEN
      iColumnNum = DYNAMIC-FUNCTION ("getBrowseColumnNum",BROWSE-HANDLE,icSearchColumn).
    IF iColumnNum = 0 THEN iColumnNum = 1.      
    IF icSearchColumn = "" THEN
      icSearchColumn = BROWSE-HANDLE:GET-BROWSE-COLUMN(1):NAME.
                                    
    DYNAMIC-FUNCTION("deleteObjectLink",SEARCH-FIELD-HANDLE,BROWSE-HANDLE).
    
    IF DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"1stSortColumn") = "" THEN
      THIS-OBJECT:setQuerySort(icSearchColumn).                                      
    
    hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",ihSearchField,
                   BROWSE-HANDLE,iColumnNum).
    DYNAMIC-FUNCTION("createObjectLink",BROWSE-HANDLE,hSearchField).
    SEARCH-FIELD-HANDLE = hSearchField.   
    
  END METHOD.  

  METHOD PUBLIC VOID setSearchField(ihSearchField  AS HANDLE):
    THIS-OBJECT:setSearchField(ihSearchField,"").
  END METHOD.  

  METHOD PUBLIC VOID setPreScanQuery(icJoinedTable  AS CHAR
                                    ,icQueryFromJoinedToPrimaryTable AS CHAR): 
    IF NOT icQueryFromJoinedToPrimaryTable BEGINS "FIRST " 
       AND NOT icQueryFromJoinedToPrimaryTable BEGINS "EACH "                                        
       AND NOT icQueryFromJoinedToPrimaryTable BEGINS "LAST " THEN 
      icQueryFromJoinedToPrimaryTable = "EACH " + icQueryFromJoinedToPrimaryTable.
                                              
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"prescanQuery" + icJoinedTable,icQueryFromJoinedToPrimaryTable).                                      
  END METHOD.  

  METHOD PUBLIC VOID setExtraFilterField(icTable      AS CHAR
                                        ,icField      AS CHAR
                                        ,icLabel      AS CHAR
                                        ,icPrescanQry AS CHAR):
    DEF VAR cExtraFilter AS CHAR NO-UNDO.
    cExtraFilter = DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:BROWSE-HANDLE,"extraFilterFields").
    cExtraFilter = cExtraFilter + (IF cExtraFilter NE "" THEN "," ELSE "")
                 + icTable + "." + icField + "|" + icLabel.                                           
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"extraFilterFields",cExtraFilter).
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"prescanQuery" + icTable,icPrescanQry).                                      
  END METHOD.  

  METHOD PUBLIC VOID setFieldQuerySort(icFieldName  AS CHAR
                                      ,icSortExpr   AS CHAR):
  /*----------------------------------------------------------------------------
    Purpose: Replace the sort string when a browse column is sorted 
    Notes:   The sort expression might consist of more than one field, f.ex
             Sak.iSaksaar desc BY Sak.cSaksnr 
             the search column.
    @param icFieldName Name of (local) field name
    @param icSortExpr  [tablename.]fieldName [desc] [by [tablename.]fieldname..]
  ------------------------------------------------------------------------------*/   
    icSortExpr = REPLACE(icSortExpr," desc "," <desc> ").                                      
    icSortExpr = REPLACE(icSortExpr," descending "," <desc> ").                                      
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"myQuerySort" + icFieldName,icSortExpr).                                      
  END METHOD.  

  METHOD PUBLIC CHARACTER getFieldQuerySort(icFieldName  AS CHAR):
    RETURN DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:BROWSE-HANDLE,"myQuerySort" + icFieldName).                                      
  END METHOD.
    
  METHOD PUBLIC VOID setFieldQueryIndex(icFieldName  AS CHAR
                                       ,icIndexName  AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BROWSE-HANDLE,"use-index_" + icFieldName,icIndexName).                                      
  END METHOD.  

  METHOD PUBLIC CHARACTER getFieldQueryIndex(icFieldName  AS CHAR):
    RETURN DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:BROWSE-HANDLE,"use_index_" + icFieldName).                                      
  END METHOD.  

  METHOD PUBLIC VOID registerOverlay(INPUT ioOverlay   AS Progress.Lang.Object
                                    ,INPUT icType      AS CHAR
                                    ,INPUT icBrowseColumn AS CHAR
                                    ,INPUT ihOverlay   AS HANDLE):
            
    FIND FIRST ttOverlay 
         WHERE ttOverlay.oOverlay = ioOverlay 
         NO-ERROR.
    IF NOT AVAIL ttOverlay THEN DO:
      CREATE ttOverlay.
      ASSIGN ttOverlay.oOverlay      = ioOverlay
             ttOverlay.cType         = icType
             ttOverlay.cBrowseColumn = icBrowseColumn
             ttOverlay.hOverlay      = ihOverlay
             ttOverlay.bInitRefrRow  = LOGICAL(DYNAMIC-FUNCTION("getAttribute",ihOverlay,"refreshRow"))
             ttOverlay.cBufferColumn = DYNAMIC-FUNCTION("getAttribute",ihOverlay,"buffercolumn")
             .
    END.                                      
  END METHOD.                                         

  METHOD PUBLIC CHARACTER getOverlaySCREEN-VALUE(icBrowseColumn AS CHAR):
    FIND FIRST ttOverlay 
         WHERE ttOverlay.cBrowseColumn = icBrowseColumn
         NO-ERROR.
    IF AVAIL ttOverlay AND VALID-OBJECT(ttOverlay.oOverlay) THEN DO:
      CASE ttOverlay.cType:
        WHEN "JBoxBrowseFillIn" THEN DO:
          DEF VAR oBrowseFillIn AS JBoxBrowseFillIn NO-UNDO.
          oBrowseFillIn = CAST(ttOverlay.oOverlay,"JBoxBrowseFillIn").
          RETURN oBrowseFillIn:SCREEN-VALUE.
        END.
        WHEN "JBoxBrowseDropDown" THEN DO:
          DEF VAR oBrowseDropDown AS JBoxBrowseDropDown NO-UNDO.
          oBrowseDropDown = CAST(ttOverlay.oOverlay,"JBoxBrowseDropDown").
          RETURN oBrowseDropDown:SCREEN-VALUE.
        END.
      END CASE.
    END.  
    ELSE RETURN ?.     
  END METHOD. 

  METHOD PUBLIC LOGICAL setOverlaySCREEN-VALUE(icBrowseColumn AS CHAR
                                              ,icScreenValue  AS CHAR):
    FIND FIRST ttOverlay 
         WHERE ttOverlay.cBrowseColumn = icBrowseColumn
         NO-ERROR.
    IF AVAIL ttOverlay AND VALID-OBJECT(ttOverlay.oOverlay) THEN DO:
      CASE ttOverlay.cType:
        WHEN "JBoxBrowseFillIn" THEN DO:
          DEF VAR oBrowseFillIn AS JBoxBrowseFillIn NO-UNDO.
          oBrowseFillIn = CAST(ttOverlay.oOverlay,"JBoxBrowseFillIn") NO-ERROR.
          IF VALID-OBJECT(oBrowseFillIn) THEN DO:
            oBrowseFillIn:SCREEN-VALUE = icScreenValue.
            RETURN YES.
          END.
          ELSE RETURN NO.
        END.
        WHEN "JBoxBrowseDropDown" THEN DO:
          DEF VAR oBrowseDropDown AS JBoxBrowseDropDown NO-UNDO.
          oBrowseDropDown = CAST(ttOverlay.oOverlay,"JBoxBrowseDropDown") NO-ERROR.
          IF VALID-OBJECT(oBrowseDropDown) THEN DO:
            oBrowseDropDown:SCREEN-VALUE = icScreenValue.
            RETURN YES.
          END.  
          ELSE RETURN NO.
        END.
      END CASE.
    END.  
    ELSE RETURN NO.     
  END METHOD. 
  
  METHOD PUBLIC HANDLE getOverlayHandle (INPUT icBrowseColumn AS CHAR).
    DEF VAR hOverlay AS HANDLE NO-UNDO.
  
    FIND FIRST ttOverlay WHERE ttOverlay.cBrowseColumn = icBrowseColumn
         NO-ERROR.
    IF AVAIL ttOverlay AND VALID-HANDLE(ttOverlay.hOverlay) THEN RETURN ttOverlay.hOverlay.
    ELSE IF AVAIL ttOverlay THEN 
      hOverlay = DYNAMIC-FUNCTION("getObjectByNameAndType",hSourceProc:CURRENT-WINDOW,ttOverlay.cBrowseColumn,ttOverlay.cType).
      
    RETURN hOverlay.
  
  END METHOD.

  METHOD PUBLIC HANDLE getPopupToolHandle (icTool  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"menu-item" + icTool)) NO-ERROR.
    RETURN hWidget.
  END METHOD.

/*
  METHOD PUBLIC LOGICAL setFilterDropDown(icFieldName AS CHAR
                                         ,icDrowDownTablesAndFlds AS CHAR
                                         ,icDrowDownQuery AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropDownFields_" + icFieldName,icDrowDownTablesAndFlds).                                       
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropDownQuery_" + icFieldName,icDrowDownQuery).                                       
  END METHOD.
  METHOD PUBLIC LOGICAL setFilterDropDown(icFieldName AS CHAR
                                         ,icDrowDownValueList AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"filterDropDownValueList_" + icFieldName,icDrowDownValueList).                                       
  END METHOD.
*/  

  METHOD PUBLIC CHARACTER getQueryStatValue(icFieldName AS CHAR):
    IF THIS-OBJECT:useLocalData AND DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"localStatValue" + icFieldName) NE "" THEN
      RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"localStatValue" + icFieldName).
    ELSE  
      RETURN DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,"statValue" + icFieldName).
  END METHOD.

  METHOD PUBLIC CHARACTER getLocalStat(icFieldList AS CHAR):
    DEF VAR cReturn AS CHAR NO-UNDO.
    cReturn = DYNAMIC-FUNCTION("getLocalQueryStat",BROWSE-HANDLE,icFieldList).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"recordcount",ENTRY(2,ENTRY(1,cReturn,";"),"|")).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"rowsadded","").
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"rowsdeleted","").
    
    DYNAMIC-FUNCTION("ViewRecordCount",BROWSE-HANDLE).
    
    IF NUM-ENTRIES(icFieldList) > 1 THEN
      RETURN SUBSTR(cReturn,INDEX(cReturn,";") + 1).
    ELSE    
      RETURN ENTRY(2,ENTRY(2,cReturn,";"),"|").
      
  END METHOD.

  METHOD PUBLIC CHARACTER getLocalStat():
    DEF VAR cReturn AS CHAR NO-UNDO.
    cReturn = DYNAMIC-FUNCTION("getLocalQueryStat",BROWSE-HANDLE,"").
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"rowsadded","").
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"rowsdeleted","").
    
    DYNAMIC-FUNCTION("ViewRecordCount",BROWSE-HANDLE).
    RETURN cReturn.
  END METHOD.

  /* Process selected rows: */ 
  METHOD PUBLIC LOGICAL processRows(icProcName AS CHAR
                                   ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.                                           
    IF cCallFromMethod = "" THEN cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").
    bOk = DYNAMIC-FUNCTION("processSelectedRows",BROWSE-HANDLE,icProcName,icParam).    
    JBoxServerAPI:Instance:CreateCall(cCallFromMethod,icParam,bOk,?,hSourceProc).
    cCallFromMethod = "".    
    RETURN bOk.
  END METHOD.

  METHOD PUBLIC LOGICAL processRowsNoRefresh(icProcName AS CHAR
                                            ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.                                           
    cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"RefreshProcessedRows","no").    
    bOk = THIS-OBJECT:processRows(icProcName,icParam).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"RefreshProcessedRows","").
    RETURN bOK.
  END METHOD.

  METHOD PUBLIC LOGICAL processRowsNoMessage(icProcName AS CHAR
                                            ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.                    
    cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").                       
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"viewErrorsFromProcessRows","no").
    bOk = THIS-OBJECT:processRows(icProcName,icParam).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"viewErrorsFromProcessRows","").
    RETURN bOK.
  END METHOD.

  /* Process all rows for query crit: */ 
  METHOD PUBLIC LOGICAL processSet(icProcName AS CHAR
                                  ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.       
    IF cCallFromMethod = "" THEN cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").                                    
    bOk = DYNAMIC-FUNCTION("processQuery",BROWSE-HANDLE,icProcName,icParam).
    JBoxServerAPI:Instance:CreateCall(cCallFromMethod,icParam,bOk,?,hSourceProc).
    cCallFromMethod = "".
    RETURN bOk.
  END METHOD.

  METHOD PUBLIC LOGICAL processSetNoMessage(icProcName AS CHAR
                                           ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.                    
    cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").                       
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"viewErrorsFromProcessRows","no").
    bOk = THIS-OBJECT:processSet(icProcName,icParam).
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,"viewErrorsFromProcessRows","").
    RETURN bOK.
  END METHOD.

  METHOD PUBLIC VOID setJBoxAttribute(icAttribute AS CHAR
                                     ,icValue AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BROWSE-HANDLE,icAttribute,icValue).
  END METHOD.
  METHOD PUBLIC CHARACTER getJBoxAttribute(icAttribute AS CHAR):
    RETURN  DYNAMIC-FUNCTION("getAttribute",BROWSE-HANDLE,icAttribute).
  END METHOD.

  METHOD PUBLIC VOID applyDefaultAction():
    RUN InvokeMethod(BROWSE-HANDLE,"DefaultActionBrowse").
  END METHOD.

  METHOD PUBLIC HANDLE getColumnHandle(INPUT icColumnName AS CHAR):
    RETURN DYNAMIC-FUNCTION("getBrowseColumn",BROWSE-HANDLE,icColumnName).
  END METHOD.

  METHOD PUBLIC VOID selectAllRows():
    RUN InvokeMethod(BROWSE-HANDLE,"selectAllRowsRecord").
  END METHOD.

  METHOD PUBLIC VOID deselectRow():
    RUN InvokeMethod(BROWSE-HANDLE,"deselectRowRecord").
  END METHOD.

  METHOD PUBLIC VOID setNoResizeX():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.  
  METHOD PUBLIC VOID setAddResizeX():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.  
              
  METHOD PUBLIC VOID setNoMoveX():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY():
    IF VALID-HANDLE(BROWSE-HANDLE) THEN
      PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hBrowse:FRAME,BROWSE-HANDLE:NAME).
    ELSE MethPropMsg(PROGRAM-NAME(1)).  
  END METHOD.
  
  METHOD PRIVATE VOID MethPropMsg (INPUT icCall AS CHAR):
    MESSAGE "Resize setting " icCall SKIP "cannot be called until browse widget is created"
            VIEW-AS ALERT-BOX.
  END METHOD.  
    
  METHOD PRIVATE LOGICAL resolveFilterExpr(icExprList AS CHAR,OUTPUT ocFields AS CHAR,OUTPUT ocOperators AS CHAR,OUTPUT ocValues AS CHAR):
    DEF VAR ix    AS INT  NO-UNDO.
    DEF VAR cExpr AS CHAR NO-UNDO.
    DEF VAR cOper AS CHAR NO-UNDO.
    
    
    DO ix = 1 TO NUM-ENTRIES(icExprList,"|"):
      
      ASSIGN cExpr       = ENTRY(ix,icExprList,"|")
             ocFields    = ocFields + (IF ocFields NE "" THEN "," ELSE "") + entry(1,cExpr," ")
             cOper       = THIS-OBJECT:getFilterOper(ENTRY(2,cExpr," ")) 
             ocOperators = ocOperators + (IF ocOperators NE "" THEN "," ELSE "") + cOper
             ocValues    = ocValues + (IF ocValues NE "" THEN "|" ELSE "") + replace(REPLACE(TRIM(substr(cExpr,INDEX(cExpr,cOper) + length(cOper))),"'",""),'"','"')
             .
    END.
    
    RETURN YES.
  END METHOD.  

  METHOD PUBLIC LOGICAL getActionPermission(INPUT icAction AS CHAR):
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    RUN JBoxLogFuncToMeth.p ("getActionPermission"
                            ,"CHARACTER,CHARACTER,CHARACTER"
                            ,hSourceProc:FILE-NAME + CHR(1) + BROWSE-HANDLE:NAME + CHR(1) + icAction
                            ,?
                            ,OUTPUT bOk).
    RETURN bOk.                        
  END.
  
  METHOD PUBLIC LOGICAL updateRow(INPUT icFields AS CHAR
                                 ,INPUT icValues AS CHAR):
                                   
    IF THIS-OBJECT:BUFFER-HANDLE:AVAIL THEN DO: 
      IF JBoxServerAPI:Instance:Update(ENTRY(1,cBufferList),THIS-OBJECT:BUFFER-HANDLE::RowIdent1,icFields,icValues) THEN DO:
        THIS-OBJECT:refreshRow().
        RETURN YES.
      END.  
      ELSE RETURN NO.
    END.
    ELSE RETURN NO.                                     
  END METHOD.

  METHOD PUBLIC LOGICAL updateSelectedRows(INPUT icFields AS CHAR
                                          ,INPUT icValues AS CHAR):
                                  
    DEF VAR ix AS INT NO-UNDO.
    
    DO ix = 1 TO THIS-OBJECT:BROWSE-HANDLE:NUM-SELECTED-ROWS:
                                   
      IF THIS-OBJECT:BROWSE-HANDLE:FETCH-SELECTED-ROW(ix) THEN DO: 
        IF NOT JBoxServerAPI:Instance:Update(ENTRY(1,cBufferList),THIS-OBJECT:BUFFER-HANDLE::RowIdent1,icFields,icValues,NO,
                                             ix = THIS-OBJECT:BROWSE-HANDLE:NUM-SELECTED-ROWS) THEN 
          RETURN NO.
      END.    
    END.
    THIS-OBJECT:refreshSelectedRows().
    RETURN YES.
  END METHOD.

  METHOD PUBLIC VOID setBrowseHandle(ihBrowse AS HANDLE):
    IF THIS-OBJECT:BROWSE-HANDLE NE ihBrowse THEN DO:
      DYNAMIC-FUNCTION ("swapObjectHandle",THIS-OBJECT:BROWSE-HANDLE,ihBrowse).
      THIS-OBJECT:BROWSE-HANDLE = ihBrowse.
    END.   
  END METHOD.  

  DESTRUCTOR PUBLIC JBoxBrowse ( ):
  	DYNAMIC-FUNCTION("DeleteObject",BROWSE-HANDLE) NO-ERROR.
  END DESTRUCTOR.

END CLASS.
