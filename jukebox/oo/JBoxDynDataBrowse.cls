 
 /*------------------------------------------------------------------------
    File        : JBoxDynDataBrowse
    Purpose     : Dynamic databrowse with drill-down capabilities
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Oct 25 12:43:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxDynDataBrowse:
  DEF VAR hToolbarRect                  AS HANDLE NO-UNDO.
  DEF PUBLIC PROPERTY BROWSE-OBJECT     AS JBoxBrowse GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY TOOLBAR-OBJECT    AS JBoxToolbar GET. PROTECTED SET.
  DEF PUBLIC PROPERTY POPUP-MENU-OBJECT AS JBoxPopupMenu GET. PROTECTED SET.
  DEF PUBLIC PROPERTY DataBrwProc       AS HANDLE GET. SET.
  DEF PUBLIC PROPERTY ParentProc        AS HANDLE GET. SET. 
  DEF PUBLIC PROPERTY BrowseRect        AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY getInitialRecordCount AS LOG INIT YES GET. SET. 
  
  DEF PUBLIC PROPERTY EnableSaveQuery AS LOG 
      PROTECTED GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setEnableSaveQuery" IN DataBrwProc,ibSetting).
      END SET.

  DEF PUBLIC PROPERTY TotalFields AS CHAR 
      PROTECTED GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setTotalFields" IN DataBrwProc,icSetting).
      END SET.

  DEF PUBLIC PROPERTY CalcFieldProc AS CHAR GET. 
      SET(icSetting AS CHAR):
        IF VALID-OBJECT(THIS-OBJECT:BROWSE-OBJECT) THEN
          THIS-OBJECT:BROWSE-OBJECT:calcFieldProc = icSetting.
      END SET. 
  
  DEF PUBLIC PROPERTY userSettingContext AS CHAR GET. 
      SET(icSetting AS CHAR):
        IF VALID-OBJECT(THIS-OBJECT:BROWSE-OBJECT) THEN
          THIS-OBJECT:BROWSE-OBJECT:userSettingContext = icSetting.
      END SET. 
    
  CONSTRUCTOR PUBLIC JBoxDynDataBrowse(INPUT iDataBrwProc  AS HANDLE
                                      ,INPUT iParentProc      AS HANDLE
                                      ,INPUT ihBrowseRect  AS HANDLE
                                      ,INPUT ihToolbarRect AS HANDLE
                                      ,INPUT ihSearchRect  AS HANDLE):
    ASSIGN DataBrwProc  = iDataBrwProc
           ParentProc   = iParentProc
           hToolbarRect = ihToolbarRect
           BrowseRect   = ihBrowseRect
           .
    
/*    BROWSE-OBJECT = NEW JBoxBrowse(ihBrowseRect,iParentProc,YES).*/
  END CONSTRUCTOR.
                               
  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icQuery            AS CHAR
                                        ,icInitialSort      AS CHAR
                                        ,ibRecordCount      AS LOG
                                        ,ihTempTable        AS HANDLE):
                                          
    DEF VAR cNoDisplayFields AS CHAR NO-UNDO.
    DEF VAR ix               AS INT  NO-UNDO.
    DEF VAR hBuffer          AS HANDLE NO-UNDO.

    IF VALID-HANDLE(ihTempTable) THEN DO:
      
      IF icBuffersAndFields NE "" THEN DO:
        IF ihTempTable:TYPE = "buffer" THEN
          hBuffer = ihTempTable.
        ELSE
          hBuffer = ihTempTable:DEFAULT-BUFFER-HANDLE.
        
        IF NUM-ENTRIES(icBuffersAndFields,";") > 1 THEN DO:
          DO ix = 1 TO hBuffer:NUM-FIELDS:
            IF LOOKUP(hBuffer:BUFFER-FIELD (ix):NAME,icBuffersAndFields,";") = 0 THEN
              cNoDisplayFields = cNoDisplayFields + (IF cNoDisplayFields NE "" THEN "," ELSE "") + hBuffer:BUFFER-FIELD (ix):NAME.
          END.     
          DYNAMIC-FUNCTION("setAttribute",BrowseRect,"NoDisplayFields",cNoDisplayFields).
        END.
      END.  
      /* Creates the browse placeholder object. Actual browse widget is created in InitializeObject in DataBrwProc */ 
      DYNAMIC-FUNCTION("setAttribute",BrowseRect,"dataBrowseObject","yes"). /* <- until the actual browse is created the rectangle holds internal attr.settings */
      BROWSE-OBJECT = NEW JBoxBrowse(BrowseRect,ParentProc,YES,ihTempTable,YES). /* Last parameter: set dataBrowseObject type */
      BROWSE-OBJECT:useLocalData = YES.
    END.  
    ELSE      
      BROWSE-OBJECT = NEW JBoxBrowse(BrowseRect,ParentProc,YES).      
    
    BROWSE-OBJECT:calcFieldProc = THIS-OBJECT:CalcFieldProc.
    BROWSE-OBJECT:userSettingContext = THIS-OBJECT:userSettingContext.            
                
    DYNAMIC-FUNCTION("setObjectSourceProc",DataBrwProc).
    
    RUN InitializeObject IN DataBrwProc (icBuffersAndFields,icQuery,icInitialSort,ibRecordCount AND getInitialRecordCount).
                                          
    THIS-OBJECT:BROWSE-OBJECT:setBrowseHandle(DYNAMIC-FUNCTION("getBrowseHandle" IN DataBrwProc)).
    DYNAMIC-FUNCTION("setObjectSourceFileHandle",DYNAMIC-FUNCTION("getBrowseHandle" IN DataBrwProc),DataBrwProc).
    TOOLBAR-OBJECT = NEW JBoxToolbar(hToolbarRect,ParentProc).
    BROWSE-OBJECT:TOOLBAR-OBJECT = TOOLBAR-OBJECT.
    POPUP-MENU-OBJECT = NEW JBoxPopupMenu(THIS-OBJECT:BROWSE-OBJECT:BROWSE-HANDLE:POPUP-MENU,ParentProc).
    BROWSE-OBJECT:POPUP-MENU-OBJECT = POPUP-MENU-OBJECT. 
    
    PUBLISH "SizeToFitTab" FROM DataBrwProc (DataBrwProc:CURRENT-WINDOW).
    
  END METHOD.

  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icQuery            AS CHAR
                                        ,icInitialSort      AS CHAR
                                        ,ibRecordCount      AS LOG):
    THIS-OBJECT:InitializeQuery(icBuffersAndFields,icQuery,icInitialSort,ibRecordCount,?).
  END METHOD.

  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icQuery            AS CHAR
                                        ,ibRecordCount      AS LOG):
    THIS-OBJECT:InitializeQuery(icBuffersAndFields,icQuery,"",ibRecordCount,?).
  END METHOD.
  
  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icQuery            AS CHAR
                                        ,icInitialSort      AS CHAR):
    THIS-OBJECT:InitializeQuery(icBuffersAndFields,icQuery,icInitialSort,NO,?).
  END METHOD.

  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icQuery            AS CHAR):
    THIS-OBJECT:InitializeQuery(icBuffersAndFields,icQuery,"",NO,?).
  END METHOD.
  
  METHOD PUBLIC LOGICAL InitializeQuery (icBuffersAndFields AS CHAR
                                        ,icInitialSort      AS CHAR
                                        ,ihTempTable        AS HANDLE):
    THIS-OBJECT:InitializeQuery(icBuffersAndFields,"",icInitialSort,YES,ihTempTable).
  END METHOD.
   
  METHOD PUBLIC LOGICAL StartQuery (INPUT icFilterFields    AS CHAR,
                                    INPUT icFilterOperators AS CHAR,
                                    INPUT icFilterValues    AS CHAR,
                                    INPUT icInitSort        AS CHAR,
                                    INPUT icDistinctCols    AS CHAR,
                                    INPUT icAccumFields     AS CHAR):
      
    DYNAMIC-FUNCTION("StartQueryCls" IN DataBrwProc
                     ,icFilterFields   /* Filter fields (comma sep) */
                     ,icFilterOperators   /* Filter operators (comma sep) */
                     ,icFilterValues   /* Filter values (pipe sep) */
                     ,icInitSort   /* Initial sort column(s) (comma sep) */
                     ,icDistinctCols   /* Distinct columns (comma sep) */
                     ,icAccumFields   /* Columns to accumulate (comma sep) */
                     ).
  END METHOD.
      
  DESTRUCTOR PUBLIC JBoxDynDataBrowse ( ):
  END DESTRUCTOR.
END CLASS.
  