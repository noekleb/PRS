 
 /*------------------------------------------------------------------------
    File        : JBoxStatusBar
    Purpose     : Create status-bar and manage it's content
    Syntax      : Default (no parameter) is 3 fields where field 1 is HelpText, 2 is RecordSelectText and 3 is RecordCountText
                  To increase number of detail fields supply a list of integer numbers for width of fields 2..n. Field 1 (Help) will use remaining space
                  When more that 2 fields RecordSelectText is field # n - 1 and RecordCountText is field # n
                  To access fields 2 to n - 2 use properties StatusText2,StatusText3, .. (up to 5) 
                  When 2 fields RecordCountText is field # 2
                  To have only one field (Help) call constructor with "" as parameter
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Aug 21 12:43:22 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
/*USING oo.* FROM PROPATH.*/

CLASS JBoxStatusBar:
  DEF VAR hSourceProc AS HANDLE NO-UNDO.
  DEF VAR hHelpText          AS HANDLE NO-UNDO.
  DEF VAR hRecordCountText   AS HANDLE NO-UNDO.
  DEF VAR hRecordSelectText  AS HANDLE NO-UNDO.
  DEF VAR hStatusText2       AS HANDLE NO-UNDO.
  DEF VAR hStatusText3       AS HANDLE NO-UNDO.
  DEF VAR hStatusText4       AS HANDLE NO-UNDO.
  DEF VAR hStatusText5       AS HANDLE NO-UNDO.
  
  DEF PUBLIC PROPERTY HelpText  AS CHAR 
      GET(): RETURN hHelpText:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        hHelpText:SCREEN-VALUE = icScreenValue.
      END SET.
      
  DEF PUBLIC PROPERTY RecordCountText  AS CHAR 
      GET(): RETURN TRIM(hRecordCountText:SCREEN-VALUE). 
      END GET. 
      SET(icScreenValue AS CHAR):
        DEF VAR iNumZ AS INT NO-UNDO.
        iNumZ = INT(icScreenValue) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          ASSIGN iNumZ = (hRecordCountText:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("..9",hRecordCountText:FONT)) 
                          / FONT-TABLE:GET-TEXT-WIDTH-PIXELS("z",hRecordCountText:FONT)
                 hRecordCountText:SCREEN-VALUE = STRING(INT(icScreenValue),FILL("z",iNumZ) + "9").
        ELSE         
          hRecordCountText:SCREEN-VALUE = icScreenValue.
      END SET.
      
  DEF PUBLIC PROPERTY RecordSelectText  AS CHAR 
      GET(): RETURN hRecordSelectText:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        DEF VAR iNumZ AS INT NO-UNDO.
        iNumZ = INT(icScreenValue) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          ASSIGN iNumZ = (hRecordSelectText:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("..9",hRecordSelectText:FONT)) 
                          / FONT-TABLE:GET-TEXT-WIDTH-PIXELS("z",hRecordSelectText:FONT)
                 hRecordSelectText:SCREEN-VALUE = STRING(INT(icScreenValue),FILL("z",iNumZ) + "9").
        ELSE         
          hRecordSelectText:SCREEN-VALUE = icScreenValue.
      END SET.
      
  DEF PUBLIC PROPERTY StatusText2  AS CHAR 
      GET(): RETURN hStatusText2:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        hStatusText2:SCREEN-VALUE = icScreenValue.
      END SET.
      
  DEF PUBLIC PROPERTY StatusText3  AS CHAR 
      GET(): RETURN hStatusText3:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        hStatusText3:SCREEN-VALUE = icScreenValue.
      END SET.

  DEF PUBLIC PROPERTY StatusText4  AS CHAR 
      GET(): RETURN hStatusText4:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        hStatusText4:SCREEN-VALUE = icScreenValue.
      END SET.
      
  DEF PUBLIC PROPERTY StatusText5  AS CHAR 
      GET(): RETURN hStatusText5:SCREEN-VALUE. 
      END GET. 
      SET(icScreenValue AS CHAR):
        hStatusText5:SCREEN-VALUE = icScreenValue.
      END SET.

  CONSTRUCTOR PUBLIC JBoxStatusBar ():
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    InitializeComponent("",0,YES,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxStatusBar (ihSourceProc AS HANDLE):
      SUPER ().

    hSourceProc = ihSourceProc.
    
    InitializeComponent("",0,YES,?).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxStatusBar (icDetailPixelList AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent("," + icDetailPixelList,0,YES,?).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxStatusBar (icDetailPixelList AS CHAR
                                   ,ihSourceProc AS HANDLE):
      SUPER ().

    hSourceProc = ihSourceProc.

    InitializeComponent("," + icDetailPixelList,0,YES,?).                 
      
  END CONSTRUCTOR.
  
  METHOD PRIVATE VOID initializeComponent(icFieldPixelList AS CHARACTER
                                         ,iiNumFields      AS INTEGER
                                         ,ibTopLine        AS LOGICAL
                                         ,iiFont           AS INT):
           
    DYNAMIC-FUNCTION("CreateStatusBar",hSourceProc:CURRENT-WINDOW,icFieldPixelList,iiNumFields,ibTopLine,iiFont).
    
    hHelpText = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"HelpTextWidget")).
    hRecordCountText = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"RecordCountWidget")) NO-ERROR.
    hRecordSelectText = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"RecordSelectWidget")) NO-ERROR.
    hStatusText2 = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"statusBarTextFillIn2")) NO-ERROR.
    hStatusText3 = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"statusBarTextFillIn3")) NO-ERROR.
    hStatusText4 = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"statusBarTextFillIn4")) NO-ERROR.
    hStatusText5 = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hSourceProc:CURRENT-WINDOW,"statusBarTextFillIn5")) NO-ERROR.
    
  END METHOD.                                       

END CLASS.