 
 /*------------------------------------------------------------------------
    File        : JBoxBrowseFillIn
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Aug 21 12:43:22 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
/*USING oo.* FROM PROPATH.*/

CLASS JBoxBrowseFillIn:
  DEF VAR hSourceProc    AS HANDLE NO-UNDO.

  DEF PUBLIC PROPERTY OVERLAY-HANDLE AS HANDLE
      GET(): RETURN OVERLAY-HANDLE. END GET.
      PROTECTED SET. 
  
  DEF PUBLIC PROPERTY SCREEN-VALUE  AS CHAR 
      GET(): RETURN IF VALID-HANDLE(OVERLAY-HANDLE) THEN OVERLAY-HANDLE:SCREEN-VALUE ELSE ?. 
      END GET. 
      SET(icScreenValue AS CHAR):
        OVERLAY-HANDLE:SCREEN-VALUE = icScreenValue.
      END SET.

  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,icTableColumn       AS CHAR
                                      ,icLookupTblsAndFlds AS CHAR
                                      ,icLookupCriteria    AS CHAR
                                      ,icLookupReturnField AS CHAR
                                      ,icValidation        AS CHAR
                                      ,ibRefreshRow        AS LOG):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icTableColumn,icLookupTblsAndFlds,icLookupCriteria,icLookupReturnField,icValidation,ibRefreshRow).                 
      
  END CONSTRUCTOR.
   
  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,icTableColumn       AS CHAR
                                      ,icLookupTblsAndFlds AS CHAR
                                      ,icLookupCriteria    AS CHAR
                                      ,icLookupReturnField AS CHAR
                                      ,icValidation        AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icTableColumn,icLookupTblsAndFlds,icLookupCriteria,icLookupReturnField,icValidation,YES).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,icTableColumn       AS CHAR
                                      ,icLookupTblsAndFlds AS CHAR
                                      ,icLookupCriteria    AS CHAR
                                      ,icLookupReturnField AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icTableColumn,icLookupTblsAndFlds,icLookupCriteria,icLookupReturnField,"",YES).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,icLookupTblsAndFlds AS CHAR
                                      ,icLookupCriteria    AS CHAR
                                      ,icLookupReturnField AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icBrowseColumn,icLookupTblsAndFlds,icLookupCriteria,icLookupReturnField,"",YES).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,ihSourceProcedure   AS HANDLE
                                      ,ibRefreshRow        AS LOG):
      SUPER ().

    hSourceProc = ihSourceProcedure.

    InitializeComponent(ioBrowse,icBrowseColumn,icBrowseColumn,"","","","",ibRefreshRow).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR
                                      ,ibRefreshRow        AS LOG):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icBrowseColumn,"","","","",ibRefreshRow).                 
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxBrowseFillIn (ioBrowse            AS JBoxBrowse
                                      ,icBrowseColumn      AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ioBrowse,icBrowseColumn,icBrowseColumn,"","","","",YES).                 
      
  END CONSTRUCTOR.
  
  METHOD PRIVATE VOID initializeComponent(ioBrowse            AS JBoxBrowse
                                         ,icBrowseColumn      AS CHAR
                                         ,icTableColumn       AS CHAR
                                         ,icLookupTblsAndFlds AS CHAR
                                         ,icLookupCriteria    AS CHAR
                                         ,icLookupReturnField AS CHAR
                                         ,icValidation        AS CHAR
                                         ,ibRefreshRow        AS LOG):
           
    
    DYNAMIC-FUNCTION("setUseBrowseColumnFormat",YES) NO-ERROR.
      
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    
    OVERLAY-HANDLE  = DYNAMIC-FUNCTION("NewBrowseFillIn",ioBrowse:BROWSE-HANDLE,icBrowseColumn,icTableColumn
               ,icLookupTblsAndFlds,icLookupCriteria,icLookupReturnField
               ,icValidation).
    DYNAMIC-FUNCTION("CreateOverlayLink",ioBrowse:BROWSE-HANDLE,OVERLAY-HANDLE,icBrowseColumn).
    
    IF ibRefreshRow THEN
      DYNAMIC-FUNCTION("setAttribute",OVERLAY-HANDLE,"refreshRow","yes").
      
    ioBrowse:registerOverlay(THIS-OBJECT,"JBoxBrowseFillIn",icBrowseColumn,OVERLAY-HANDLE).               
  END METHOD.                                       

END CLASS.