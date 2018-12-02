 /*------------------------------------------------------------------------
    File        : JBoxProcedure
    Purpose     : Help manage the content, resize settings and such for a procedure, ie suppressed window
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Feb 19 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxProcedure:
  DEF TEMP-TABLE ttChildWin 
      FIELD hChildWin AS HANDLE.
      
/*  DEF TEMP-TABLE ttFieldValue*/
/*      FIELD cBuffer AS CHAR  */
/*      FIELD cField  AS CHAR  */
/*      FIELD cValue  AS CHAR. */
      
  DEF PUBLIC PROPERTY PROCEDURE-HANDLE AS HANDLE GET. PROTECTED SET.

  DEF PUBLIC PROPERTY CURRENT-WINDOW AS HANDLE
      GET(): RETURN PROCEDURE-HANDLE:CURRENT-WINDOW. 
      END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY CONTAINER-HANDLE AS HANDLE GET. PROTECTED SET.

  DEF PUBLIC PROPERTY CONTAINER-OBJECT AS JBoxContainer GET. PROTECTED SET.

  DEF PUBLIC PROPERTY FRAME-HANDLE AS HANDLE GET. 
      SET(ihFrame AS HANDLE): 
        FRAME-HANDLE = ihFrame. 
      END SET.
      
  CONSTRUCTOR PUBLIC JBoxProcedure ():
      SUPER ().

    IF PROVERSION < "11" THEN PROCEDURE-HANDLE = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE PROCEDURE-HANDLE = SOURCE-PROCEDURE.

    DYNAMIC-FUNCTION("setObjectSourceProc",PROCEDURE-HANDLE).    
    DYNAMIC-FUNCTION("NewObject",PROCEDURE-HANDLE:CURRENT-WINDOW,PROCEDURE-HANDLE,"Procedure").

    PUBLISH "getWindowContainerHandle" FROM PROCEDURE-HANDLE (PROCEDURE-HANDLE:CURRENT-WINDOW,OUTPUT CONTAINER-HANDLE).
    PUBLISH "getWindowContainerObject" FROM PROCEDURE-HANDLE (CONTAINER-HANDLE,OUTPUT CONTAINER-OBJECT).

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxProcedure (INPUT ihFrame AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN PROCEDURE-HANDLE = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE PROCEDURE-HANDLE = SOURCE-PROCEDURE.

    DYNAMIC-FUNCTION("setObjectSourceProc",PROCEDURE-HANDLE).    
    DYNAMIC-FUNCTION("NewObject",PROCEDURE-HANDLE:CURRENT-WINDOW,PROCEDURE-HANDLE,"Procedure").
    FRAME-HANDLE = ihFrame.

    PUBLISH "getWindowContainerHandle" FROM PROCEDURE-HANDLE (PROCEDURE-HANDLE:CURRENT-WINDOW,OUTPUT CONTAINER-HANDLE).
    PUBLISH "getWindowContainerObject" FROM PROCEDURE-HANDLE (CONTAINER-HANDLE,OUTPUT CONTAINER-OBJECT).

  END CONSTRUCTOR.

  METHOD PUBLIC LOGICAL bringContainerToFront():
    IF VALID-HANDLE(CONTAINER-HANDLE) THEN DO:
      PUBLISH "ViewTabProcHandle" FROM PROCEDURE-HANDLE (CONTAINER-HANDLE).
      THIS-OBJECT:unFreezeContainer().
      RETURN YES.
    END.
    ELSE IF VALID-HANDLE(CURRENT-WINDOW) THEN CURRENT-WINDOW:MOVE-TO-TOP().
    ELSE RETURN NO.  
  END METHOD.  

  METHOD PUBLIC LOGICAL freezeContainer():
    IF VALID-HANDLE(CURRENT-WINDOW) THEN DO:
      DYNAMIC-FUNCTION("DoLockWindow",CURRENT-WINDOW).
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","yes").
      RETURN YES.
    END.
    ELSE RETURN NO.  
  END METHOD.  

  METHOD PUBLIC LOGICAL unFreezeContainer():
    IF VALID-HANDLE(CURRENT-WINDOW) THEN DO:
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userKeepsWindowLocked","").
      DYNAMIC-FUNCTION("DoLockWindow",?).
      RETURN YES.
    END.
    ELSE RETURN NO.  
  END METHOD.  

  METHOD PUBLIC HANDLE StartChildWindow (icProcName     AS CHAR
                                        ,icWinTitle     AS CHAR
                                        ,ibNew          AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.

    PUBLISH "StartChildWindow" FROM PROCEDURE-HANDLE(icProcName,icWinTitle,PROCEDURE-HANDLE,ibNew,OUTPUT ohWinHandle).
    FIND FIRST ttChildWin WHERE ttChildWin.hChildWin = ohWinHandle
         NO-ERROR.
    IF NOT AVAIL ttChildWin THEN DO:
      CREATE ttChildWin.
      ttChildWin.hChildWin = ohWinHandle.
    END.
    RETURN ohWinHandle.

  END METHOD.

  METHOD PUBLIC HANDLE StartChildTab (icProcName     AS CHAR
                                     ,icTabTitle     AS CHAR
                                     ,ibNew          AS HANDLE
                                     ,ibKeepOpen     AS LOG).
    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.

    PUBLISH "StartChildTab" FROM PROCEDURE-HANDLE(icProcName,icTabTitle,PROCEDURE-HANDLE,ibNew,ibKeepOpen,"",OUTPUT ohWinHandle).
    RETURN ohWinHandle.

  END METHOD.
/*                                                                                                                                 */
/*  METHOD PUBLIC HANDLE StartTabWindow (icProcName     AS CHAR                                                                    */
/*                                      ,icTabTitle     AS CHAR                                                                    */
/*                                      ,ibNew          AS LOG                                                                     */
/*                                      ,ibKeepOpen     AS LOG).                                                                   */
/*    DEF VAR ohWinHandle    AS HANDLE NO-UNDO.                                                                                    */
/*                                                                                                                                 */
/*    PUBLISH "StartTabWindow" FROM PROCEDURE-HANDLE(icProcName,icTabTitle,PROCEDURE-HANDLE,ibNew,ibKeepOpen,OUTPUT ohWinHandle).  */
/*    RETURN ohWinHandle.                                                                                                          */
/*                                                                                                                                 */
/*  END METHOD.                                                                                                                    */
  
/*  METHOD PUBLIC CHARACTER getFieldList(icBuffersAndFields AS CHAR                                                 */
/*                                      ,icQueryCrit        AS CHAR):                                               */
/*    RETURN DYNAMIC-FUNCTION("getFieldList",icBuffersAndFields,"WHERE " + LEFT-TRIM(icQueryCrit,"WHERE ")).        */
/*  END METHOD.                                                                                                     */
/*  METHOD PUBLIC CHARACTER getFieldList(icBuffersAndFields AS CHAR                                                 */
/*                                      ,icQueryCrit        AS CHAR                                                 */
/*                                      ,icDelimiter        AS CHAR):                                               */
/*    RETURN REPLACE(THIS-OBJECT:getFieldList(icBuffersAndFields,icQueryCrit),"|",icDelimiter).                     */
/*  END METHOD.                                                                                                     */
/*                                                                                                                  */
/*  METHOD PUBLIC CHARACTER getFieldValues(icBuffer      AS CHAR                                                    */
/*                                        ,icQueryCrit   AS CHAR                                                    */
/*                                        ,icFieldList   AS CHAR):                                                  */
/*    DEF VAR cReturn   AS CHAR NO-UNDO.                                                                            */
/*    DEF VAR cDbBuffer AS CHAR NO-UNDO.                                                                            */
/*    DEF VAR ix        AS INT  NO-UNDO.                                                                            */
/*    IF icBuffer MATCHES "buf*_*" THEN                                                                             */
/*      cDbBuffer = SUBSTR(icBuffer,6).                                                                             */
/*    ELSE                                                                                                          */
/*      cDbBuffer = icBuffer.                                                                                       */
/*    cReturn = DYNAMIC-FUNCTION("getFieldValues",cDbBuffer,"WHERE " + LEFT-TRIM(icQueryCrit,"WHERE "),icFieldList).*/
/*    FOR EACH ttFieldValue                                                                                         */
/*        WHERE ttFieldValue.cBuffer = icBuffer:                                                                    */
/*        DELETE ttFieldValue.                                                                                      */
/*    END.                                                                                                          */
/*    DO ix = 1 TO NUM-ENTRIES(icFieldList):                                                                        */
/*      CREATE ttFieldValue.                                                                                        */
/*      ASSIGN ttFieldValue.cBuffer = icBuffer                                                                      */
/*             ttFieldValue.cField  = ENTRY(ix,cField)                                                              */
/*             ttFieldValue.cValue  = ENTRY(ix,cReturn,"|").                                                        */
/*    END.                                                                                                          */
/*    RETURN cReturn.                                                                                               */
/*  END METHOD.                                                                                                     */
/*                                                                                                                  */
/*  METHOD PUBLIC CHARACTER fieldValue (icField AS CHAR):                                                           */
/*    DEF VAR cBuffer   AS CHAR NO-UNDO.                                                                            */
/*    IF NUM-ENTRIES(icField,".") > 1 THEN DO:                                                                      */
/*      ASSIGN cBuffer = SUBSTR(icField,1,R-INDEX(icField,".") - 1)                                                 */
/*             icField = ENTRY(NUM-ENTRIES(icField,"."),".").                                                       */
/*                                                                                                                  */
/*      FIND FIRST ttFieldValue                                                                                     */
/*           WHERE ttFieldValue.cBuffer = cBuffer                                                                   */
/*             AND ttFieldValue.cField = icField                                                                    */
/*           NO-ERROR.                                                                                              */
/*    END.                                                                                                          */
/*    ELSE                                                                                                          */
/*      FIND FIRST ttFieldValue                                                                                     */
/*           WHERE ttFieldValue.cField = icField                                                                    */
/*           NO-ERROR.                                                                                              */
/*                                                                                                                  */
/*    IF AVAIL ttFieldValue THEN RETURN ttFieldValue.cValue.                                                        */
/*    ELSE RETURN ?.                                                                                                */
/*  END METHOD.                                                                                                     */
/*                                                                                                                  */
/*  METHOD PUBLIC CHARACTER fieldType (icField AS CHAR):                                                            */
/*    RETURN DYNAMIC-FUNCTION("getFieldDataType",icField).                                                          */
/*  END METHOD.                                                                                                     */

  METHOD PUBLIC VOID setNoResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoResizeX(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT ihFrame AS HANDLE
                                 ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT ihFrame AS HANDLE
                                  ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),FRAME-HANDLE,icWidgetNameList).
  END METHOD.
  
  METHOD PUBLIC VOID setNoMoveX(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT ihFrame AS HANDLE
                               ,INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM PROCEDURE-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),ihFrame,icWidgetNameList).
  END METHOD.
  
  DESTRUCTOR PUBLIC JBoxProcedure ( ):
    FOR EACH ttChildWin:
      IF VALID-HANDLE(ttChildWin.hChildWin) THEN
        APPLY "close" TO ttChildWin.hChildWin.
    END.  
  END DESTRUCTOR.
                             
END CLASS.
  