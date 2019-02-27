
/*------------------------------------------------------------------------
    File        : InitFlatViewDataBrwObj.p
    Purpose     : Avoid class ref in UI-lib (FlatViewRecord) 
                  so that libraries can work without classes
    Syntax      :

    Description : 

    Author(s)   : Brynjar
    Created     : Fri Nov 18 15:49:42 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView          AS HANDLE NO-UNDO.
DEF INPUT PARAM icBuffersAndFields  AS CHAR   NO-UNDO.
DEF INPUT PARAM icQuery             AS CHAR   NO-UNDO.
DEF INPUT PARAM icSort              AS CHAR   NO-UNDO.
DEF INPUT PARAM ibCount             AS LOG    NO-UNDO.

DEF VAR oDynDataBrowseObject  AS JBoxDynDataBrowse NO-UNDO.

oDynDataBrowseObject = DYNAMIC-FUNCTION("getDynDataBrowseObject" IN ihFlatView). 

oDynDataBrowseObject:InitializeQuery(icBuffersAndFields,icQuery,icSort,
                                     ibCount).
