
/*------------------------------------------------------------------------
    File        : jbsetup_custom_menu_access.p
    Purpose     : Determine access to menu node based on an application specific criteria		

    Syntax      :

    Description : Custom menu access	

    Author(s)   : Brynjar
    Created     : Wed Mar 12 16:20:51 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO. 
DEF INPUT  PARAM icUserId        AS CHAR NO-UNDO.
DEF INPUT  PARAM iiMenuId        AS INT  NO-UNDO.
DEF INPUT  PARAM iiCompanyOrRole AS INT  NO-UNDO.
DEF INPUT  PARAM icParam         AS CHAR NO-UNDO.
DEF OUTPUT PARAM obAccess        AS LOG  NO-UNDO INIT YES.
