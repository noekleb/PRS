/* To recompile add the src catalog for the AppBuilder source, f.ex
   T:\Progress\OE_100b_fcs_src\src
   to the PROPATH after your adeuib override catalog.
   (Your adeuib must be created under a directory in the PROPATH
   but before the Progress/OpenEdge installation dir.)
   
   REMOVE THE src CATALOG FROM YOUR PROPATH AFTER FINISHING THE COMPILATION  
   
   The source code for various patces of Progress/OpenEdge can be found here:
   http://communities.progress.com/pcom/search.jspa?q=OpenEdge+source+code&resultTypes=BLOG_POST&resultTypes=DOCUMENT&resultTypes=MESSAGE&resultTypes=BLOG&resultTypes=COMMUNITY&resultTypes=PROJECT&resultTypes=SOCIAL_GROUP&resultTypes=COMMENT&peopleEnabled=true&dateRange=all&communityID=&username=
   
   There are changes in definitions of temp-tables etc between releases (and even patches)
   so f.ex the rcode for 10.0b will not work on 10.1a
   
   The super procedure uses a dynamic query on the _fields table so version 9.d is needed (hasn't been tested)
   
   Run this program in an editor (Client) startup with these startup parameters included:
   -zn -inp 32000
   
   Changes to the _* files require a restart of AppBuilder to take effect
   
   Created: 04.04.08 by brynjar@chemistry.no
   
   Modified: 20.11.13 by brynjar@chemistry.no
            - two more files have been customized to enable use of query/browse designer
*/

COMPILE adeuib\_drwflds.p SAVE.
COMPILE adeuib\_drawobj.p SAVE.
COMPILE adeuib\_rdfill.p SAVE.
COMPILE adeuib\abTmpDrwFldsSuper.p SAVE.
COMPILE adeuib\abFieldSelectOptions.w SAVE.
/* New 20.11.13: */
COMPILE adeuib\_ffqdlg.p SAVE.
COMPILE adeshar\_bstname.p SAVE.


/* Modifications to the AppBuilder code for select fields options:

_rdfill.p:

DEF VAR cDbFieldUsage AS CHAR NO-UNDO.
cDbFieldUsage = DYNAMIC-FUNCTION("getDbFieldUsage") NO-ERROR.
IF NOT ERROR-STATUS:ERROR AND cDbFieldUsage NE "abdefault" AND cDbFieldUsage NE "" AND cDbFieldUsage NE ? THEN 
  _U._TOOLTIP = DYNAMIC-FUNCTION("getDbFieldToVarTooltip",_U._NAME) NO-ERROR.


_drwflds.p

DEF VAR cDbFieldUsage      AS CHAR   NO-UNDO.
DEF VAR cFillInHelp        AS CHAR   NO-UNDO.
..
  IF NOT useDataObject THEN DO:
    RUN adeuib\abFieldSelectOptions.w (_fld_names,OUTPUT cDbFieldUsage).
    IF cDbFieldUsage = "" THEN RETURN.  /* If fields are just copied to the clipboard */
    DYNAMIC-FUNCTION("setDbFieldUsage",cDbFieldUsage).
  END.
..
  IF cDbFieldUsage NE "" AND cDbFieldUsage NE "abdefault" THEN 
    PUT STREAM temp_file UNFORMATTED DYNAMIC-FUNCTION("creFillInDefFromDb",_fld_names).
..
    IF cDbFieldUsage NE "" AND cDbFieldUsage NE "abdefault" THEN 
      ASSIGN fld_name    = DYNAMIC-FUNCTION("getDbFieldCorrVarName",ENTRY(NUM-ENTRIES(fld_name,"."),fld_name,"."))
             cFillInHelp = DYNAMIC-FUNCTION("getDbFieldToVarHelp",fld_name).
..             
    IF cFillInHelp NE "" THEN
      PUT STREAM temp_file UNFORMATTED ' HELP "' cFillInHelp '"'.

 _drawobj.p:
 
  DEF VAR hAbTmpDrwFldsSuper AS HANDLE NO-UNDO.
..
            RUN adeuib\AbTmpDrwFldsSuper.p PERSIST SET hAbTmpDrwFldsSuper.
            SESSION:ADD-SUPER-PROC(hAbTmpDrwFldsSuper).
..
            DELETE PROCEDURE hAbTmpDrwFldsSuper.
*/  
