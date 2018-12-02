/* To recompile add the src catalog for the AppBuilder source, f.ex
   T:\Progress\OE_100b_fcs_src\src
   to the PROPATH after your adeuib override catalog.
   (Your adeuib must be created under a directory in the PROPATH
   but before the Progress/OpenEdge installation dir.)
   
   REMOVE THE src CATALOG FROM YOUR PROPATH AFTER FINISHING THE COMPILATION  
   
   The source code for various patces of Progress/OpenEdge can be found by searching
   the Progress communtity for "ADE - OpenEdge 11.3.1 Development Tools Source Code" f.ex
   
   There are changes in definitions of temp-tables etc between releases (and even patches)
   so f.ex the rcode for 10.0b will not work on 10.1a
      
   Run this program in an editor (Client) startup with these startup parameters included:
   -zn -inp 32000
   
   Changes to the _* files require a restart of AppBuilder to take effect
   
   Created: 04.04.08 by brynjar@chemistry.no
   
   Modified: 20.11.13 by brynjar@chemistry.no
            - two more files have been customized to enable use of query/browse designer
*/

/* Increase size of column editor for browse: */
COMPILE adeuib\_coledit.p SAVE.
COMPILE adeuib\ide\_dialog_coledit.p SAVE INTO c:\progress\jukebox\tools\11.3\adeuib\ide.
COMPILE adeuib\_rsz_wp.p SAVE.
COMPILE adeuib\_cr_pal.p SAVE.

COMPILE adecomm\oeideservice.p SAVE.

COMPILE adeuib\_uibmain.p SAVE.
COMPILE adeuib\_cr_pal.p SAVE.
COMPILE adeuib\_drawobj.p SAVE.

COMPILE adeuib\_prpobj.p SAVE.
COMPILE adeuib\ide\_dialog_prpobj.p SAVE.


COMPILE JukeBox\abTmpDrawObjSuper.p SAVE.

/*

COMPILE adeuib\_drwflds.p SAVE.


COMPILE adeuib\_callqry.p SAVE.
COMPILE adeuib\_ffqdlg.p SAVE.

COMPILE adeshar\_query.p SAVE INTO c:\progress\jukebox\tools\11.3.
COMPILE adeuib\ide\_dialog_query.p SAVE.
*/

/*
COMPILE adeshar\_query.p SAVE INTO c:\progress\jukebox\tools\11.3.


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
