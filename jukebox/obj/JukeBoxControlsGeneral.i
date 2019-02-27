/*=================================================================
	File Name:	         JukeBoxControlsGeneral.i
	Dll:                 Controls.dll
	Dll File Version:    2.0.0 Build 1
	Dll Product Version: 2.0.0
	
	Author:		James Wiliams
	Created:	July 28, 1999
	Modified:   August 9, 1999 [jlw]
	              Added new wrapper functions for Insert of Items.
	            October 7,1999 [jlw]
                      Added new wrapper functions and modified Tab
                      wrappers for new functionality.
                October 15, 1999 [ekr]
                      Added new wrapper function for the panel 
                      control.
                October 20,1999 [jlw]
                	  Added additional function wrappers for new
                	  Tab control functionality.
                November 05,1999 [jlw] 
                	  Changed this comment section to reflect 
                	  current file version of dll.  
                	  Added wrapper procedure for setting indentation
                	  of child nodes.
               July 6,2000 [jlw]
                    Added a new procedure for treeview drag and drop
                    operations.  Also added another event code for 
                    the trigger.
               Sept 21,2000 [jlw]
               		Added more support procedures for listview control.
               		Also added extra constants for control.
               Sept 30,2007 [jlw]
                    Added new support procedures for NavBar control.      	  
	Description:
		This include file contains the basic procedure wrappers 
		for the controls.dll.  This dll provides standard tab
		and treeview controls to the progress environment.
	Note:
		This include file may not contain every available interface
		procedure into the currently named dll.  This is because 
		some functions are still under developement even though they 
		are exported in the dll version.  If you use quickview to 
		see the contains of the dll and discover that there are
		additional functions not mentioned in this file and try to
		use these functions, then the stability of the application
		is not garrentied by CreditView.
===================================================================*/
&GLOBAL-DEFINE DLL_FILE_SPEC "1.0.1.327"
/*&MESSAGE Make sure you are useing dll file version {&DLL_FILE_SPEC}*/

  
&IF DEFINED(WND_PROCEDURES-PLEASE) = 0 &THEN
&SCOPED-DEFINE WND_PROCEDURES-PLEASE YES
&ENDIF

&IF DEFINED(WND_PROCEDURES-PLEASE) <> 0 AND {&WND_PROCEDURES-PLEASE} = YES &THEN


  /*----> Some standard APIs for loading and unloading dll <----*/
  
  PROCEDURE LoadLibraryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT  PARAMETER LLA_cName   AS CHARACTER.
    DEFINE RETURN PARAMETER LLA_hHandle AS LONG.
  END PROCEDURE.

  PROCEDURE FreeLibrary EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT PARAMETER FL_hHanlde AS LONG.
  END PROCEDURE.
  PROCEDURE SetFocus EXTERNAL "User32.dll":U:
    DEFINE INPUT PARAMETER hWnd AS LONG.
  END PROCEDURE.

  PROCEDURE LockWindowUpdate EXTERNAL "User32.dll":U:
    DEFINE INPUT PARAMETER hWnd AS LONG.
  END PROCEDURE.

&ENDIF
  
  FUNCTION GlobGetImagePath RETURNS CHARACTER
    (INPUT ggip_cImageName AS CHARACTER):
    
    DEF VAR cImagesPath AS CHARACTER NO-UNDO.
    
      cImagesPath = SEARCH(ggip_cImageName).
      IF cImagesPath = ? THEN
        RETURN "":U.
      cImagesPath = SUBSTRING(cImagesPath,1,
                          LENGTH(cImagesPath) - 
                          LENGTH(ggip_cImageName) - 1). 
      RETURN cImagesPath.
    
  END FUNCTION.
  
  /* ---> Global Preprocessors <---*/

&GLOBAL-DEFINE GEN_DISABLE   0
&GLOBAL-DEFINE GEN_ENABLE    1
&GLOBAL-DEFINE GEN_NOSELECT -1
&GLOBAL-DEFINE GEN_NOIMAGE  -1
&GLOBAL-DEFINE GEN_VISIBLE   1
&GLOBAL-DEFINE GEN_HIDDEN    0

/*======================================================================
  Data type global defines.
========================================================================*/
&GLOBAL-DEFINE GEN_STRING-TYPE   0
&GLOBAL-DEFINE GEN_INTEGER-TYPE  1
&GLOBAL-DEFINE GEN_DATETIME-TYPE 2
&GLOBAL-DEFINE GEN_DECIMAL-TYPE  3
&GLOBAL-DEFINE GEN_CURRENCY-TYPE 4
&GLOBAL-DEFINE GEN_LOGICAL-TYPE  5
/*======================================================================
    Font related global defines.
========================================================================*/

&GLOBAL-DEFINE FS-BOLD      1
&GLOBAL-DEFINE FS-ITALIC    2
&GLOBAL-DEFINE FS-UNDERLINE 4
&GLOBAL-DEFINE FS-STRIKEOUT 8
&GLOBAL-DEFINE FN_VAL_FONTNAME 	    1
&GLOBAL-DEFINE FN_VAL_FONTSIZE      2
&GLOBAL-DEFINE FN_VAL_FONTSTYLE     4
&GLOBAL-DEFINE FN_VAL_FONTCOLOR     8
/*======================================================================
  Text alignment global defines.
========================================================================*/
&GLOBAL-DEFINE GEN_LEFT-JUSTIFY     0
&GLOBAL-DEFINE GEN_CENTER-JUSTIFY   1
&GLOBAL-DEFINE GEN_RIGHT-JUSTIFY    2
/*======================================================================
    Predefined colors.
========================================================================*/


&GLOBAL-DEFINE COLOR-AQUA            16776960
&GLOBAL-DEFINE COLOR-BLACK                  0
&GLOBAL-DEFINE COLOR-BLUE            16711680
&GLOBAL-DEFINE COLOR-CREAM           15793151
&GLOBAL-DEFINE COLOR-DARKGRAY         8421504
&GLOBAL-DEFINE COLOR-FUCHSIA         16711935
&GLOBAL-DEFINE COLOR-GRAY             8421504
&GLOBAL-DEFINE COLOR-GREEN              32768
&GLOBAL-DEFINE COLOR-LIMEGREEN          65280
&GLOBAL-DEFINE COLOR-LIGHTGRAY       12632256
&GLOBAL-DEFINE COLOR-MAROON               128
&GLOBAL-DEFINE COLOR-MEDIUMGRAY      10789024
&GLOBAL-DEFINE COLOR-MINTGREEN       12639424
&GLOBAL-DEFINE COLOR-NAVYBLUE         8388608
&GLOBAL-DEFINE COLOR-OLIVE              32896
&GLOBAL-DEFINE COLOR-PURPLE           8388736
&GLOBAL-DEFINE COLOR-RED                  255
&GLOBAL-DEFINE COLOR-SILVER          12632256
&GLOBAL-DEFINE COLOR-SKYBLUE         15780518
&GLOBAL-DEFINE COLOR-TEAL             8421376
&GLOBAL-DEFINE COLOR-WHITE           16777215
&GLOBAL-DEFINE COLOR-YELLOW             65535






