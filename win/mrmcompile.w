&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME br-frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS br-frame 
/*
-------------------------------------------------------------------
Copyright (c) 1998 Walvis Software B.V.
-------------------------------------------------------------------
Name     : mrmcompile.w
Purpose  : Most Recent Modified Compiler
Author   : Peter van Dam
-------------------------------------------------------------------
Version   Date   Author Description
-------------------------------------------------------------------
 1.01   08/12/98  PvD   First Version 
-------------------------------------------------------------------
Description:

  This utility searches your PROPATH for Progress objects (.r files)
  and tries to find the corresponding sources.
  When the source is located the new FILE-MOD-DATE and FILE-MOD-TIME
  attributes of FILE-INFO are used to compare the time stamps.
  With the Compile button you can compile any sources that are
  found to be newer than the object.

*/

/*------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable atota# as integer no-undo.
define variable lstop# as logical no-undo.
define variable aobje# as integer no-undo.
define variable acomp# as integer no-undo.
define variable wspla# as handle no-undo. /* splash screen */
define variable wwind# as handle no-undo. /* window of splash screen */

define temp-table t-proc no-undo
       field object-name as char format "x(60)"
       field object-dir as char 
       field object-mod-date as date
       field object-mod-time as int
       field source-name as char format "x(60)"
       field source-mod-date as date
       field source-mod-time as int
       index source-name is unique primary source-name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE wppvoort
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME br-frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 rect-buit RECT-7 rect-buit2 ncomp# ~
butt-ok butt-help butt-canc wperc# 
&Scoped-Define DISPLAYED-OBJECTS wperc# nstat# 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON butt-canc AUTO-END-KEY DEFAULT 
     LABEL "&Cancel":L 
     SIZE 15 BY 1.

DEFINE BUTTON butt-comp 
     LABEL "Compile" 
     SIZE 15 BY 1.

DEFINE BUTTON butt-help 
     LABEL "&Help" 
     SIZE 15 BY 1.

DEFINE BUTTON butt-ok AUTO-GO DEFAULT 
     LABEL "&Start" 
     SIZE 15 BY 1.

DEFINE VARIABLE ncomp# AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 132 BY 7.38
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE nstat# AS CHARACTER FORMAT "x(256)":U 
      VIEW-AS TEXT 
     SIZE 130.6 BY .81
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE wperc# AS CHARACTER FORMAT "X(256)":U INITIAL "0 %" 
      VIEW-AS TEXT 
     SIZE 5 BY .67
     BGCOLOR 9 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 136 BY 11.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 132 BY 1.

DEFINE RECTANGLE rect-buit
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 132 BY 1.29
     FGCOLOR 9 .

DEFINE RECTANGLE rect-buit2
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 132 BY 1.29
     BGCOLOR 9 FGCOLOR 9 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME br-frame
     ncomp# AT ROW 5.05 COL 5 NO-LABEL
     butt-ok AT ROW 13.14 COL 3
     butt-comp AT ROW 13.14 COL 21
     butt-help AT ROW 13.14 COL 106
     butt-canc AT ROW 13.14 COL 124
     wperc# AT ROW 2.19 COL 67 COLON-ALIGNED NO-LABEL
     nstat# AT ROW 3.71 COL 3.4 COLON-ALIGNED NO-LABEL
     RECT-6 AT ROW 1.52 COL 3
     rect-buit AT ROW 1.95 COL 5
     RECT-7 AT ROW 3.62 COL 5
     rect-buit2 AT ROW 1.95 COL 5
     SPACE(3.99) SKIP(11.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "WPP MRM Compiler":L
         DEFAULT-BUTTON butt-ok CANCEL-BUTTON butt-canc.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: wppvoort
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX br-frame
                                                                        */
ASSIGN 
       FRAME br-frame:SCROLLABLE       = FALSE
       FRAME br-frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON butt-comp IN FRAME br-frame
   NO-ENABLE                                                            */
ASSIGN 
       butt-help:PRIVATE-DATA IN FRAME br-frame     = 
                "A".

/* SETTINGS FOR EDITOR ncomp# IN FRAME br-frame
   NO-DISPLAY                                                           */
ASSIGN 
       ncomp#:READ-ONLY IN FRAME br-frame        = TRUE.

/* SETTINGS FOR FILL-IN nstat# IN FRAME br-frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX br-frame
/* Query rebuild information for DIALOG-BOX br-frame
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX br-frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME butt-canc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-canc br-frame
ON CHOOSE OF butt-canc IN FRAME br-frame /* Cancel */
DO:
  assign lstop# = true
         ncomp#:screen-value = ncomp#:screen-value + "~n~nCanceled.".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butt-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-comp br-frame
ON CHOOSE OF butt-comp IN FRAME br-frame /* Compile */
DO:
  
  def var ndire# as char no-undo.
  def var aerro# as int no-undo.
  def var atota# as integer no-undo.
  def var aproc# as integer no-undo.
  def var afout# as integer no-undo.
  
  do with frame {&FRAME-NAME}:
  
    assign ncomp#:screen-value = ncomp#:screen-value + "~n~nCompiling...~n"
           ncomp#:tooltip = "".
             
    for each t-proc:
      assign atota# = atota# + 1.
    end.
             
    for each t-proc:
      
      assign ncomp#:screen-value = ncomp#:screen-value 
                                 + "~n" + t-proc.source-name + "~n" 
                                 + "  INTO " + t-proc.object-dir + ":"
             aproc# = aproc# + 1.

      run dispbar in this-procedure (aproc#, atota#).
                                 
      compile value (t-proc.source-name) save into value (t-proc.object-dir)
              no-error.

      if compiler:error or compiler:warning then do:
  
        assign afout# = afout# + 1.
        
        /* error or warning */
            
        do aerro# = 1 to error-status:num-messages:
          assign ncomp#:screen-value = ncomp#:screen-value + "~n" 
                                     + error-status:get-message(aerro#).                                    
        end. /* do aerro# ... */

      end. /* of compiler errors and warnings */
      else
        assign ncomp#:screen-value = ncomp#:screen-value + " OK.".
                      
      process events.   
      
      if lstop# = true then leave.
    
    end. /* of for each t-proc */

    if lstop# = false then
      assign ncomp#:screen-value = ncomp#:screen-value 
                                 + "~n~nCompilation complete. "
                                 + substitute("(&1 sources, &2 error(s))",
                                              aproc#,afout#).
    
    assign self:sensitive = false.
    ncomp#:move-to-eof().
    
  end. /* of do with frame */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butt-help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-help br-frame
ON CHOOSE OF butt-help IN FRAME br-frame /* Help */
DO:
  system-help search("wpp9.hlp") context 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-ok br-frame
ON CHOOSE OF butt-ok IN FRAME br-frame /* Start */
DO:              
  
  do on error undo, return no-apply:
    
    run check-files in this-procedure.
    
    if acomp# > 0 then do with frame {&FRAME-NAME}: 
      enable butt-comp.
      disable butt-ok.
      apply "entry" to butt-comp.
      assign butt-canc:label = "Close".
      return no-apply.
    end.
    else
      message "No recompilation needed." view-as alert-box information.
  end.
    
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK br-frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN do:  
  
  if default-window:visible = false then 
    assign default-window:x = 0
           default-window:y = 0
           default-window:width-pixels = session:width-pixels
           default-window:height-pixels = session:height-pixels
           frame {&FRAME-NAME}:parent = default-window
           default-window:hidden = true.
  else         
    assign FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
end.

assign frame {&FRAME-NAME}:title = "WALVIS Software Most Recent Modified Compiler"
       /*
       frame {&FRAME-NAME}:x = (session:width-pixels - frame {&FRAME-NAME}:width-pixels) / 2
       frame {&FRAME-NAME}:y = (session:height-pixels - frame {&FRAME-NAME}:height-pixels) / 2
       */
       ncomp# = "This utility searches your PROPATH for Progress objects (.r files)
 and tries to find the corresponding sources.
 When the source is located the new FILE-MOD-DATE and FILE-MOD-TIME
 attributes of FILE-INFO are used to compare the time stamps.
 With the Compile button you can compile any sources that are
 found to be newer than the object.
".


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  run wppsplash.w persistent set wspla#.
 
  session:set-wait-state("general").
  pause 1 no-message.
  run LockWindowUpdate in wspla# (input 0).
  process events.
  run LockWindowUpdate in wspla# (input {&WINDOW-NAME}:hwnd).  
  
  RUN enable_UI.

    
  pause 2 no-message. /* wait-for animation */

  display ncomp# with frame {&FRAME-NAME}.
  
  pause 2 no-message.
  
  run LockWindowUpdate in wspla# (input 0).

  run disable_ui in wspla# no-error. /* close splash screen */
  process events.

  session:set-wait-state("").

  /*
  message session:width-pixels frame {&FRAME-NAME}:width-pixels frame {&FRAME-NAME}:x.
  */
  
  /*
  assign
       frame {&FRAME-NAME}:x = (session:width-pixels - frame {&FRAME-NAME}:width-pixels) / 2
       frame {&FRAME-NAME}:y = (session:height-pixels - frame {&FRAME-NAME}:height-pixels) / 2.
  */

  /*
  apply "entry" to {&WINDOW-NAME}.
  */
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-files br-frame 
PROCEDURE check-files :
/*------------------------------------------------------------------------------
  Main routine.        
------------------------------------------------------------------------------*/
  def var atell# as int no-undo.
  
  do with frame {&FRAME-NAME}:
  
    assign atota# = num-entries(propath)
           self:sensitive = false
           ncomp#:screen-value = ""
           ncomp#:tooltip = "Based on their time stamps these files need recompiling. "
                          + "Press the Compile button to start the compilation.".
    
    do atell# = 1 to atota#:
      assign nstat#:screen-value = " " + entry(atell#,propath).
      run dispbar (atell#, atota#).
      if lstop# = true then 
        leave.
      run find-objects in this-procedure (input entry(atell#,propath)).  
    end.

    assign nstat#:screen-value = substitute(" &1 objects checked; &2 compilation.",
                                            aobje#,
                                            (if acomp# = 0 then "none need"
                                             else if acomp# = 1 then "1 needs"
                                             else substitute("&1 need",acomp#))).
              
  end. /* of do with frame */              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI br-frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME br-frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispbar br-frame 
PROCEDURE dispbar :
/*------------------------------------------------------------------------------
  Purpose   :  display Progress bar   
  Parameters:  atell# and atota#, where atell# / atota# is the percentage shown.
------------------------------------------------------------------------------*/

  define input parameter atell# as integer no-undo.
  define input parameter atota# as integer no-undo.
  
  def var aperc# as integer.
  
  process events.
  
  do with frame {&FRAME-NAME}:   

    if atota# = 0 then return.

    assign aperc# = min(max(int((atell# / atota#) * 100),0),100).
    if wperc#:screen-value = string(aperc#) + " %" then return.
  
    if (aperc# / 100) * rect-buit:width-pixels
      >= wperc#:x - rect-buit2:x
      + wperc#:width-pixels then
      assign wperc#:bgcolor = rect-buit:fgcolor
             wperc#:fgcolor = 15. /* wit */
    else
      assign wperc#:bgcolor = frame {&FRAME-NAME}:bgcolor
             wperc#:fgcolor = rect-buit:fgcolor.
      
    assign rect-buit2:width-pixels = max((aperc# / 100) * rect-buit:width-pixels,1)
           wperc#:screen-value    = (if aperc# ne 100 then
                                       string(aperc#) + " %"
                                     else
                                       string(aperc#) + "%").
  
  end. /* of do with frame */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI br-frame _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY wperc# nstat# 
      WITH FRAME br-frame.
  ENABLE RECT-6 rect-buit RECT-7 rect-buit2 ncomp# butt-ok butt-help butt-canc 
         wperc# 
      WITH FRAME br-frame.
  VIEW FRAME br-frame.
  {&OPEN-BROWSERS-IN-QUERY-br-frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-objects br-frame 
PROCEDURE find-objects :
/*------------------------------------------------------------------------------
  Check the object files in a directory       
------------------------------------------------------------------------------*/

  define input parameter ndire# as char no-undo.

  define variable nobje# as char format "x(20)" no-undo.
  define variable nobjefull# as char format "x(60)" no-undo.
  define variable nattr# as char no-undo.
  define variable nsour# like nobjefull# no-undo.
  define variable ndirefull# as char no-undo.
    
  assign file-info:file-name = ndire#
         ndirefull# = file-info:full-pathname.

  process events.
  if lstop# = true then stop.
  
  if (file-info:file-type matches "*D*") ne true then return.
  
  input from os-dir(ndire#).
  find-object:
  repeat with frame {&FRAME-NAME}:
    import nobje# nobjefull# nattr#.
      
    if nattr# matches "*F*" and nobje# matches "*~~.r" then do:

      assign nstat#:screen-value = " Checking " + nobjefull# + "..."
             aobje# = aobje# + 1
             file-info:file-name = nobjefull#.
  
      if (file-info:file-type matches "*W*") = false then 
        next find-object.
        
      assign nsour# = entry(1,nobje#,".") + ".w".
      if search(nsour#) = ? then do:
        assign nsour# = entry(1,nobje#,".") + ".p".
        if search(nsour#) = ? then /* no source found */
          next find-object.
      end.
      
      /* nsour# now contains the relative name of the source file */
      
      assign nsour# = search(nsour#).
        
      /* Since we can only check one file at a time we need a place
         to store the attributes of the object file.
         We use temp-table t-proc for this purpose */
         
      create t-proc.
      
      assign t-proc.object-name = nobjefull#
             t-proc.object-dir = ndirefull#
             t-proc.object-mod-date = file-info:file-mod-date
             t-proc.object-mod-time = file-info:file-mod-time
             file-info:file-name = nsour#
             t-proc.source-name = file-info:full-pathname.
             
      if file-info:file-mod-date < t-proc.object-mod-date
      or (file-info:file-mod-date = t-proc.object-mod-date
          and
          file-info:file-mod-time < t-proc.object-mod-time) then
        delete t-proc.
      else       
        assign t-proc.source-mod-date = file-info:file-mod-date
               t-proc.source-mod-time = file-info:file-mod-time
               acomp#                 = acomp# + 1
               ncomp#:screen-value    = trim(ncomp#:screen-value + "~n"
                                             + t-proc.object-name
                                             + " " + string(t-proc.object-mod-date,"99-99-9999")
                                                   + " "
                                                   + string(t-proc.object-mod-time,"HH:MM:SS")
                                             + "; src " + string(t-proc.source-mod-date,"99-99-9999")
                                                        + " "
                                                        + string(t-proc.source-mod-time,"HH:MM:SS")).

      
    end.
    
  end.
  input close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

