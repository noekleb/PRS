&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

INIT:
DO:
    DEF VAR bOk   AS LOG NO-UNDO.

    bOK = SESSION:SET-WAIT-STATE("general").

/*     {incl/ttDataDict.i NEW} */

    RUN JBoxLoadLib.p ("JBoxUILib.p,JBoxASlib.p,ResizeLib.p").

    IF PROVERSION BEGINS "1" THEN
      RUN JBoxLoadLib.p ("JBoxFUlib.p"). 

    FIND FIRST bruker NO-LOCK
         WHERE bruker.BrukerID = USERID("dictdb")
         NO-ERROR.

    DYNAMIC-FUNCTION("setAppserviceId","skotex").
    DYNAMIC-FUNCTION("setSessionId","validsession").
    DYNAMIC-FUNCTION("setASUserId",userid("dictdb"),IF AVAIL bruker THEN bruker.Navn ELSE " ").
    DYNAMIC-FUNCTION("setCompanyId","0").
    DYNAMIC-FUNCTION("setAppTitle","PRS").
    DYNAMIC-FUNCTION("setBehaviour",
                      "DefaultSortFont|6," +   
                      "DefaultSortColor|15," + 
                      "BrowseSearchDefault|goto," +
                      "TabOnReturn|yes," +       
                      "SetSortLabel|yes," +
                      "CtrlHotkeyActions|," +
                      "DefaultImageList|" +
                        "bmp/accep16e.bmp;icon/add.bmp;icon/e-detail.bmp" 
                      + ";bmp/e-exit.bmp;bmp/e-help.bmp;icon/copyrec.bmp" 
                      + ";icon/reset.bmp;icon/deleterec.bmp;gif/saverec.gif" 
                      + ";gif/afexcel.gif;gif/afword.gif;bmp/print16e.bmp" 
                      + ";gif/filter.gif;gif/afinternet.gif;gif/msngrWindow.gif" 
                      + ";icon/next.bmp;icon/prev.bmp;icon/first.bmp;icon/last.bmp"
                      + ";bmp/commit.bmp;bmp/rollback.bmp;gif/active.gif;gif/sdogen16.gif"
                      ).      

    DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").
    DYNAMIC-FUNCTION("setAttribute",SESSION,"SEKASSE",OS-GETENV("SEKASSE")).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"SEBUTIKK",OS-GETENV("SEBUTIKK")).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"filterwindow","JBoxDynamicsFilter.w").

    DYNAMIC-FUNCTION("setBaseLanguageCode","NO").
    DYNAMIC-FUNCTION("setLanguages","NO|SE").

    IF AVAIL bruker THEN DO:
      IF CAN-DO("des,no",bruker.Lng) THEN
        DYNAMIC-FUNCTION("setLanguageCode","NO").
      ELSE
        DYNAMIC-FUNCTION("setLanguageCode",bruker.Lng).
    END.
    ELSE DYNAMIC-FUNCTION("setLanguageCode","NO").
  
    bOK = SESSION:SET-WAIT-STATE("").

END. /* JukeBox */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


