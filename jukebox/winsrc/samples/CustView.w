&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE AdvGuiWin

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
                          
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.

DEF VAR oCommentEdit AS JBoxDevExEdit NO-UNDO.
DEF VAR oCreditLimit AS JBoxDevExCalcEdit NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmCustView

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar CustNum Name SalesRep Address ~
CreditLimit Address2 Discount PostalCode Terms City Balance State btnState ~
StateName Country Contact EmailAddress Phone Fax Comments 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name SalesRep Address CreditLimit ~
Address2 Discount PostalCode Terms City Balance State StateName Country ~
Contact EmailAddress Phone Fax Comments fiCommentLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnState 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Salesrep" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE Comments AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 95.2 BY 3.1 NO-UNDO.

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE Address2 AS CHARACTER FORMAT "x(35)" 
     LABEL "Address2" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE Balance AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Balance" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE City AS CHARACTER FORMAT "x(25)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE Contact AS CHARACTER FORMAT "x(30)" 
     LABEL "Contact" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Country AS CHARACTER FORMAT "x(20)" INITIAL "USA" 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE CreditLimit AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 1500 
     LABEL "Credit Limit" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE Discount AS INTEGER FORMAT ">>9%" INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE EmailAddress AS CHARACTER FORMAT "x(50)" 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE Fax AS CHARACTER FORMAT "x(20)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE fiCommentLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Comment:" 
      VIEW-AS TEXT 
     SIZE 9.8 BY .62 NO-UNDO.

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Phone AS CHARACTER FORMAT "x(20)" 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE PostalCode AS CHARACTER FORMAT "x(10)" 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE State AS CHARACTER FORMAT "x(20)" 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE StateName AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE Terms AS CHARACTER FORMAT "x(20)" INITIAL "Net30" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmCustView
     CustNum AT ROW 2.67 COL 16 COLON-ALIGNED HELP
          "Please enter a customer number."
     Name AT ROW 3.76 COL 16 COLON-ALIGNED HELP
          "Please enter a name."
     SalesRep AT ROW 3.76 COL 69 COLON-ALIGNED
     Address AT ROW 4.81 COL 16 COLON-ALIGNED HELP
          "Please enter an address."
     CreditLimit AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Please enter a Credit Limit."
     Address2 AT ROW 5.86 COL 16 COLON-ALIGNED HELP
          "Please enter an address."
     Discount AT ROW 5.86 COL 69 COLON-ALIGNED HELP
          "Please enter a percentage from 0 to 100."
     PostalCode AT ROW 6.91 COL 16 COLON-ALIGNED HELP
          "Please enter the appropriate Postal Code."
     Terms AT ROW 6.91 COL 69 COLON-ALIGNED HELP
          "Please enter terms"
     City AT ROW 7.95 COL 16 COLON-ALIGNED HELP
          "Please enter a city."
     Balance AT ROW 7.95 COL 69 COLON-ALIGNED HELP
          "Please enter a balance."
     State AT ROW 9 COL 16 COLON-ALIGNED HELP
          "Please enter standard state abbreviation."
     btnState AT ROW 9.05 COL 40.2 NO-TAB-STOP 
     StateName AT ROW 9.05 COL 42.8 COLON-ALIGNED HELP
          "Please enter the full state name." NO-LABEL
     Country AT ROW 10.05 COL 16 COLON-ALIGNED HELP
          "Please enter a country."
     Contact AT ROW 11.1 COL 16 COLON-ALIGNED HELP
          "Please enter a contact."
     EmailAddress AT ROW 12.14 COL 16 COLON-ALIGNED HELP
          "Please enter an full Internet Email Address."
     Phone AT ROW 13.19 COL 16 COLON-ALIGNED HELP
          "Please enter a phone number"
     Fax AT ROW 13.19 COL 46 COLON-ALIGNED HELP
          "Please enter a fax number."
     Comments AT ROW 14.29 COL 17.8 NO-LABEL
     fiCommentLabel AT ROW 14.29 COL 5.6 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.24 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 16.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer"
         HEIGHT             = 16.67
         WIDTH              = 113.4
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmCustView
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frmCustView:RESIZABLE        = TRUE.

/* SETTINGS FOR FILL-IN fiCommentLabel IN FRAME frmCustView
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmCustView
/* Query rebuild information for FRAME frmCustView
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmCustView */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnState
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnState C-Win
ON CHOOSE OF btnState IN FRAME frmCustView /* ... */
DO:
  DEF VAR cStateFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "State"
                     + ";State"
                     + ";StateName"
                     + ";Region"
                     ,
                   "WHERE true"
                    ,""
                    ,"StateName,State",
                    OUTPUT cStateFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cStateFieldList NE "" THEN DO:
    ASSIGN 
       StateName:SCREEN-VALUE   = ENTRY(1,cStateFieldList,"|")
       State:SCREEN-VALUE       = ENTRY(2,cStateFieldList,"|")
       .
    APPLY "any-printable" TO State.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
  PUBLISH "InvalidateHandle".

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hFieldMap}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME frmCustView.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY CustNum Name SalesRep Address CreditLimit Address2 Discount PostalCode 
          Terms City Balance State StateName Country Contact EmailAddress Phone 
          Fax Comments fiCommentLabel 
      WITH FRAME frmCustView.
  ENABLE rectToolbar CustNum Name SalesRep Address CreditLimit Address2 
         Discount PostalCode Terms City Balance State btnState StateName 
         Country Contact EmailAddress Phone Fax Comments 
      WITH FRAME frmCustView.
  {&OPEN-BROWSERS-IN-QUERY-frmCustView}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraPrintRecord C-Win 
PROCEDURE ExtraPrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihPreview AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk      AS LOG    NO-UNDO INIT YES.

DYNAMIC-FUNCTION("LoadPreviewFromText" IN ihPreview,
                 DYNAMIC-FUNCTION("getAppTitle") + "|" + FILL("=",120),"|",YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,
                            "",
                            "Customer"
                          + ",SalesRep;RepName"
                          + ",State;StateName"
                            ,"where false"
                            + ",FIRST SalesRep OF Customer NO-LOCK OUTER-JOIN"
                            + ",FIRST State OF Customer NO-LOCK OUTER-JOIN"
                            ,"").

  ASSIGN SalesRep:DELIMITER = "|"
         SalesRep:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SalesRep;RepName|SalesRep;SalesRep","WHERE TRUE").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "Name,Address,Address2,City,Comments,Contact,Country,CreditLimit,Discount,EmailAddress,Fax,Phone,PostalCode,SalesRep,State,Terms","",
                             "Balance,CustNum,StateName","",
                             "btnState"). 

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc",'customer_create.p').

  &IF DEFINED(AdvGuiWin) &THEN
    oCommentEdit = NEW JBoxDevExEdit(THIS-PROCEDURE,Comments:HANDLE).
    oCommentEdit:RegisterWithJukeBox(YES).
    oCommentEdit:CreateDisplayLink(hFieldMap,"Comments").
    oCommentEdit:cDocLoadContext = "Customer.Comments".
    oCommentEdit:cDocLoadIdFields = "CustNum".
/*    oCommentEdit:cDocLoadFormat = "html".*/
 
    oCreditLimit = NEW JBoxDevExCalcEdit(THIS-PROCEDURE,CreditLimit:HANDLE).
    oCreditLimit:RegisterWithJukeBox(NO).
    oCreditLimit:CreateDisplayLink(hFieldMap,"CreditLimit").
/*    oCreditLimit:calcEdit1:Scale(0,100000).*/
/*    oCreditLimit:calcEdit1:Properties:Mask:EditMask = "n0".*/
    
  &ENDIF

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectToolbar:HANDLE,
                            "",
                            "New,Edit,Undo,Save,Delete,print,rule,first,prev,next,last",
                            "maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,Comments:HANDLE,
                    STRING(SalesRep:HANDLE) + ","
                  + STRING(CreditLimit:HANDLE) + ","
                  + STRING(Discount:HANDLE) + ","
                  + STRING(Terms:HANDLE) + ","
                  + STRING(Balance:HANDLE) 
                   ,"").

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*DEF VAR oEdit AS DevExpress.XtraRichEdit.RichEditControl NO-UNDO.*/
/*RUN SUPER.*/

/*oEdit = oCommentEdit:getDotNetWidget().          */
/*                                                 */
/*MESSAGE VALID-OBJECT(oEdit) skip oEdit:ToString()*/
/*VIEW-AS ALERT-BOX.                               */
oCommentEdit:Print().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF NOT ERROR-STATUS:ERROR THEN
  RUN SaveOk IN hParent NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentQuery = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

