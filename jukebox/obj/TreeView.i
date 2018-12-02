&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  /*====================================================*/
  /*              WINDOW STYLES                         */
  /*====================================================*/
    
    /* Window Extended Styles */

&GLOBAL-DEFINE WS_EX_CLIENTEDGE      512  /* 0x00000200L */
&GLOBAL-DEFINE WS_EX_TOPMOST           8  /* 0x00000008L */
&GLOBAL-DEFINE WS_EX_TRANSPARENT      32  /* 0x00000020L */


         /* Window Class names. */
&GLOBAL-DEFINE  WC_TREEVIEWA           "SysTreeView32":U
&GLOBAL-DEFINE  WC_TABCONTROL          "SysTabControl32":U

         /* Standard Window Styles */

&GLOBAL-DEFINE WS_BORDER            8388608  /* 0x00800000L */
&GLOBAL-DEFINE WS_CHILD          1073741824  /* 0x40000000L */
&GLOBAL-DEFINE WS_THICKFRAME         262144  /* 0x00040000L */
&GLOBAL-DEFINE WS_VISIBLE         268435456  /* 0x10000000L */
&GLOBAL-DEFINE WS_CHILDWINDOW      {&WS_CHILD}
&GLOBAL-DEFINE WS_SIZEBOX          {&WS_THICKFRAME}


  /*===========================================*/
  /*==   WINDOW MESSAGES                     ==*/
  /*===========================================*/

&GLOBAL-DEFINE WM_KEYFIRST              256      /* 0x0100 */
&GLOBAL-DEFINE WM_KEYDOWN               256      /* 0x0100 */
&GLOBAL-DEFINE WM_KEYUP                 257      /* 0x0101 */

&GLOBAL-DEFINE WM_MOUSEFIRST            512      /* 0x0200 */
&GLOBAL-DEFINE WM_MOUSEMOVE             512      /* 0x0200 */
&GLOBAL-DEFINE WM_LBUTTONDOWN           513      /* 0x0201 */
&GLOBAL-DEFINE WM_LBUTTONUP             514      /* 0x0202 */
&GLOBAL-DEFINE WM_LBUTTONDBLCLK         515      /* 0x0203 */
&GLOBAL-DEFINE WM_RBUTTONDOWN           516      /* 0x0204 */
&GLOBAL-DEFINE WM_RBUTTONUP             517      /* 0x0205 */
&GLOBAL-DEFINE WM_RBUTTONDBLCLK         518      /* 0x0206 */
&GLOBAL-DEFINE WM_MBUTTONDOWN           519      /* 0x0207 */
&GLOBAL-DEFINE WM_MBUTTONUP             520      /* 0x0208 */
&GLOBAL-DEFINE WM_MBUTTONDBLCLK         521      /* 0x0209 */  
&GLOBAL-DEFINE WM_QUIT                   18

&GLOBAL-DEFINE WM_SIZING                532     /*  0x0214 */
&GLOBAL-DEFINE WM_MOVING                534     /*  0x0216 */
&GLOBAL-DEFINE WM_DEVICECHANGE          537     /*  0x0219 */

&GLOBAL-DEFINE WM_USER                 1024     /*  0x0400 */


/*==============================================*/
/*== Window field offsets for GetWindowLong() ==*/
/*==============================================*/

&GLOBAL-DEFINE GWL_EXSTYLE      (-20)
&GLOBAL-DEFINE GWL_HINSTANCE    (-6)
&GLOBAL-DEFINE GWL_HWNDPARENT   (-8)
&GLOBAL-DEFINE GWL_ID           (-12)
&GLOBAL-DEFINE GWL_STYLE        (-16)
&GLOBAL-DEFINE GWL_USERDATA     (-21) 
&GLOBAL-DEFINE GWL_WNDPROC      (-4)

&GLOBAL-DEFINE ILC_COLOR8          8
&GLOBAL-DEFINE IMAGE_BITMAP        0  
&GLOBAL-DEFINE LR_LOADFROMFILE     16
&GLOBAL-DEFINE LR_LOADTRANSPARENT  32     


&GLOBAL-DEFINE TVS_HASBUTTONS         1
&GLOBAL-DEFINE TVS_HASLINES           2
&GLOBAL-DEFINE TVS_LINESATROOT        4
&GLOBAL-DEFINE TVS_EDITLABELS         8
&GLOBAL-DEFINE TVS_DISABLEDRAGDROP    16
&GLOBAL-DEFINE TVS_SHOWSELALWAYS      32
&GLOBAL-DEFINE TVS_RTLREADING         64
&GLOBAL-DEFINE TVS_NOTOOLTIPS         128
&GLOBAL-DEFINE TVS_CHECKBOXES         256
&GLOBAL-DEFINE TVS_TRACKSELECT        512
&GLOBAL-DEFINE TVS_SINGLEEXPAND       1024
&GLOBAL-DEFINE TVS_INFOTIP            2048
&GLOBAL-DEFINE TVS_FULLROWSELECT      4096
&GLOBAL-DEFINE TVS_NOSCROLL           8192
&GLOBAL-DEFINE TVS_NONEVENHEIGHT      16384

/* Control Classes for use with InitCommonControlsEx() */
&GLOBAL-DEFINE ICC_LISTVIEW_CLASSES   1 /* listview, header */
&GLOBAL-DEFINE ICC_TREEVIEW_CLASSES   2 /* treeview, tooltips */
&GLOBAL-DEFINE ICC_BAR_CLASSES        4 /* toolbar, statusbar, trackbar, tooltips */
&GLOBAL-DEFINE ICC_TAB_CLASSES        8 /* tab, tooltips */
&GLOBAL-DEFINE ICC_UPDOWN_CLASS       16 /* updown */
&GLOBAL-DEFINE ICC_PROGRESS_CLASS     32 /* progress */
&GLOBAL-DEFINE ICC_HOTKEY_CLASS       40 /* hotkey */
&GLOBAL-DEFINE ICC_ANIMATE_CLASS      64 /* animate */
&GLOBAL-DEFINE ICC_WIN95_CLASSES      255
&GLOBAL-DEFINE ICC_DATE_CLASSES       256 /* month picker, date picker, time picker, updown */
&GLOBAL-DEFINE ICC_USEREX_CLASSES     512 /* comboex */
&GLOBAL-DEFINE ICC_COOL_CLASSES       1024 /* rebar (coolbar) control */
&GLOBAL-DEFINE ICC_INTERNET_CLASSES   2048
&GLOBAL-DEFINE ICC_PAGESCROLLER_CLASS 4096 /* page scroller */
&GLOBAL-DEFINE ICC_NATIVEFNTCTL_CLASS 8192 /* native font control */



/* TreeView Insert Flags  Begin*/
&GLOBAL-DEFINE TVIF_TEXT              1 
&GLOBAL-DEFINE TVIF_IMAGE             2
&GLOBAL-DEFINE TVIF_PARAM             4
&GLOBAL-DEFINE TVIF_SELECTEDIMAGE     32

&GLOBAL-DEFINE TVI_ROOT               -65536
&GLOBAL-DEFINE TVI_FIRST              -65535
&GLOBAL-DEFINE TVI_LAST               -65534
&GLOBAL-DEFINE TVI_SORT               -65533
&GLOBAL-DEFINE TV_FIRST               4352      
&GLOBAL-DEFINE TVM_INSERTITEMA        ({&TV_FIRST} + 0)
&GLOBAL-DEFINE TVM_SETIMAGELIST       ({&TV_FIRST} + 9)
&GLOBAL-DEFINE TVM_GETNEXTITEM        ({&TV_FIRST} + 10)
&GLOBAL-DEFINE TVM_GETITEMA           ({&TV_FIRST} + 12)           
&GLOBAL-DEFINE TVM_EXPAND             ({&TV_FIRST} + 2)
&GLOBAL-DEFINE TVE_COLLAPSE           1
&GLOBAL-DEFINE TVE_EXPAND             2
&GLOBAL-DEFINE TVE_TOGGLE             3
&GLOBAL-DEFINE TVGN_CARET             9

 /* Flags needed for Tab item */
&GLOBAL-DEFINE TCIF_TEXT               1
&GLOBAL-DEFINE TCIF_IMAGE              2
&GLOBAL-DEFINE TCIF_RTLREADING         4

 /* Tab insert message id */
&GLOBAL-DEFINE TCM_INSERTITEMA       4871
&GLOBAL-DEFINE TCM_GETCURSEL         4875
 
 /* Messages needed for setting font for tab strip */
&GLOBAL-DEFINE WM_SETFONT             48
&GLOBAL-DEFINE WM_GETFONT             49

&GLOBAL-DEFINE NO-REMOVE              0
&GLOBAL-DEFINE REMOVE                 1

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


