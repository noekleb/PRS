  /*=================================================================*/
  /*  Procedures for the progression bar. */
  
  
&IF DEFINED(WND_PROCEDURES-PLEASE) = 0 &THEN
&SCOPED-DEFINE WND_PROCEDURES-PLEASE YES
&ENDIF

&IF DEFINED(WND_PROCEDURES-PLEASE) <> 0 AND {&WND_PROCEDURES-PLEASE} = YES &THEN  
  
  
  PROCEDURE ProgBarCreate EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT PARAMETER PGB_hFRAME              AS LONG.
      DEFINE RETURN PARAMETER PGB_hProgressBarObject AS LONG.
  END PROCEDURE.
  
  PROCEDURE ProgBarSetProperties EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iBarStyle          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iBeginColor        AS LONG.
      DEFINE INPUT  PARAMETER PGB_iEndColor          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iMaxValue          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iMinValue          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iOrientation       AS LONG.
      DEFINE RETURN PARAMETER PGB_bStatus            AS LONG.
  END PROCEDURE.
  
  PROCEDURE ProgBarSetPosition EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject  AS LONG.
      DEFINE INPUT  PARAMETER PGB_iPosition           AS LONG.
      DEFINE RETURN PARAMETER PGB_iReturedPosition    AS LONG.
  END PROCEDURE.
  
  PROCEDURE ProgBarSetBooleanProperty EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_bValue             AS LONG.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE ProgBarSetIntegerProperty EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iValue             AS LONG.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE ProgBarSetStringProperty EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_cValue             AS CHAR.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE ProgBarDestroy EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE RETURN PARAMETER PGB_bStatus            AS LONG.
  END PROCEDURE.

/*  =========================================================
    Multi-Threaded Version of the Progress bar.
    Note:  At this time you can have only one instance of this
           object at a time.
    =========================================================*/

  PROCEDURE ThreadProgBarCreate EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT PARAMETER PGB_hFRAME              AS LONG.
      DEFINE RETURN PARAMETER PGB_hProgressBarObject AS LONG.
  END PROCEDURE.

  PROCEDURE ThreadProgBarSetProperties EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iBarStyle          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iBeginColor        AS LONG.
      DEFINE INPUT  PARAMETER PGB_iEndColor          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iMaxValue          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iMinValue          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iOrientation       AS LONG.
      DEFINE RETURN PARAMETER PGB_bStatus            AS LONG.
  END PROCEDURE.
  
  PROCEDURE ThreadProgBarSetPosition EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject  AS LONG.
      DEFINE INPUT  PARAMETER PGB_iPosition           AS LONG.
      DEFINE RETURN PARAMETER PGB_iReturedPosition    AS LONG.
  END PROCEDURE.
  
  PROCEDURE ThreadProgBarDestroy EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE RETURN PARAMETER PGB_bStatus            AS LONG.
  END PROCEDURE.

  PROCEDURE ThreadProgBarSetBooleanProperty EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_bValue             AS LONG.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE ThreadProgBarSetIntegerProperty EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_iValue             AS LONG.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE ThreadProgBarSetStringProperty EXTERNAL "ThreadProgBar.DLL":U:
      DEFINE INPUT  PARAMETER PGB_hProgressBarObject AS LONG.
      DEFINE INPUT  PARAMETER PGB_iProperty          AS LONG.
      DEFINE INPUT  PARAMETER PGB_cValue             AS CHAR.
      DEFINE RETURN PARAMETER PGB_iSuccess           AS LONG.
  END PROCEDURE.
&ENDIF
/*================================================================
  Bar styles for the Progression Bar control
==================================================================*/
&GLOBAL-DEFINE PROGBAR_STYLE-SOLID         0
&GLOBAL-DEFINE PROGBAR_STYLE-LEDS          1
&GLOBAL-DEFINE PROGBAR_STYLE-GRADIENT      2
&GLOBAL-DEFINE PROGBAR_STYLE-GRADIENTLEDS  3
&GLOBAL-DEFINE PROGBAR_STYLE-RESERVED1     4
&GLOBAL-DEFINE PROGBAR_STYLE-RESERVED2     5
&GLOBAL-DEFINE PROGBAR_STYLE-ANIMATION     6
&GLOBAL-DEFINE PROGBAR_STYLE-ANIMATIONLEDS 7

/*================================================================
  Orientation for the Progression Bar control
==================================================================*/
&GLOBAL-DEFINE PROGBAR_ORIENT-VERTICAL    0
&GLOBAL-DEFINE PROGBAR_ORIENT-HORIZONTAL  1

/*================================================================
  Widget properties for the Progression Bar control
==================================================================*/
&GLOBAL-DEFINE PROGBAR_PROP-ANIMATIONPATH		    1
&GLOBAL-DEFINE PROGBAR_PROP-ANIMATIONRESTARTDELAY	2
&GLOBAL-DEFINE PROGBAR_PROP-ANIMATIONSPEED		    3
&GLOBAL-DEFINE PROGBAR_PROP-BARBEVELOUTER		    4
&GLOBAL-DEFINE PROGBAR_PROP-BARSTYLE			    5
&GLOBAL-DEFINE PROGBAR_PROP-BEGINCOLOR			    6
&GLOBAL-DEFINE PROGBAR_PROP-ENDCOLOR			    7
&GLOBAL-DEFINE PROGBAR_PROP-FOREGROUNDIMAGE		    8
&GLOBAL-DEFINE PROGBAR_PROP-MARQUEE			        9
&GLOBAL-DEFINE PROGBAR_PROP-MAXVALUE			    10
&GLOBAL-DEFINE PROGBAR_PROP-MINVALUE			    11
&GLOBAL-DEFINE PROGBAR_PROP-ORIENTATION			    12
&GLOBAL-DEFINE PROGBAR_PROP-OVERLOADBEGINCOLOR		13
&GLOBAL-DEFINE PROGBAR_PROP-OVERLOADENDCOLOR		14
&GLOBAL-DEFINE PROGBAR_PROP-OVERLOADVALUE		    15
&GLOBAL-DEFINE PROGBAR_PROP-PEAKCOLOR			    16
&GLOBAL-DEFINE PROGBAR_PROP-PEAKSIZE			    17
&GLOBAL-DEFINE PROGBAR_PROP-PEAKVALUE			    18
&GLOBAL-DEFINE PROGBAR_PROP-SHOWOVERLOAD		    19
&GLOBAL-DEFINE PROGBAR_PROP-SHOWPEAK			    20
&GLOBAL-DEFINE PROGBAR_PROP-SHOWTEXT			    21
&GLOBAL-DEFINE PROGBAR_PROP-SHOWTEXTSTYLE		    22
&GLOBAL-DEFINE PROGBAR_PROP-SOLIDTEXTCOLOR		    23
&GLOBAL-DEFINE PROGBAR_PROP-TEXT			        24
&GLOBAL-DEFINE PROGBAR_PROP-TEXTORIENTATION		    25
&GLOBAL-DEFINE PROGBAR_PROP-TRANSPARENT			    26
&GLOBAL-DEFINE PROGBAR_PROP-TRANSPARENTIMAGE		27
&GLOBAL-DEFINE PROGBAR_PROP-TIMER                   28
&GLOBAL-DEFINE PROGBAR_PROP-TIMER-TIMEOUT           29
/*================================================================
  Animation properties for the Progression Bar control
==================================================================*/
&GLOBAL-DEFINE PROGBAR_ANIMPATH-CYCLE   		0
&GLOBAL-DEFINE PROGBAR_ANIMPATH-PINGPONG        1

/*================================================================
  Text properties for the Progression Bar control
==================================================================*/
&GLOBAL-DEFINE PROGBAR_TEXTSTYLE-PERCENT		1
&GLOBAL-DEFINE PROGBAR_TEXTSTYLE-POSITION		2
&GLOBAL-DEFINE PROGBAR_TEXTSTYLE-TEXT			3
