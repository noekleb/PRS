/* fld_color.i - 
   Set bg/fgcolor according to focus
   Author: Kjell Arne Ihlebek, Volvat
 - Must be invoked before conttrigg.i / supptrigg.i that checks for preprosessor directive
*/

DEF VAR v-old-bg-color AS INT NO-UNDO INIT ?.
DEF VAR v-old-fg-color AS INT NO-UNDO INIT ?.
DEF VAR v-bg-color     AS INT NO-UNDO INIT 11.
DEF VAR v-fg-color     AS INT NO-UNDO INIT ?.

&GLOBAL-DEFINE use_fld_color 

ON 'entry':U OF FRAME {&FRAME-NAME} ANYWHERE
DO:

  IF v-bg-color = ?
     OR NOT CAN-QUERY(SELF,"TYPE") 
     OR NOT SELF:SENSITIVE
     OR (CAN-QUERY(SELF,"read-only") AND SELF:READ-ONLY)
    THEN LEAVE.
  ELSE IF v-bg-color NE ?
     AND CAN-QUERY(SELF,"TYPE") 
     AND (NOT SELF:SENSITIVE OR (CAN-QUERY(SELF,"read-only") AND SELF:READ-ONLY))
    THEN DO:
    RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,
                           v-old-bg-color,
                           v-old-fg-color,
                           v-bg-color,
                           v-fg-color).
    LEAVE.
  END.

  RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,
                         v-old-bg-color,
                         v-old-fg-color,
                         v-bg-color,
                         v-fg-color).

  IF CAN-DO("fill-in,editor,combo-box,toggle-box,radio-set",SELF:TYPE) THEN 
    ASSIGN v-old-bg-color = SELF:BGCOLOR 
           v-old-fg-color = SELF:FGCOLOR 
           SELF:BGCOLOR = v-bg-color
           SELF:FGCOLOR = v-fg-color 
           NO-ERROR.

END.
