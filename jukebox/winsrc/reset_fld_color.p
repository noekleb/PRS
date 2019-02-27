/* reset_fld_color - clear bgcolor for input field 
   Author: Kjell Arne Ihlebek, Volvat
*/

  
DEF INPUT PARAMETER h_fld_handle   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAMETER v-old-bg-color AS INT  NO-UNDO INIT ?.
DEF INPUT PARAMETER v-old-fg-color AS INT  NO-UNDO INIT ?.
DEF INPUT PARAMETER v-bg-color     AS INT  NO-UNDO INIT ?.
DEF INPUT PARAMETER v-fg-color     AS INT  NO-UNDO INIT ?.


h_fld_handle = h_fld_handle:FIRST-CHILD.

DO WHILE VALID-HANDLE(h_fld_handle).
  IF CAN-QUERY(h_fld_handle,"TYPE") 
     AND (IF v-bg-color = ? THEN YES ELSE h_fld_handle:BGCOLOR = v-bg-color)
     AND h_fld_handle:FGCOLOR = v-fg-color
     AND h_fld_handle:PRIVATE-DATA NE "UNCLEAR"
     AND (IF v-bg-color = ? THEN YES ELSE (h_fld_handle:SENSITIVE OR (CAN-QUERY(h_fld_handle,"read-only") AND NOT h_fld_handle:READ-ONLY)))
     AND CAN-DO("fill-in,editor,combo-box,toggle-box,radio-set",h_fld_handle:TYPE) THEN  
    ASSIGN h_fld_handle:BGCOLOR = (IF v-old-bg-color = ? AND h_fld_handle:TYPE = "combo-box" THEN 15 ELSE v-old-bg-color)
           h_fld_handle:FGCOLOR = v-old-fg-color 
           NO-ERROR.

  h_fld_handle = h_fld_handle:NEXT-SIBLING.
END.  

