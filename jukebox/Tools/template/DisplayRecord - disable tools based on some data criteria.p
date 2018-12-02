/* When multiple queries always check which is the current one: */
IF <QueryObject>:IsCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  <ToolbarObject>:disabledTools = "new,edit,..".
END.

RUN SUPER. 

