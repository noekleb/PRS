/* CritLookup.i Used for all lookup triggers */

DEF VAR cLookupValue AS CHAR NO-UNDO.
RUN JBoxDLookup.w (icTableName, icFieldName, INPUT-OUTPUT cLookupValue).
IF cLookupValue NE "" THEN
  {1}:SCREEN-VALUE = ENTRY(1,cLookupValue,";").

