  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "<field1>".

  RUN JBoxDLookup.w ("<table>"
                   + ";<field1>"
                   + ";<field2>"
                  ,"WHERE true"
                  ,INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN <field1>:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
           .
    APPLY "any-printable" TO <field1>.
  END.
