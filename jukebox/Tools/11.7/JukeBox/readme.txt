Overrides to reshape AppBuilder for 3-tier:

1. Main objectives
- Do a little as possible in standard programs - they are subject to change 
- Change (fields to variables) or append (browse/query) output from tools
- Enable custom objects
- Enable custom properties for standard widgets (f.ex .net calendar for date field)

2. Processes and hooks

2.1 Enable custom tools on palette
Palette is modified in adeuib/cr_pal.p:
Added palette type (group) 2 with JukeBox objects (only if oeideisrunning)
Palette items are created as copies of original items (for simplicity)

2.2 Invoke custom tool
When a palette tool is selected a corresponding is called in adeuib/_uibmain.p procedure 

