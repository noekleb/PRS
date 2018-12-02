/*      gradient.i
        ==========
        Copyright 4GL & M. FONDACCI 1998-1999
        
        input parameters :
                    &FRAME      undefined = frame {&frame-name}
                    &COLOR      = BLUE,RED,YELLOW,GRAY,GREEN,ORANGE
                    
        */
        
&IF "{&FRAME}" = "" &THEN
    &SCOP FRAME    FRAME {&FRAME-NAME}
    &ENDIF

&IF DEFINED( GRADIENT ) = 0 &THEN
        Def var I-COLOR      as int          NO-UNDO.
        
        Def var h_BackGround as handle       NO-UNDO.
        Def var h_Group      as HANDLE       NO-UNDO.
        Def var h_Field      as HANDLE       NO-UNDO.
        Def var Height_Rectangle_Pixels         as INT          NO-UNDO.
        Def var Height_Frame                    as INT          NO-UNDO.
        Def var N_Rectangles as INT          NO-UNDO.
        Def var     Red-Value       as INT NO-UNDO.
        Def var     Green-Value     as INT NO-UNDO.
        Def var     Blue-Value      as INT NO-UNDO  initial 10.
        Def var     AddRed          as int NO-UNDO.
        Def var     AddGreen        as int NO-UNDO.
        Def var     AddBlue         as int NO-UNDO  initial 4.        
        Def var     GapColor        as int NO-UNDO  initial 4.
        &GLOBAL GRADIENT OK
&ENDIF

/*  All is in the background container...
    ===================================*/

      h_BackGround = {&FRAME}:BACKGROUND.    
           
/*  ==============================================*/
   
IF COLOR-TABLE:NUM-ENTRIES <> 256 then 
                        COLOR-TABLE:NUM-ENTRIES = 256.

&IF "{&COLOR}"  = "" &THEN
        &SCOP COLOR "BLUE"
        &ENDIF 


        
CASE {&COLOR} :
     WHEN "BLUE" THEN
            Assign   Red-Value     =  0
                     Green-Value   =  0
                     Blue-Value    =  0
                     AddRED   = 0
                     AddGREEN = 0
                     AddBLUE  = GapColor.
    WHEN "RED" THEN
            ASSIGN  Red-Value      = 10
                    Green-Value    = 0
                    Blue-Value     = 0
                    AddRED   = GapColor
                    AddGREEN = 0
                    AddBLUE  = 0.
    WHEN "GRAY" THEN
            ASSIGN  Red-Value     =  0
                    Green-Value   =  0
                    Blue-Value    =  0
                    AddRED   = GapColor
                    AddGREEN = GapColor
                    AddBLUE  = GapColor.
    WHEN "GREEN" THEN
            ASSIGN  Red-Value     =  0
                    Green-Value   =  0
                    Blue-Value    =  0
                    AddRED   = 0
                    AddGREEN = GapColor
                    AddBLUE  = 0.
    WHEN "PURPLE" THEN
            ASSIGN  Red-Value     =  0
                    Green-Value   =  0
                    Blue-Value    =  0
                    AddRED   = 0
                    AddGREEN = GapColor
                    AddBLUE  = GapColor.   
    WHEN "YELLOW" THEN
            ASSIGN  Red-Value     =  0
                    Green-Value   =  0
                    Blue-Value    =  0
                    AddRED   = GapColor
                    AddGREEN = GapColor
                    AddBLUE  = 0.                                
    END CASE.                                                

&SCOP       LAST        164
&SCOP       FIRST       100

            
IF  COLOR-TABLE:GET-RED-VALUE(   {&LAST} - 1) <> RED-VALUE   + ( {&LAST} - {&first} - 1) * AddRED
OR  COLOR-TABLE:GET-GREEN-VALUE( {&LAST} - 1) <> GREEN-VALUE + ( {&LAST} - {&first} - 1) * AddGREEN
OR  COLOR-TABLE:GET-BLUE-VALUE(  {&LAST} - 1) <> BLUE-VALUE  + ( {&LAST} - {&first} - 1) * AddBLUE THEN
           DO I-COLOR = {&FIRST} to {&LAST} :
                                COLOR-TABLE:SET-DYNAMIC(I-COLOR, yes).  
                                COLOR-TABLE:Set-red-value(I-COLOR, Red-Value).
                                COLOR-TABLE:Set-green-value(I-COLOR, Green-Value).
                                COLOR-TABLE:Set-blue-value(I-COLOR,  Blue-Value).     
                                Assign  Red-Value   = Red-Value     + AddRED
                                        Green-Value = Green-Value   + AddGREEN
                                        Blue-Value  = Blue-Value    + AddBLUE.
                                end.


/*      Calculate the width of rectangles
        -------------------------------*/

Height_Rectangle_Pixels = {&FRAME}:HEIGHT-PIXELS   /  ({&LAST} - {&FIRST} ) 

&IF     "{&STYLE}" = "MEDIUM" &THEN    * 1.5
&ELSEIF "{&STYLE}" = "LARGE"  &THEN    * 2.5
&ENDIF
   + 1
   .
   
/*      # of rectangles to draw :
        =======================*/
   
   HEIGHT_FRAME = {&FRAME}:HEIGHT-pixels - {&FRAME}:BORDER-TOP-PIXELS
                                         - {&FRAME}:BORDER-BOTTOM-PIXELS.
                                         
   N_Rectangles = TRUNCATE( HEIGHT_FRAME / Height_Rectangle_Pixels, 0).     

   If  HEIGHT_FRAME MOD Height_Rectangle_Pixels > 0 then
            N_Rectangles = N_Rectangles + 1.
                        
   /*           Gradient already exists ?
                =======================*/
                
   h_Field = h_BackGround:FIRST-CHILD.

 
   I-Color = 1.
   
   If NOT valid-Handle(h_Field)
   OR     h_Field:Type <> "RECTANGLE"
   OR     h_Field:BGCOLOR < {&FIRST} THEN do I-color = 1 to n_Rectangles : 
        
        CREATE rectangle h_Field
                ASSIGN Y                = (I-COLOR - 1) * Height_Rectangle_Pixels
                       X                = 0
                       WIDTH-PIXELS     = {&FRAME}:Width-pixels 
                                                - {&FRAME}:BORDER-RIGHT-PIXELS
                                                - {&FRAME}:BORDER-LEFT-PIXELS
                       HEIGHT-PIXELS    = MIN( Height_Rectangle_Pixels,
                                               Height_FRAME - h_Field:Y
                                             )
                       FILLED           = true
                       BGCOLOR          = I-COLOR + 99
                       EDGE-PIXELS      = 0 
                       PARENT           = h_BackGround
                       VISIBLE          = TRUE.   
        If h_Field:MOVE-TO-BOTTOM() THEN.           /* For compatibility with 8.1 */
        END.
  
  
/*      Set to 'transparent' background all litterals, radio, toggle, slider or text widgets
        ====================================================================================
                    Scanning is made on every object with an undefined (?) background color.
                    I will find the color of the most accurate rectangle in the background, 
                    then set the background color of the object to this value.
                                        ( My personal copyright ! )
                    If the foreground object color is undefined, setting of white color
                    for the foreground too.
        */
      
   h_Group = {&FRAME}:FIRST-CHILD.
   Do while valid-handle( h_Group ) :
        h_Field     =  h_Group:FIRST-CHILD.
        DO WHILE VALID-HANDLE(h_Field) :
                 If can-do("LITERAL,RADIO-SET,TOGGLE-BOX,SLIDER,TEXT", h_Field:Type)
                 AND h_Field:BGCOLOR = ? then do :
                            /* What's the middle of object ?
                               ===========================*/
                            I-COLOR = h_Field:Y + h_Field:height-pixels / 2.    /* Middle of object */
                            /*  What's the best color for this object ?
                                -------------------------------------*/
                            h_Field:BGCOLOR = {&FIRST} + ( I-COLOR / Height_Rectangle_Pixels).
                            If  h_Field:FGCOLOR = ? then
                                    h_Field:FGCOLOR = 15.                       /* set to white foreground */
                            end.
                 h_Field = h_Field:NEXT-SIBLING.
                 END.
        h_Group = h_Group:NEXT-SIBLING.
        END.
        
        
/*      That's all folks !!!
        ==================*/

