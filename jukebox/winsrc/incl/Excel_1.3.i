/* Type Library definitions dumped by TLibDef ©Chemix PLC Oct 2000            */
/*  Portions Copyright 1996-1997, Polar Engineering and Consulting,           */
/*  http://www.kenai.com/vb4tools                                             */

/* Generated on :30/10/2002 at: 12:15:32                                      */
/* Library Name : Excel                                                       */
/* Version      : 1.3                                                         */
/* Help File    : Microsoft Excel 9.0 Object Library                          */
/* GUID         : {00020813-0000-0000-C000-000000000046}                      */
/* From File    : C:\Program Files\Microsoft Office\Office\EXCEL9.OLB         */
/* Help File    : C:\Program Files\Microsoft Office\Office\VBAXL9.CHM         */

&If DEFINED (Excel_Constants) = 0 &THEN
&GLOBAL-DEFINE Excel_Constants 1

/* Enum Constants   -                                                         */
&GLOBAL-DEFINE xlAll -4104
&GLOBAL-DEFINE xlAutomatic -4105
&GLOBAL-DEFINE xlBoth 1
&GLOBAL-DEFINE xlCenter -4108
&GLOBAL-DEFINE xlChecker 9
&GLOBAL-DEFINE xlCircle 8
&GLOBAL-DEFINE xlCorner 2
&GLOBAL-DEFINE xlCrissCross 16
&GLOBAL-DEFINE xlCross 4
&GLOBAL-DEFINE xlDiamond 2
&GLOBAL-DEFINE xlDistributed -4117
&GLOBAL-DEFINE xlDoubleAccounting 5
&GLOBAL-DEFINE xlFixedValue 1
&GLOBAL-DEFINE xlFormats -4122
&GLOBAL-DEFINE xlGray16 17
&GLOBAL-DEFINE xlGray8 18
&GLOBAL-DEFINE xlGrid 15
&GLOBAL-DEFINE xlHigh -4127
&GLOBAL-DEFINE xlInside 2
&GLOBAL-DEFINE xlJustify -4130
&GLOBAL-DEFINE xlLightDown 13
&GLOBAL-DEFINE xlLightHorizontal 11
&GLOBAL-DEFINE xlLightUp 14
&GLOBAL-DEFINE xlLightVertical 12
&GLOBAL-DEFINE xlLow -4134
&GLOBAL-DEFINE xlManual -4135
&GLOBAL-DEFINE xlMinusValues 3
&GLOBAL-DEFINE xlModule -4141
&GLOBAL-DEFINE xlNextToAxis 4
&GLOBAL-DEFINE xlNone -4142
&GLOBAL-DEFINE xlNotes -4144
&GLOBAL-DEFINE xlOff -4146
&GLOBAL-DEFINE xlOn 1
&GLOBAL-DEFINE xlPercent 2
&GLOBAL-DEFINE xlPlus 9
&GLOBAL-DEFINE xlPlusValues 2
&GLOBAL-DEFINE xlSemiGray75 10
&GLOBAL-DEFINE xlShowLabel 4
&GLOBAL-DEFINE xlShowLabelAndPercent 5
&GLOBAL-DEFINE xlShowPercent 3
&GLOBAL-DEFINE xlShowValue 2
&GLOBAL-DEFINE xlSimple -4154
&GLOBAL-DEFINE xlSingle 2
&GLOBAL-DEFINE xlSingleAccounting 4
&GLOBAL-DEFINE xlSolid 1
&GLOBAL-DEFINE xlSquare 1
&GLOBAL-DEFINE xlStar 5
&GLOBAL-DEFINE xlStError 4
&GLOBAL-DEFINE xlToolbarButton 2
&GLOBAL-DEFINE xlTriangle 3
&GLOBAL-DEFINE xlGray25 -4124
&GLOBAL-DEFINE xlGray50 -4125
&GLOBAL-DEFINE xlGray75 -4126
&GLOBAL-DEFINE xlBottom -4107
&GLOBAL-DEFINE xlLeft -4131
&GLOBAL-DEFINE xlRight -4152
&GLOBAL-DEFINE xlTop -4160
&GLOBAL-DEFINE xl3DBar -4099
&GLOBAL-DEFINE xl3DSurface -4103
&GLOBAL-DEFINE xlBar 2
&GLOBAL-DEFINE xlColumn 3
&GLOBAL-DEFINE xlCombination -4111
&GLOBAL-DEFINE xlCustom -4114
&GLOBAL-DEFINE xlDefaultAutoFormat -1
&GLOBAL-DEFINE xlMaximum 2
&GLOBAL-DEFINE xlMinimum 4
&GLOBAL-DEFINE xlOpaque 3
&GLOBAL-DEFINE xlTransparent 2
&GLOBAL-DEFINE xlBidi -5000
&GLOBAL-DEFINE xlLatin -5001
&GLOBAL-DEFINE xlContext -5002
&GLOBAL-DEFINE xlLTR -5003
&GLOBAL-DEFINE xlRTL -5004
&GLOBAL-DEFINE xlFullScript 1
&GLOBAL-DEFINE xlPartialScript 2
&GLOBAL-DEFINE xlMixedScript 3
&GLOBAL-DEFINE xlMixedAuthorizedScript 4
&GLOBAL-DEFINE xlVisualCursor 2
&GLOBAL-DEFINE xlLogicalCursor 1
&GLOBAL-DEFINE xlSystem 1
&GLOBAL-DEFINE xlPartial 3
&GLOBAL-DEFINE xlHindiNumerals 3
&GLOBAL-DEFINE xlBidiCalendar 3
&GLOBAL-DEFINE xlGregorian 2
&GLOBAL-DEFINE xlComplete 4
&GLOBAL-DEFINE xlScale 3
&GLOBAL-DEFINE xlClosed 3
&GLOBAL-DEFINE xlColor1 7
&GLOBAL-DEFINE xlColor2 8
&GLOBAL-DEFINE xlColor3 9
&GLOBAL-DEFINE xlConstants 2
&GLOBAL-DEFINE xlContents 2
&GLOBAL-DEFINE xlBelow 1
&GLOBAL-DEFINE xlCascade 7
&GLOBAL-DEFINE xlCenterAcrossSelection 7
&GLOBAL-DEFINE xlChart4 2
&GLOBAL-DEFINE xlChartSeries 17
&GLOBAL-DEFINE xlChartShort 6
&GLOBAL-DEFINE xlChartTitles 18
&GLOBAL-DEFINE xlClassic1 1
&GLOBAL-DEFINE xlClassic2 2
&GLOBAL-DEFINE xlClassic3 3
&GLOBAL-DEFINE xl3DEffects1 13
&GLOBAL-DEFINE xl3DEffects2 14
&GLOBAL-DEFINE xlAbove 0
&GLOBAL-DEFINE xlAccounting1 4
&GLOBAL-DEFINE xlAccounting2 5
&GLOBAL-DEFINE xlAccounting3 6
&GLOBAL-DEFINE xlAccounting4 17
&GLOBAL-DEFINE xlAdd 2
&GLOBAL-DEFINE xlDebugCodePane 13
&GLOBAL-DEFINE xlDesktop 9
&GLOBAL-DEFINE xlDirect 1
&GLOBAL-DEFINE xlDivide 5
&GLOBAL-DEFINE xlDoubleClosed 5
&GLOBAL-DEFINE xlDoubleOpen 4
&GLOBAL-DEFINE xlDoubleQuote 1
&GLOBAL-DEFINE xlEntireChart 20
&GLOBAL-DEFINE xlExcelMenus 1
&GLOBAL-DEFINE xlExtended 3
&GLOBAL-DEFINE xlFill 5
&GLOBAL-DEFINE xlFirst 0
&GLOBAL-DEFINE xlFloating 5
&GLOBAL-DEFINE xlFormula 5
&GLOBAL-DEFINE xlGeneral 1
&GLOBAL-DEFINE xlGridline 22
&GLOBAL-DEFINE xlIcons 1
&GLOBAL-DEFINE xlImmediatePane 12
&GLOBAL-DEFINE xlInteger 2
&GLOBAL-DEFINE xlLast 1
&GLOBAL-DEFINE xlLastCell 11
&GLOBAL-DEFINE xlList1 10
&GLOBAL-DEFINE xlList2 11
&GLOBAL-DEFINE xlList3 12
&GLOBAL-DEFINE xlLocalFormat1 15
&GLOBAL-DEFINE xlLocalFormat2 16
&GLOBAL-DEFINE xlLong 3
&GLOBAL-DEFINE xlLotusHelp 2
&GLOBAL-DEFINE xlMacrosheetCell 7
&GLOBAL-DEFINE xlMixed 2
&GLOBAL-DEFINE xlMultiply 4
&GLOBAL-DEFINE xlNarrow 1
&GLOBAL-DEFINE xlNoDocuments 3
&GLOBAL-DEFINE xlOpen 2
&GLOBAL-DEFINE xlOutside 3
&GLOBAL-DEFINE xlReference 4
&GLOBAL-DEFINE xlSemiautomatic 2
&GLOBAL-DEFINE xlShort 1
&GLOBAL-DEFINE xlSingleQuote 2
&GLOBAL-DEFINE xlStrict 2
&GLOBAL-DEFINE xlSubtract 3
&GLOBAL-DEFINE xlTextBox 16
&GLOBAL-DEFINE xlTiled 1
&GLOBAL-DEFINE xlTitleBar 8
&GLOBAL-DEFINE xlToolbar 1
&GLOBAL-DEFINE xlVisible 12
&GLOBAL-DEFINE xlWatchPane 11
&GLOBAL-DEFINE xlWide 3
&GLOBAL-DEFINE xlWorkbookTab 6
&GLOBAL-DEFINE xlWorksheet4 1
&GLOBAL-DEFINE xlWorksheetCell 3
&GLOBAL-DEFINE xlWorksheetShort 5
&GLOBAL-DEFINE xlAllExceptBorders 6
&GLOBAL-DEFINE xlLeftToRight 2
&GLOBAL-DEFINE xlTopToBottom 1
&GLOBAL-DEFINE xlVeryHidden 2
&GLOBAL-DEFINE xlDrawingObject 14

/* Enum XlCreator   -                                                         */
&GLOBAL-DEFINE xlCreatorCode 1480803660

/* Enum XlChartGallery   -                                                    */
&GLOBAL-DEFINE xlBuiltIn 21
&GLOBAL-DEFINE xlUserDefined 22
&GLOBAL-DEFINE xlAnyGallery 23

/* Enum XlColorIndex   -                                                      */
&GLOBAL-DEFINE xlColorIndexAutomatic -4105
&GLOBAL-DEFINE xlColorIndexNone -4142

/* Enum XlEndStyleCap   -                                                     */
&GLOBAL-DEFINE xlCap 1
&GLOBAL-DEFINE xlNoCap 2

/* Enum XlRowCol   -                                                          */
&GLOBAL-DEFINE xlColumns 2
&GLOBAL-DEFINE xlRows 1

/* Enum XlScaleType   -                                                       */
&GLOBAL-DEFINE xlScaleLinear -4132
&GLOBAL-DEFINE xlScaleLogarithmic -4133

/* Enum XlDataSeriesType   -                                                  */
&GLOBAL-DEFINE xlAutoFill 4
&GLOBAL-DEFINE xlChronological 3
&GLOBAL-DEFINE xlGrowth 2
&GLOBAL-DEFINE xlDataSeriesLinear -4132

/* Enum XlAxisCrosses   -                                                     */
&GLOBAL-DEFINE xlAxisCrossesAutomatic -4105
&GLOBAL-DEFINE xlAxisCrossesCustom -4114
&GLOBAL-DEFINE xlAxisCrossesMaximum 2
&GLOBAL-DEFINE xlAxisCrossesMinimum 4

/* Enum XlAxisGroup   -                                                       */
&GLOBAL-DEFINE xlPrimary 1
&GLOBAL-DEFINE xlSecondary 2

/* Enum XlBackground   -                                                      */
&GLOBAL-DEFINE xlBackgroundAutomatic -4105
&GLOBAL-DEFINE xlBackgroundOpaque 3
&GLOBAL-DEFINE xlBackgroundTransparent 2

/* Enum XlWindowState   -                                                     */
&GLOBAL-DEFINE xlMaximized -4137
&GLOBAL-DEFINE xlMinimized -4140
&GLOBAL-DEFINE xlNormal -4143

/* Enum XlAxisType   -                                                        */
&GLOBAL-DEFINE xlCategory 1
&GLOBAL-DEFINE xlSeriesAxis 3
&GLOBAL-DEFINE xlValue 2

/* Enum XlArrowHeadLength   -                                                 */
&GLOBAL-DEFINE xlArrowHeadLengthLong 3
&GLOBAL-DEFINE xlArrowHeadLengthMedium -4138
&GLOBAL-DEFINE xlArrowHeadLengthShort 1

/* Enum XlVAlign   -                                                          */
&GLOBAL-DEFINE xlVAlignBottom -4107
&GLOBAL-DEFINE xlVAlignCenter -4108
&GLOBAL-DEFINE xlVAlignDistributed -4117
&GLOBAL-DEFINE xlVAlignJustify -4130
&GLOBAL-DEFINE xlVAlignTop -4160

/* Enum XlTickMark   -                                                        */
&GLOBAL-DEFINE xlTickMarkCross 4
&GLOBAL-DEFINE xlTickMarkInside 2
&GLOBAL-DEFINE xlTickMarkNone -4142
&GLOBAL-DEFINE xlTickMarkOutside 3

/* Enum XlErrorBarDirection   -                                               */
&GLOBAL-DEFINE xlX -4168
&GLOBAL-DEFINE xlY 1

/* Enum XlErrorBarInclude   -                                                 */
&GLOBAL-DEFINE xlErrorBarIncludeBoth 1
&GLOBAL-DEFINE xlErrorBarIncludeMinusValues 3
&GLOBAL-DEFINE xlErrorBarIncludeNone -4142
&GLOBAL-DEFINE xlErrorBarIncludePlusValues 2

/* Enum XlDisplayBlanksAs   -                                                 */
&GLOBAL-DEFINE xlInterpolated 3
&GLOBAL-DEFINE xlNotPlotted 1
&GLOBAL-DEFINE xlZero 2

/* Enum XlArrowHeadStyle   -                                                  */
&GLOBAL-DEFINE xlArrowHeadStyleClosed 3
&GLOBAL-DEFINE xlArrowHeadStyleDoubleClosed 5
&GLOBAL-DEFINE xlArrowHeadStyleDoubleOpen 4
&GLOBAL-DEFINE xlArrowHeadStyleNone -4142
&GLOBAL-DEFINE xlArrowHeadStyleOpen 2

/* Enum XlArrowHeadWidth   -                                                  */
&GLOBAL-DEFINE xlArrowHeadWidthMedium -4138
&GLOBAL-DEFINE xlArrowHeadWidthNarrow 1
&GLOBAL-DEFINE xlArrowHeadWidthWide 3

/* Enum XlHAlign   -                                                          */
&GLOBAL-DEFINE xlHAlignCenter -4108
&GLOBAL-DEFINE xlHAlignCenterAcrossSelection 7
&GLOBAL-DEFINE xlHAlignDistributed -4117
&GLOBAL-DEFINE xlHAlignFill 5
&GLOBAL-DEFINE xlHAlignGeneral 1
&GLOBAL-DEFINE xlHAlignJustify -4130
&GLOBAL-DEFINE xlHAlignLeft -4131
&GLOBAL-DEFINE xlHAlignRight -4152

/* Enum XlTickLabelPosition   -                                               */
&GLOBAL-DEFINE xlTickLabelPositionHigh -4127
&GLOBAL-DEFINE xlTickLabelPositionLow -4134
&GLOBAL-DEFINE xlTickLabelPositionNextToAxis 4
&GLOBAL-DEFINE xlTickLabelPositionNone -4142

/* Enum XlLegendPosition   -                                                  */
&GLOBAL-DEFINE xlLegendPositionBottom -4107
&GLOBAL-DEFINE xlLegendPositionCorner 2
&GLOBAL-DEFINE xlLegendPositionLeft -4131
&GLOBAL-DEFINE xlLegendPositionRight -4152
&GLOBAL-DEFINE xlLegendPositionTop -4160

/* Enum XlChartPictureType   -                                                */
&GLOBAL-DEFINE xlStackScale 3
&GLOBAL-DEFINE xlStack 2
&GLOBAL-DEFINE xlStretch 1

/* Enum XlChartPicturePlacement   -                                           */
&GLOBAL-DEFINE xlSides 1
&GLOBAL-DEFINE xlEnd 2
&GLOBAL-DEFINE xlEndSides 3
&GLOBAL-DEFINE xlFront 4
&GLOBAL-DEFINE xlFrontSides 5
&GLOBAL-DEFINE xlFrontEnd 6
&GLOBAL-DEFINE xlAllFaces 7

/* Enum XlOrientation   -                                                     */
&GLOBAL-DEFINE xlDownward -4170
&GLOBAL-DEFINE xlHorizontal -4128
&GLOBAL-DEFINE xlUpward -4171
&GLOBAL-DEFINE xlVertical -4166

/* Enum XlTickLabelOrientation   -                                            */
&GLOBAL-DEFINE xlTickLabelOrientationAutomatic -4105
&GLOBAL-DEFINE xlTickLabelOrientationDownward -4170
&GLOBAL-DEFINE xlTickLabelOrientationHorizontal -4128
&GLOBAL-DEFINE xlTickLabelOrientationUpward -4171
&GLOBAL-DEFINE xlTickLabelOrientationVertical -4166

/* Enum XlBorderWeight   -                                                    */
&GLOBAL-DEFINE xlHairline 1
&GLOBAL-DEFINE xlMedium -4138
&GLOBAL-DEFINE xlThick 4
&GLOBAL-DEFINE xlThin 2

/* Enum XlDataSeriesDate   -                                                  */
&GLOBAL-DEFINE xlDay 1
&GLOBAL-DEFINE xlMonth 3
&GLOBAL-DEFINE xlWeekday 2
&GLOBAL-DEFINE xlYear 4

/* Enum XlUnderlineStyle   -                                                  */
&GLOBAL-DEFINE xlUnderlineStyleDouble -4119
&GLOBAL-DEFINE xlUnderlineStyleDoubleAccounting 5
&GLOBAL-DEFINE xlUnderlineStyleNone -4142
&GLOBAL-DEFINE xlUnderlineStyleSingle 2
&GLOBAL-DEFINE xlUnderlineStyleSingleAccounting 4

/* Enum XlErrorBarType   -                                                    */
&GLOBAL-DEFINE xlErrorBarTypeCustom -4114
&GLOBAL-DEFINE xlErrorBarTypeFixedValue 1
&GLOBAL-DEFINE xlErrorBarTypePercent 2
&GLOBAL-DEFINE xlErrorBarTypeStDev -4155
&GLOBAL-DEFINE xlErrorBarTypeStError 4

/* Enum XlTrendlineType   -                                                   */
&GLOBAL-DEFINE xlExponential 5
&GLOBAL-DEFINE xlLinear -4132
&GLOBAL-DEFINE xlLogarithmic -4133
&GLOBAL-DEFINE xlMovingAvg 6
&GLOBAL-DEFINE xlPolynomial 3
&GLOBAL-DEFINE xlPower 4

/* Enum XlLineStyle   -                                                       */
&GLOBAL-DEFINE xlContinuous 1
&GLOBAL-DEFINE xlDash -4115
&GLOBAL-DEFINE xlDashDot 4
&GLOBAL-DEFINE xlDashDotDot 5
&GLOBAL-DEFINE xlDot -4118
&GLOBAL-DEFINE xlDouble -4119
&GLOBAL-DEFINE xlSlantDashDot 13
&GLOBAL-DEFINE xlLineStyleNone -4142

/* Enum XlDataLabelsType   -                                                  */
&GLOBAL-DEFINE xlDataLabelsShowNone -4142
&GLOBAL-DEFINE xlDataLabelsShowValue 2
&GLOBAL-DEFINE xlDataLabelsShowPercent 3
&GLOBAL-DEFINE xlDataLabelsShowLabel 4
&GLOBAL-DEFINE xlDataLabelsShowLabelAndPercent 5
&GLOBAL-DEFINE xlDataLabelsShowBubbleSizes 6

/* Enum XlMarkerStyle   -                                                     */
&GLOBAL-DEFINE xlMarkerStyleAutomatic -4105
&GLOBAL-DEFINE xlMarkerStyleCircle 8
&GLOBAL-DEFINE xlMarkerStyleDash -4115
&GLOBAL-DEFINE xlMarkerStyleDiamond 2
&GLOBAL-DEFINE xlMarkerStyleDot -4118
&GLOBAL-DEFINE xlMarkerStyleNone -4142
&GLOBAL-DEFINE xlMarkerStylePicture -4147
&GLOBAL-DEFINE xlMarkerStylePlus 9
&GLOBAL-DEFINE xlMarkerStyleSquare 1
&GLOBAL-DEFINE xlMarkerStyleStar 5
&GLOBAL-DEFINE xlMarkerStyleTriangle 3
&GLOBAL-DEFINE xlMarkerStyleX -4168

/* Enum XlPictureConvertorType   -                                            */
&GLOBAL-DEFINE xlBMP 1
&GLOBAL-DEFINE xlCGM 7
&GLOBAL-DEFINE xlDRW 4
&GLOBAL-DEFINE xlDXF 5
&GLOBAL-DEFINE xlEPS 8
&GLOBAL-DEFINE xlHGL 6
&GLOBAL-DEFINE xlPCT 13
&GLOBAL-DEFINE xlPCX 10
&GLOBAL-DEFINE xlPIC 11
&GLOBAL-DEFINE xlPLT 12
&GLOBAL-DEFINE xlTIF 9
&GLOBAL-DEFINE xlWMF 2
&GLOBAL-DEFINE xlWPG 3

/* Enum XlPattern   -                                                         */
&GLOBAL-DEFINE xlPatternAutomatic -4105
&GLOBAL-DEFINE xlPatternChecker 9
&GLOBAL-DEFINE xlPatternCrissCross 16
&GLOBAL-DEFINE xlPatternDown -4121
&GLOBAL-DEFINE xlPatternGray16 17
&GLOBAL-DEFINE xlPatternGray25 -4124
&GLOBAL-DEFINE xlPatternGray50 -4125
&GLOBAL-DEFINE xlPatternGray75 -4126
&GLOBAL-DEFINE xlPatternGray8 18
&GLOBAL-DEFINE xlPatternGrid 15
&GLOBAL-DEFINE xlPatternHorizontal -4128
&GLOBAL-DEFINE xlPatternLightDown 13
&GLOBAL-DEFINE xlPatternLightHorizontal 11
&GLOBAL-DEFINE xlPatternLightUp 14
&GLOBAL-DEFINE xlPatternLightVertical 12
&GLOBAL-DEFINE xlPatternNone -4142
&GLOBAL-DEFINE xlPatternSemiGray75 10
&GLOBAL-DEFINE xlPatternSolid 1
&GLOBAL-DEFINE xlPatternUp -4162
&GLOBAL-DEFINE xlPatternVertical -4166

/* Enum XlChartSplitType   -                                                  */
&GLOBAL-DEFINE xlSplitByPosition 1
&GLOBAL-DEFINE xlSplitByPercentValue 3
&GLOBAL-DEFINE xlSplitByCustomSplit 4
&GLOBAL-DEFINE xlSplitByValue 2

/* Enum XlDisplayUnit   -                                                     */
&GLOBAL-DEFINE xlHundreds -2
&GLOBAL-DEFINE xlThousands -3
&GLOBAL-DEFINE xlTenThousands -4
&GLOBAL-DEFINE xlHundredThousands -5
&GLOBAL-DEFINE xlMillions -6
&GLOBAL-DEFINE xlTenMillions -7
&GLOBAL-DEFINE xlHundredMillions -8
&GLOBAL-DEFINE xlThousandMillions -9
&GLOBAL-DEFINE xlMillionMillions -10

/* Enum XlDataLabelPosition   -                                               */
&GLOBAL-DEFINE xlLabelPositionCenter -4108
&GLOBAL-DEFINE xlLabelPositionAbove 0
&GLOBAL-DEFINE xlLabelPositionBelow 1
&GLOBAL-DEFINE xlLabelPositionLeft -4131
&GLOBAL-DEFINE xlLabelPositionRight -4152
&GLOBAL-DEFINE xlLabelPositionOutsideEnd 2
&GLOBAL-DEFINE xlLabelPositionInsideEnd 3
&GLOBAL-DEFINE xlLabelPositionInsideBase 4
&GLOBAL-DEFINE xlLabelPositionBestFit 5
&GLOBAL-DEFINE xlLabelPositionMixed 6
&GLOBAL-DEFINE xlLabelPositionCustom 7

/* Enum XlTimeUnit   -                                                        */
&GLOBAL-DEFINE xlDays 0
&GLOBAL-DEFINE xlMonths 1
&GLOBAL-DEFINE xlYears 2

/* Enum XlCategoryType   -                                                    */
&GLOBAL-DEFINE xlCategoryScale 2
&GLOBAL-DEFINE xlTimeScale 3
&GLOBAL-DEFINE xlAutomaticScale -4105

/* Enum XlBarShape   -                                                        */
&GLOBAL-DEFINE xlBox 0
&GLOBAL-DEFINE xlPyramidToPoint 1
&GLOBAL-DEFINE xlPyramidToMax 2
&GLOBAL-DEFINE xlCylinder 3
&GLOBAL-DEFINE xlConeToPoint 4
&GLOBAL-DEFINE xlConeToMax 5

/* Enum XlChartType   -                                                       */
&GLOBAL-DEFINE xlColumnClustered 51
&GLOBAL-DEFINE xlColumnStacked 52
&GLOBAL-DEFINE xlColumnStacked100 53
&GLOBAL-DEFINE xl3DColumnClustered 54
&GLOBAL-DEFINE xl3DColumnStacked 55
&GLOBAL-DEFINE xl3DColumnStacked100 56
&GLOBAL-DEFINE xlBarClustered 57
&GLOBAL-DEFINE xlBarStacked 58
&GLOBAL-DEFINE xlBarStacked100 59
&GLOBAL-DEFINE xl3DBarClustered 60
&GLOBAL-DEFINE xl3DBarStacked 61
&GLOBAL-DEFINE xl3DBarStacked100 62
&GLOBAL-DEFINE xlLineStacked 63
&GLOBAL-DEFINE xlLineStacked100 64
&GLOBAL-DEFINE xlLineMarkers 65
&GLOBAL-DEFINE xlLineMarkersStacked 66
&GLOBAL-DEFINE xlLineMarkersStacked100 67
&GLOBAL-DEFINE xlPieOfPie 68
&GLOBAL-DEFINE xlPieExploded 69
&GLOBAL-DEFINE xl3DPieExploded 70
&GLOBAL-DEFINE xlBarOfPie 71
&GLOBAL-DEFINE xlXYScatterSmooth 72
&GLOBAL-DEFINE xlXYScatterSmoothNoMarkers 73
&GLOBAL-DEFINE xlXYScatterLines 74
&GLOBAL-DEFINE xlXYScatterLinesNoMarkers 75
&GLOBAL-DEFINE xlAreaStacked 76
&GLOBAL-DEFINE xlAreaStacked100 77
&GLOBAL-DEFINE xl3DAreaStacked 78
&GLOBAL-DEFINE xl3DAreaStacked100 79
&GLOBAL-DEFINE xlDoughnutExploded 80
&GLOBAL-DEFINE xlRadarMarkers 81
&GLOBAL-DEFINE xlRadarFilled 82
&GLOBAL-DEFINE xlSurface 83
&GLOBAL-DEFINE xlSurfaceWireframe 84
&GLOBAL-DEFINE xlSurfaceTopView 85
&GLOBAL-DEFINE xlSurfaceTopViewWireframe 86
&GLOBAL-DEFINE xlBubble 15
&GLOBAL-DEFINE xlBubble3DEffect 87
&GLOBAL-DEFINE xlStockHLC 88
&GLOBAL-DEFINE xlStockOHLC 89
&GLOBAL-DEFINE xlStockVHLC 90
&GLOBAL-DEFINE xlStockVOHLC 91
&GLOBAL-DEFINE xlCylinderColClustered 92
&GLOBAL-DEFINE xlCylinderColStacked 93
&GLOBAL-DEFINE xlCylinderColStacked100 94
&GLOBAL-DEFINE xlCylinderBarClustered 95
&GLOBAL-DEFINE xlCylinderBarStacked 96
&GLOBAL-DEFINE xlCylinderBarStacked100 97
&GLOBAL-DEFINE xlCylinderCol 98
&GLOBAL-DEFINE xlConeColClustered 99
&GLOBAL-DEFINE xlConeColStacked 100
&GLOBAL-DEFINE xlConeColStacked100 101
&GLOBAL-DEFINE xlConeBarClustered 102
&GLOBAL-DEFINE xlConeBarStacked 103
&GLOBAL-DEFINE xlConeBarStacked100 104
&GLOBAL-DEFINE xlConeCol 105
&GLOBAL-DEFINE xlPyramidColClustered 106
&GLOBAL-DEFINE xlPyramidColStacked 107
&GLOBAL-DEFINE xlPyramidColStacked100 108
&GLOBAL-DEFINE xlPyramidBarClustered 109
&GLOBAL-DEFINE xlPyramidBarStacked 110
&GLOBAL-DEFINE xlPyramidBarStacked100 111
&GLOBAL-DEFINE xlPyramidCol 112
&GLOBAL-DEFINE xl3DColumn -4100
&GLOBAL-DEFINE xlLine 4
&GLOBAL-DEFINE xl3DLine -4101
&GLOBAL-DEFINE xl3DPie -4102
&GLOBAL-DEFINE xlPie 5
&GLOBAL-DEFINE xlXYScatter -4169
&GLOBAL-DEFINE xl3DArea -4098
&GLOBAL-DEFINE xlArea 1
&GLOBAL-DEFINE xlDoughnut -4120
&GLOBAL-DEFINE xlRadar -4151

/* Enum XlChartItem   -                                                       */
&GLOBAL-DEFINE xlDataLabel 0
&GLOBAL-DEFINE xlChartArea 2
&GLOBAL-DEFINE xlSeries 3
&GLOBAL-DEFINE xlChartTitle 4
&GLOBAL-DEFINE xlWalls 5
&GLOBAL-DEFINE xlCorners 6
&GLOBAL-DEFINE xlDataTable 7
&GLOBAL-DEFINE xlTrendline 8
&GLOBAL-DEFINE xlErrorBars 9
&GLOBAL-DEFINE xlXErrorBars 10
&GLOBAL-DEFINE xlYErrorBars 11
&GLOBAL-DEFINE xlLegendEntry 12
&GLOBAL-DEFINE xlLegendKey 13
&GLOBAL-DEFINE xlShape 14
&GLOBAL-DEFINE xlMajorGridlines 15
&GLOBAL-DEFINE xlMinorGridlines 16
&GLOBAL-DEFINE xlAxisTitle 17
&GLOBAL-DEFINE xlUpBars 18
&GLOBAL-DEFINE xlPlotArea 19
&GLOBAL-DEFINE xlDownBars 20
&GLOBAL-DEFINE xlAxis 21
&GLOBAL-DEFINE xlSeriesLines 22
&GLOBAL-DEFINE xlFloor 23
&GLOBAL-DEFINE xlLegend 24
&GLOBAL-DEFINE xlHiLoLines 25
&GLOBAL-DEFINE xlDropLines 26
&GLOBAL-DEFINE xlRadarAxisLabels 27
&GLOBAL-DEFINE xlNothing 28
&GLOBAL-DEFINE xlLeaderLines 29
&GLOBAL-DEFINE xlDisplayUnitLabel 30
&GLOBAL-DEFINE xlPivotChartFieldButton 31
&GLOBAL-DEFINE xlPivotChartDropZone 32

/* Enum XlSizeRepresents   -                                                  */
&GLOBAL-DEFINE xlSizeIsWidth 2
&GLOBAL-DEFINE xlSizeIsArea 1

/* Enum XlInsertShiftDirection   -                                            */
&GLOBAL-DEFINE xlShiftDown -4121
&GLOBAL-DEFINE xlShiftToRight -4161

/* Enum XlDeleteShiftDirection   -                                            */
&GLOBAL-DEFINE xlShiftToLeft -4159
&GLOBAL-DEFINE xlShiftUp -4162

/* Enum XlDirection   -                                                       */
&GLOBAL-DEFINE xlDown -4121
&GLOBAL-DEFINE xlToLeft -4159
&GLOBAL-DEFINE xlToRight -4161
&GLOBAL-DEFINE xlUp -4162

/* Enum XlConsolidationFunction   -                                           */
&GLOBAL-DEFINE xlAverage -4106
&GLOBAL-DEFINE xlCount -4112
&GLOBAL-DEFINE xlCountNums -4113
&GLOBAL-DEFINE xlMax -4136
&GLOBAL-DEFINE xlMin -4139
&GLOBAL-DEFINE xlProduct -4149
&GLOBAL-DEFINE xlStDev -4155
&GLOBAL-DEFINE xlStDevP -4156
&GLOBAL-DEFINE xlSum -4157
&GLOBAL-DEFINE xlVar -4164
&GLOBAL-DEFINE xlVarP -4165
&GLOBAL-DEFINE xlUnknown 1000

/* Enum XlSheetType   -                                                       */
&GLOBAL-DEFINE xlChart -4109
&GLOBAL-DEFINE xlDialogSheet -4116
&GLOBAL-DEFINE xlExcel4IntlMacroSheet 4
&GLOBAL-DEFINE xlExcel4MacroSheet 3
&GLOBAL-DEFINE xlWorksheet -4167

/* Enum XlLocationInTable   -                                                 */
&GLOBAL-DEFINE xlColumnHeader -4110
&GLOBAL-DEFINE xlColumnItem 5
&GLOBAL-DEFINE xlDataHeader 3
&GLOBAL-DEFINE xlDataItem 7
&GLOBAL-DEFINE xlPageHeader 2
&GLOBAL-DEFINE xlPageItem 6
&GLOBAL-DEFINE xlRowHeader -4153
&GLOBAL-DEFINE xlRowItem 4
&GLOBAL-DEFINE xlTableBody 8

/* Enum XlFindLookIn   -                                                      */
&GLOBAL-DEFINE xlFormulas -4123
&GLOBAL-DEFINE xlComments -4144
&GLOBAL-DEFINE xlValues -4163

/* Enum XlWindowType   -                                                      */
&GLOBAL-DEFINE xlChartAsWindow 5
&GLOBAL-DEFINE xlChartInPlace 4
&GLOBAL-DEFINE xlClipboard 3
&GLOBAL-DEFINE xlInfo -4129
&GLOBAL-DEFINE xlWorkbook 1

/* Enum XlPivotFieldDataType   -                                              */
&GLOBAL-DEFINE xlDate 2
&GLOBAL-DEFINE xlNumber -4145
&GLOBAL-DEFINE xlText -4158

/* Enum XlCopyPictureFormat   -                                               */
&GLOBAL-DEFINE xlBitmap 2
&GLOBAL-DEFINE xlPicture -4147

/* Enum XlPivotTableSourceType   -                                            */
&GLOBAL-DEFINE xlConsolidation 3
&GLOBAL-DEFINE xlDatabase 1
&GLOBAL-DEFINE xlExternal 2
&GLOBAL-DEFINE xlPivotTable -4148

/* Enum XlReferenceStyle   -                                                  */
&GLOBAL-DEFINE xlA1 1
&GLOBAL-DEFINE xlR1C1 -4150

/* Enum XlMSApplication   -                                                   */
&GLOBAL-DEFINE xlMicrosoftAccess 4
&GLOBAL-DEFINE xlMicrosoftFoxPro 5
&GLOBAL-DEFINE xlMicrosoftMail 3
&GLOBAL-DEFINE xlMicrosoftPowerPoint 2
&GLOBAL-DEFINE xlMicrosoftProject 6
&GLOBAL-DEFINE xlMicrosoftSchedulePlus 7
&GLOBAL-DEFINE xlMicrosoftWord 1

/* Enum XlMouseButton   -                                                     */
&GLOBAL-DEFINE xlNoButton 0
&GLOBAL-DEFINE xlPrimaryButton 1
&GLOBAL-DEFINE xlSecondaryButton 2

/* Enum XlCutCopyMode   -                                                     */
&GLOBAL-DEFINE xlCopy 1
&GLOBAL-DEFINE xlCut 2

/* Enum XlFillWith   -                                                        */
&GLOBAL-DEFINE xlFillWithAll -4104
&GLOBAL-DEFINE xlFillWithContents 2
&GLOBAL-DEFINE xlFillWithFormats -4122

/* Enum XlFilterAction   -                                                    */
&GLOBAL-DEFINE xlFilterCopy 2
&GLOBAL-DEFINE xlFilterInPlace 1

/* Enum XlOrder   -                                                           */
&GLOBAL-DEFINE xlDownThenOver 1
&GLOBAL-DEFINE xlOverThenDown 2

/* Enum XlLinkType   -                                                        */
&GLOBAL-DEFINE xlLinkTypeExcelLinks 1
&GLOBAL-DEFINE xlLinkTypeOLELinks 2

/* Enum XlApplyNamesOrder   -                                                 */
&GLOBAL-DEFINE xlColumnThenRow 2
&GLOBAL-DEFINE xlRowThenColumn 1

/* Enum XlEnableCancelKey   -                                                 */
&GLOBAL-DEFINE xlDisabled 0
&GLOBAL-DEFINE xlErrorHandler 2
&GLOBAL-DEFINE xlInterrupt 1

/* Enum XlPageBreak   -                                                       */
&GLOBAL-DEFINE xlPageBreakAutomatic -4105
&GLOBAL-DEFINE xlPageBreakManual -4135
&GLOBAL-DEFINE xlPageBreakNone -4142

/* Enum XlOLEType   -                                                         */
&GLOBAL-DEFINE xlOLEControl 2
&GLOBAL-DEFINE xlOLEEmbed 1
&GLOBAL-DEFINE xlOLELink 0

/* Enum XlPageOrientation   -                                                 */
&GLOBAL-DEFINE xlLandscape 2
&GLOBAL-DEFINE xlPortrait 1

/* Enum XlLinkInfo   -                                                        */
&GLOBAL-DEFINE xlEditionDate 2
&GLOBAL-DEFINE xlUpdateState 1

/* Enum XlCommandUnderlines   -                                               */
&GLOBAL-DEFINE xlCommandUnderlinesAutomatic -4105
&GLOBAL-DEFINE xlCommandUnderlinesOff -4146
&GLOBAL-DEFINE xlCommandUnderlinesOn 1

/* Enum XlOLEVerb   -                                                         */
&GLOBAL-DEFINE xlVerbOpen 2
&GLOBAL-DEFINE xlVerbPrimary 1

/* Enum XlCalculation   -                                                     */
&GLOBAL-DEFINE xlCalculationAutomatic -4105
&GLOBAL-DEFINE xlCalculationManual -4135
&GLOBAL-DEFINE xlCalculationSemiautomatic 2

/* Enum XlFileAccess   -                                                      */
&GLOBAL-DEFINE xlReadOnly 3
&GLOBAL-DEFINE xlReadWrite 2

/* Enum XlEditionType   -                                                     */
&GLOBAL-DEFINE xlPublisher 1
&GLOBAL-DEFINE xlSubscriber 2

/* Enum XlObjectSize   -                                                      */
&GLOBAL-DEFINE xlFitToPage 2
&GLOBAL-DEFINE xlFullPage 3
&GLOBAL-DEFINE xlScreenSize 1

/* Enum XlLookAt   -                                                          */
&GLOBAL-DEFINE xlPart 2
&GLOBAL-DEFINE xlWhole 1

/* Enum XlMailSystem   -                                                      */
&GLOBAL-DEFINE xlMAPI 1
&GLOBAL-DEFINE xlNoMailSystem 0
&GLOBAL-DEFINE xlPowerTalk 2

/* Enum XlLinkInfoType   -                                                    */
&GLOBAL-DEFINE xlLinkInfoOLELinks 2
&GLOBAL-DEFINE xlLinkInfoPublishers 5
&GLOBAL-DEFINE xlLinkInfoSubscribers 6

/* Enum XlCVError   -                                                         */
&GLOBAL-DEFINE xlErrDiv0 2007
&GLOBAL-DEFINE xlErrNA 2042
&GLOBAL-DEFINE xlErrName 2029
&GLOBAL-DEFINE xlErrNull 2000
&GLOBAL-DEFINE xlErrNum 2036
&GLOBAL-DEFINE xlErrRef 2023
&GLOBAL-DEFINE xlErrValue 2015

/* Enum XlEditionFormat   -                                                   */
&GLOBAL-DEFINE xlBIFF 2
&GLOBAL-DEFINE xlPICT 1
&GLOBAL-DEFINE xlRTF 4
&GLOBAL-DEFINE xlVALU 8

/* Enum XlLink   -                                                            */
&GLOBAL-DEFINE xlExcelLinks 1
&GLOBAL-DEFINE xlOLELinks 2
&GLOBAL-DEFINE xlPublishers 5
&GLOBAL-DEFINE xlSubscribers 6

/* Enum XlCellType   -                                                        */
&GLOBAL-DEFINE xlCellTypeBlanks 4
&GLOBAL-DEFINE xlCellTypeConstants 2
&GLOBAL-DEFINE xlCellTypeFormulas -4123
&GLOBAL-DEFINE xlCellTypeLastCell 11
&GLOBAL-DEFINE xlCellTypeComments -4144
&GLOBAL-DEFINE xlCellTypeVisible 12
&GLOBAL-DEFINE xlCellTypeAllFormatConditions -4172
&GLOBAL-DEFINE xlCellTypeSameFormatConditions -4173
&GLOBAL-DEFINE xlCellTypeAllValidation -4174
&GLOBAL-DEFINE xlCellTypeSameValidation -4175

/* Enum XlArrangeStyle   -                                                    */
&GLOBAL-DEFINE xlArrangeStyleCascade 7
&GLOBAL-DEFINE xlArrangeStyleHorizontal -4128
&GLOBAL-DEFINE xlArrangeStyleTiled 1
&GLOBAL-DEFINE xlArrangeStyleVertical -4166

/* Enum XlMousePointer   -                                                    */
&GLOBAL-DEFINE xlIBeam 3
&GLOBAL-DEFINE xlDefault -4143
&GLOBAL-DEFINE xlNorthwestArrow 1
&GLOBAL-DEFINE xlWait 2

/* Enum XlEditionOptionsOption   -                                            */
&GLOBAL-DEFINE xlAutomaticUpdate 4
&GLOBAL-DEFINE xlCancel 1
&GLOBAL-DEFINE xlChangeAttributes 6
&GLOBAL-DEFINE xlManualUpdate 5
&GLOBAL-DEFINE xlOpenSource 3
&GLOBAL-DEFINE xlSelect 3
&GLOBAL-DEFINE xlSendPublisher 2
&GLOBAL-DEFINE xlUpdateSubscriber 2

/* Enum XlAutoFillType   -                                                    */
&GLOBAL-DEFINE xlFillCopy 1
&GLOBAL-DEFINE xlFillDays 5
&GLOBAL-DEFINE xlFillDefault 0
&GLOBAL-DEFINE xlFillFormats 3
&GLOBAL-DEFINE xlFillMonths 7
&GLOBAL-DEFINE xlFillSeries 2
&GLOBAL-DEFINE xlFillValues 4
&GLOBAL-DEFINE xlFillWeekdays 6
&GLOBAL-DEFINE xlFillYears 8
&GLOBAL-DEFINE xlGrowthTrend 10
&GLOBAL-DEFINE xlLinearTrend 9

/* Enum XlAutoFilterOperator   -                                              */
&GLOBAL-DEFINE xlAnd 1
&GLOBAL-DEFINE xlBottom10Items 4
&GLOBAL-DEFINE xlBottom10Percent 6
&GLOBAL-DEFINE xlOr 2
&GLOBAL-DEFINE xlTop10Items 3
&GLOBAL-DEFINE xlTop10Percent 5

/* Enum XlClipboardFormat   -                                                 */
&GLOBAL-DEFINE xlClipboardFormatBIFF 8
&GLOBAL-DEFINE xlClipboardFormatBIFF2 18
&GLOBAL-DEFINE xlClipboardFormatBIFF3 20
&GLOBAL-DEFINE xlClipboardFormatBIFF4 30
&GLOBAL-DEFINE xlClipboardFormatBinary 15
&GLOBAL-DEFINE xlClipboardFormatBitmap 9
&GLOBAL-DEFINE xlClipboardFormatCGM 13
&GLOBAL-DEFINE xlClipboardFormatCSV 5
&GLOBAL-DEFINE xlClipboardFormatDIF 4
&GLOBAL-DEFINE xlClipboardFormatDspText 12
&GLOBAL-DEFINE xlClipboardFormatEmbeddedObject 21
&GLOBAL-DEFINE xlClipboardFormatEmbedSource 22
&GLOBAL-DEFINE xlClipboardFormatLink 11
&GLOBAL-DEFINE xlClipboardFormatLinkSource 23
&GLOBAL-DEFINE xlClipboardFormatLinkSourceDesc 32
&GLOBAL-DEFINE xlClipboardFormatMovie 24
&GLOBAL-DEFINE xlClipboardFormatNative 14
&GLOBAL-DEFINE xlClipboardFormatObjectDesc 31
&GLOBAL-DEFINE xlClipboardFormatObjectLink 19
&GLOBAL-DEFINE xlClipboardFormatOwnerLink 17
&GLOBAL-DEFINE xlClipboardFormatPICT 2
&GLOBAL-DEFINE xlClipboardFormatPrintPICT 3
&GLOBAL-DEFINE xlClipboardFormatRTF 7
&GLOBAL-DEFINE xlClipboardFormatScreenPICT 29
&GLOBAL-DEFINE xlClipboardFormatStandardFont 28
&GLOBAL-DEFINE xlClipboardFormatStandardScale 27
&GLOBAL-DEFINE xlClipboardFormatSYLK 6
&GLOBAL-DEFINE xlClipboardFormatTable 16
&GLOBAL-DEFINE xlClipboardFormatText 0
&GLOBAL-DEFINE xlClipboardFormatToolFace 25
&GLOBAL-DEFINE xlClipboardFormatToolFacePICT 26
&GLOBAL-DEFINE xlClipboardFormatVALU 1
&GLOBAL-DEFINE xlClipboardFormatWK1 10

/* Enum XlFileFormat   -                                                      */
&GLOBAL-DEFINE xlAddIn 18
&GLOBAL-DEFINE xlCSV 6
&GLOBAL-DEFINE xlCSVMac 22
&GLOBAL-DEFINE xlCSVMSDOS 24
&GLOBAL-DEFINE xlCSVWindows 23
&GLOBAL-DEFINE xlDBF2 7
&GLOBAL-DEFINE xlDBF3 8
&GLOBAL-DEFINE xlDBF4 11
&GLOBAL-DEFINE xlDIF 9
&GLOBAL-DEFINE xlExcel2 16
&GLOBAL-DEFINE xlExcel2FarEast 27
&GLOBAL-DEFINE xlExcel3 29
&GLOBAL-DEFINE xlExcel4 33
&GLOBAL-DEFINE xlExcel5 39
&GLOBAL-DEFINE xlExcel7 39
&GLOBAL-DEFINE xlExcel9795 43
&GLOBAL-DEFINE xlExcel4Workbook 35
&GLOBAL-DEFINE xlIntlAddIn 26
&GLOBAL-DEFINE xlIntlMacro 25
&GLOBAL-DEFINE xlWorkbookNormal -4143
&GLOBAL-DEFINE xlSYLK 2
&GLOBAL-DEFINE xlTemplate 17
&GLOBAL-DEFINE xlCurrentPlatformText -4158
&GLOBAL-DEFINE xlTextMac 19
&GLOBAL-DEFINE xlTextMSDOS 21
&GLOBAL-DEFINE xlTextPrinter 36
&GLOBAL-DEFINE xlTextWindows 20
&GLOBAL-DEFINE xlWJ2WD1 14
&GLOBAL-DEFINE xlWK1 5
&GLOBAL-DEFINE xlWK1ALL 31
&GLOBAL-DEFINE xlWK1FMT 30
&GLOBAL-DEFINE xlWK3 15
&GLOBAL-DEFINE xlWK4 38
&GLOBAL-DEFINE xlWK3FM3 32
&GLOBAL-DEFINE xlWKS 4
&GLOBAL-DEFINE xlWorks2FarEast 28
&GLOBAL-DEFINE xlWQ1 34
&GLOBAL-DEFINE xlWJ3 40
&GLOBAL-DEFINE xlWJ3FJ3 41
&GLOBAL-DEFINE xlUnicodeText 42
&GLOBAL-DEFINE xlHtml 44

/* Enum XlApplicationInternational   -                                        */
&GLOBAL-DEFINE xl24HourClock 33
&GLOBAL-DEFINE xl4DigitYears 43
&GLOBAL-DEFINE xlAlternateArraySeparator 16
&GLOBAL-DEFINE xlColumnSeparator 14
&GLOBAL-DEFINE xlCountryCode 1
&GLOBAL-DEFINE xlCountrySetting 2
&GLOBAL-DEFINE xlCurrencyBefore 37
&GLOBAL-DEFINE xlCurrencyCode 25
&GLOBAL-DEFINE xlCurrencyDigits 27
&GLOBAL-DEFINE xlCurrencyLeadingZeros 40
&GLOBAL-DEFINE xlCurrencyMinusSign 38
&GLOBAL-DEFINE xlCurrencyNegative 28
&GLOBAL-DEFINE xlCurrencySpaceBefore 36
&GLOBAL-DEFINE xlCurrencyTrailingZeros 39
&GLOBAL-DEFINE xlDateOrder 32
&GLOBAL-DEFINE xlDateSeparator 17
&GLOBAL-DEFINE xlDayCode 21
&GLOBAL-DEFINE xlDayLeadingZero 42
&GLOBAL-DEFINE xlDecimalSeparator 3
&GLOBAL-DEFINE xlGeneralFormatName 26
&GLOBAL-DEFINE xlHourCode 22
&GLOBAL-DEFINE xlLeftBrace 12
&GLOBAL-DEFINE xlLeftBracket 10
&GLOBAL-DEFINE xlListSeparator 5
&GLOBAL-DEFINE xlLowerCaseColumnLetter 9
&GLOBAL-DEFINE xlLowerCaseRowLetter 8
&GLOBAL-DEFINE xlMDY 44
&GLOBAL-DEFINE xlMetric 35
&GLOBAL-DEFINE xlMinuteCode 23
&GLOBAL-DEFINE xlMonthCode 20
&GLOBAL-DEFINE xlMonthLeadingZero 41
&GLOBAL-DEFINE xlMonthNameChars 30
&GLOBAL-DEFINE xlNoncurrencyDigits 29
&GLOBAL-DEFINE xlNonEnglishFunctions 34
&GLOBAL-DEFINE xlRightBrace 13
&GLOBAL-DEFINE xlRightBracket 11
&GLOBAL-DEFINE xlRowSeparator 15
&GLOBAL-DEFINE xlSecondCode 24
&GLOBAL-DEFINE xlThousandsSeparator 4
&GLOBAL-DEFINE xlTimeLeadingZero 45
&GLOBAL-DEFINE xlTimeSeparator 18
&GLOBAL-DEFINE xlUpperCaseColumnLetter 7
&GLOBAL-DEFINE xlUpperCaseRowLetter 6
&GLOBAL-DEFINE xlWeekdayNameChars 31
&GLOBAL-DEFINE xlYearCode 19

/* Enum XlPageBreakExtent   -                                                 */
&GLOBAL-DEFINE xlPageBreakFull 1
&GLOBAL-DEFINE xlPageBreakPartial 2

/* Enum XlCellInsertionMode   -                                               */
&GLOBAL-DEFINE xlOverwriteCells 0
&GLOBAL-DEFINE xlInsertDeleteCells 1
&GLOBAL-DEFINE xlInsertEntireRows 2

/* Enum XlFormulaLabel   -                                                    */
&GLOBAL-DEFINE xlNoLabels -4142
&GLOBAL-DEFINE xlRowLabels 1
&GLOBAL-DEFINE xlColumnLabels 2
&GLOBAL-DEFINE xlMixedLabels 3

/* Enum XlHighlightChangesTime   -                                            */
&GLOBAL-DEFINE xlSinceMyLastSave 1
&GLOBAL-DEFINE xlAllChanges 2
&GLOBAL-DEFINE xlNotYetReviewed 3

/* Enum XlCommentDisplayMode   -                                              */
&GLOBAL-DEFINE xlNoIndicator 0
&GLOBAL-DEFINE xlCommentIndicatorOnly -1
&GLOBAL-DEFINE xlCommentAndIndicator 1

/* Enum XlFormatConditionType   -                                             */
&GLOBAL-DEFINE xlCellValue 1
&GLOBAL-DEFINE xlExpression 2

/* Enum XlFormatConditionOperator   -                                         */
&GLOBAL-DEFINE xlBetween 1
&GLOBAL-DEFINE xlNotBetween 2
&GLOBAL-DEFINE xlEqual 3
&GLOBAL-DEFINE xlNotEqual 4
&GLOBAL-DEFINE xlGreater 5
&GLOBAL-DEFINE xlLess 6
&GLOBAL-DEFINE xlGreaterEqual 7
&GLOBAL-DEFINE xlLessEqual 8

/* Enum XlEnableSelection   -                                                 */
&GLOBAL-DEFINE xlNoRestrictions 0
&GLOBAL-DEFINE xlUnlockedCells 1
&GLOBAL-DEFINE xlNoSelection -4142

/* Enum XlDVType   -                                                          */
&GLOBAL-DEFINE xlValidateInputOnly 0
&GLOBAL-DEFINE xlValidateWholeNumber 1
&GLOBAL-DEFINE xlValidateDecimal 2
&GLOBAL-DEFINE xlValidateList 3
&GLOBAL-DEFINE xlValidateDate 4
&GLOBAL-DEFINE xlValidateTime 5
&GLOBAL-DEFINE xlValidateTextLength 6
&GLOBAL-DEFINE xlValidateCustom 7

/* Enum XlIMEMode   -                                                         */
&GLOBAL-DEFINE xlIMEModeNoControl 0
&GLOBAL-DEFINE xlIMEModeOn 1
&GLOBAL-DEFINE xlIMEModeOff 2
&GLOBAL-DEFINE xlIMEModeDisable 3
&GLOBAL-DEFINE xlIMEModeHiragana 4
&GLOBAL-DEFINE xlIMEModeKatakana 5
&GLOBAL-DEFINE xlIMEModeKatakanaHalf 6
&GLOBAL-DEFINE xlIMEModeAlphaFull 7
&GLOBAL-DEFINE xlIMEModeAlpha 8
&GLOBAL-DEFINE xlIMEModeHangulFull 9
&GLOBAL-DEFINE xlIMEModeHangul 10

/* Enum XlDVAlertStyle   -                                                    */
&GLOBAL-DEFINE xlValidAlertStop 1
&GLOBAL-DEFINE xlValidAlertWarning 2
&GLOBAL-DEFINE xlValidAlertInformation 3

/* Enum XlChartLocation   -                                                   */
&GLOBAL-DEFINE xlLocationAsNewSheet 1
&GLOBAL-DEFINE xlLocationAsObject 2
&GLOBAL-DEFINE xlLocationAutomatic 3

/* Enum XlPaperSize   -                                                       */
&GLOBAL-DEFINE xlPaper10x14 16
&GLOBAL-DEFINE xlPaper11x17 17
&GLOBAL-DEFINE xlPaperA3 8
&GLOBAL-DEFINE xlPaperA4 9
&GLOBAL-DEFINE xlPaperA4Small 10
&GLOBAL-DEFINE xlPaperA5 11
&GLOBAL-DEFINE xlPaperB4 12
&GLOBAL-DEFINE xlPaperB5 13
&GLOBAL-DEFINE xlPaperCsheet 24
&GLOBAL-DEFINE xlPaperDsheet 25
&GLOBAL-DEFINE xlPaperEnvelope10 20
&GLOBAL-DEFINE xlPaperEnvelope11 21
&GLOBAL-DEFINE xlPaperEnvelope12 22
&GLOBAL-DEFINE xlPaperEnvelope14 23
&GLOBAL-DEFINE xlPaperEnvelope9 19
&GLOBAL-DEFINE xlPaperEnvelopeB4 33
&GLOBAL-DEFINE xlPaperEnvelopeB5 34
&GLOBAL-DEFINE xlPaperEnvelopeB6 35
&GLOBAL-DEFINE xlPaperEnvelopeC3 29
&GLOBAL-DEFINE xlPaperEnvelopeC4 30
&GLOBAL-DEFINE xlPaperEnvelopeC5 28
&GLOBAL-DEFINE xlPaperEnvelopeC6 31
&GLOBAL-DEFINE xlPaperEnvelopeC65 32
&GLOBAL-DEFINE xlPaperEnvelopeDL 27
&GLOBAL-DEFINE xlPaperEnvelopeItaly 36
&GLOBAL-DEFINE xlPaperEnvelopeMonarch 37
&GLOBAL-DEFINE xlPaperEnvelopePersonal 38
&GLOBAL-DEFINE xlPaperEsheet 26
&GLOBAL-DEFINE xlPaperExecutive 7
&GLOBAL-DEFINE xlPaperFanfoldLegalGerman 41
&GLOBAL-DEFINE xlPaperFanfoldStdGerman 40
&GLOBAL-DEFINE xlPaperFanfoldUS 39
&GLOBAL-DEFINE xlPaperFolio 14
&GLOBAL-DEFINE xlPaperLedger 4
&GLOBAL-DEFINE xlPaperLegal 5
&GLOBAL-DEFINE xlPaperLetter 1
&GLOBAL-DEFINE xlPaperLetterSmall 2
&GLOBAL-DEFINE xlPaperNote 18
&GLOBAL-DEFINE xlPaperQuarto 15
&GLOBAL-DEFINE xlPaperStatement 6
&GLOBAL-DEFINE xlPaperTabloid 3
&GLOBAL-DEFINE xlPaperUser 256

/* Enum XlPasteSpecialOperation   -                                           */
&GLOBAL-DEFINE xlPasteSpecialOperationAdd 2
&GLOBAL-DEFINE xlPasteSpecialOperationDivide 5
&GLOBAL-DEFINE xlPasteSpecialOperationMultiply 4
&GLOBAL-DEFINE xlPasteSpecialOperationNone -4142
&GLOBAL-DEFINE xlPasteSpecialOperationSubtract 3

/* Enum XlPasteType   -                                                       */
&GLOBAL-DEFINE xlPasteAll -4104
&GLOBAL-DEFINE xlPasteAllExceptBorders 7
&GLOBAL-DEFINE xlPasteFormats -4122
&GLOBAL-DEFINE xlPasteFormulas -4123
&GLOBAL-DEFINE xlPasteComments -4144
&GLOBAL-DEFINE xlPasteValues -4163

/* Enum XlPhoneticCharacterType   -                                           */
&GLOBAL-DEFINE xlKatakanaHalf 0
&GLOBAL-DEFINE xlKatakana 1
&GLOBAL-DEFINE xlHiragana 2
&GLOBAL-DEFINE xlNoConversion 3

/* Enum XlPhoneticAlignment   -                                               */
&GLOBAL-DEFINE xlPhoneticAlignNoControl 0
&GLOBAL-DEFINE xlPhoneticAlignLeft 1
&GLOBAL-DEFINE xlPhoneticAlignCenter 2
&GLOBAL-DEFINE xlPhoneticAlignDistributed 3

/* Enum XlPictureAppearance   -                                               */
&GLOBAL-DEFINE xlPrinter 2
&GLOBAL-DEFINE xlScreen 1

/* Enum XlPivotFieldOrientation   -                                           */
&GLOBAL-DEFINE xlColumnField 2
&GLOBAL-DEFINE xlDataField 4
&GLOBAL-DEFINE xlHidden 0
&GLOBAL-DEFINE xlPageField 3
&GLOBAL-DEFINE xlRowField 1

/* Enum XlPivotFieldCalculation   -                                           */
&GLOBAL-DEFINE xlDifferenceFrom 2
&GLOBAL-DEFINE xlIndex 9
&GLOBAL-DEFINE xlNoAdditionalCalculation -4143
&GLOBAL-DEFINE xlPercentDifferenceFrom 4
&GLOBAL-DEFINE xlPercentOf 3
&GLOBAL-DEFINE xlPercentOfColumn 7
&GLOBAL-DEFINE xlPercentOfRow 6
&GLOBAL-DEFINE xlPercentOfTotal 8
&GLOBAL-DEFINE xlRunningTotal 5

/* Enum XlPlacement   -                                                       */
&GLOBAL-DEFINE xlFreeFloating 3
&GLOBAL-DEFINE xlMove 2
&GLOBAL-DEFINE xlMoveAndSize 1

/* Enum XlPlatform   -                                                        */
&GLOBAL-DEFINE xlMacintosh 1
&GLOBAL-DEFINE xlMSDOS 3
&GLOBAL-DEFINE xlWindows 2

/* Enum XlPrintLocation   -                                                   */
&GLOBAL-DEFINE xlPrintSheetEnd 1
&GLOBAL-DEFINE xlPrintInPlace 16
&GLOBAL-DEFINE xlPrintNoComments -4142

/* Enum XlPriority   -                                                        */
&GLOBAL-DEFINE xlPriorityHigh -4127
&GLOBAL-DEFINE xlPriorityLow -4134
&GLOBAL-DEFINE xlPriorityNormal -4143

/* Enum XlPTSelectionMode   -                                                 */
&GLOBAL-DEFINE xlLabelOnly 1
&GLOBAL-DEFINE xlDataAndLabel 0
&GLOBAL-DEFINE xlDataOnly 2
&GLOBAL-DEFINE xlOrigin 3
&GLOBAL-DEFINE xlButton 15
&GLOBAL-DEFINE xlBlanks 4
&GLOBAL-DEFINE xlFirstRow 256

/* Enum XlRangeAutoFormat   -                                                 */
&GLOBAL-DEFINE xlRangeAutoFormat3DEffects1 13
&GLOBAL-DEFINE xlRangeAutoFormat3DEffects2 14
&GLOBAL-DEFINE xlRangeAutoFormatAccounting1 4
&GLOBAL-DEFINE xlRangeAutoFormatAccounting2 5
&GLOBAL-DEFINE xlRangeAutoFormatAccounting3 6
&GLOBAL-DEFINE xlRangeAutoFormatAccounting4 17
&GLOBAL-DEFINE xlRangeAutoFormatClassic1 1
&GLOBAL-DEFINE xlRangeAutoFormatClassic2 2
&GLOBAL-DEFINE xlRangeAutoFormatClassic3 3
&GLOBAL-DEFINE xlRangeAutoFormatColor1 7
&GLOBAL-DEFINE xlRangeAutoFormatColor2 8
&GLOBAL-DEFINE xlRangeAutoFormatColor3 9
&GLOBAL-DEFINE xlRangeAutoFormatList1 10
&GLOBAL-DEFINE xlRangeAutoFormatList2 11
&GLOBAL-DEFINE xlRangeAutoFormatList3 12
&GLOBAL-DEFINE xlRangeAutoFormatLocalFormat1 15
&GLOBAL-DEFINE xlRangeAutoFormatLocalFormat2 16
&GLOBAL-DEFINE xlRangeAutoFormatLocalFormat3 19
&GLOBAL-DEFINE xlRangeAutoFormatLocalFormat4 20
&GLOBAL-DEFINE xlRangeAutoFormatReport1 21
&GLOBAL-DEFINE xlRangeAutoFormatReport2 22
&GLOBAL-DEFINE xlRangeAutoFormatReport3 23
&GLOBAL-DEFINE xlRangeAutoFormatReport4 24
&GLOBAL-DEFINE xlRangeAutoFormatReport5 25
&GLOBAL-DEFINE xlRangeAutoFormatReport6 26
&GLOBAL-DEFINE xlRangeAutoFormatReport7 27
&GLOBAL-DEFINE xlRangeAutoFormatReport8 28
&GLOBAL-DEFINE xlRangeAutoFormatReport9 29
&GLOBAL-DEFINE xlRangeAutoFormatReport10 30
&GLOBAL-DEFINE xlRangeAutoFormatClassicPivotTable 31
&GLOBAL-DEFINE xlRangeAutoFormatTable1 32
&GLOBAL-DEFINE xlRangeAutoFormatTable2 33
&GLOBAL-DEFINE xlRangeAutoFormatTable3 34
&GLOBAL-DEFINE xlRangeAutoFormatTable4 35
&GLOBAL-DEFINE xlRangeAutoFormatTable5 36
&GLOBAL-DEFINE xlRangeAutoFormatTable6 37
&GLOBAL-DEFINE xlRangeAutoFormatTable7 38
&GLOBAL-DEFINE xlRangeAutoFormatTable8 39
&GLOBAL-DEFINE xlRangeAutoFormatTable9 40
&GLOBAL-DEFINE xlRangeAutoFormatTable10 41
&GLOBAL-DEFINE xlRangeAutoFormatPTNone 42
&GLOBAL-DEFINE xlRangeAutoFormatNone -4142
&GLOBAL-DEFINE xlRangeAutoFormatSimple -4154

/* Enum XlReferenceType   -                                                   */
&GLOBAL-DEFINE xlAbsolute 1
&GLOBAL-DEFINE xlAbsRowRelColumn 2
&GLOBAL-DEFINE xlRelative 4
&GLOBAL-DEFINE xlRelRowAbsColumn 3

/* Enum XlLayoutFormType   -                                                  */
&GLOBAL-DEFINE xlTabular 0
&GLOBAL-DEFINE xlOutline 1

/* Enum XlRoutingSlipDelivery   -                                             */
&GLOBAL-DEFINE xlAllAtOnce 2
&GLOBAL-DEFINE xlOneAfterAnother 1

/* Enum XlRoutingSlipStatus   -                                               */
&GLOBAL-DEFINE xlNotYetRouted 0
&GLOBAL-DEFINE xlRoutingComplete 2
&GLOBAL-DEFINE xlRoutingInProgress 1

/* Enum XlRunAutoMacro   -                                                    */
&GLOBAL-DEFINE xlAutoActivate 3
&GLOBAL-DEFINE xlAutoClose 2
&GLOBAL-DEFINE xlAutoDeactivate 4
&GLOBAL-DEFINE xlAutoOpen 1

/* Enum XlSaveAction   -                                                      */
&GLOBAL-DEFINE xlDoNotSaveChanges 2
&GLOBAL-DEFINE xlSaveChanges 1

/* Enum XlSaveAsAccessMode   -                                                */
&GLOBAL-DEFINE xlExclusive 3
&GLOBAL-DEFINE xlNoChange 1
&GLOBAL-DEFINE xlShared 2

/* Enum XlSaveConflictResolution   -                                          */
&GLOBAL-DEFINE xlLocalSessionChanges 2
&GLOBAL-DEFINE xlOtherSessionChanges 3
&GLOBAL-DEFINE xlUserResolution 1

/* Enum XlSearchDirection   -                                                 */
&GLOBAL-DEFINE xlNext 1
&GLOBAL-DEFINE xlPrevious 2

/* Enum XlSearchOrder   -                                                     */
&GLOBAL-DEFINE xlByColumns 2
&GLOBAL-DEFINE xlByRows 1

/* Enum XlSheetVisibility   -                                                 */
&GLOBAL-DEFINE xlSheetVisible -1
&GLOBAL-DEFINE xlSheetHidden 0
&GLOBAL-DEFINE xlSheetVeryHidden 2

/* Enum XlSortMethod   -                                                      */
&GLOBAL-DEFINE xlPinYin 1
&GLOBAL-DEFINE xlStroke 2

/* Enum XlSortMethodOld   -                                                   */
&GLOBAL-DEFINE xlCodePage 2
&GLOBAL-DEFINE xlSyllabary 1

/* Enum XlSortOrder   -                                                       */
&GLOBAL-DEFINE xlAscending 1
&GLOBAL-DEFINE xlDescending 2

/* Enum XlSortOrientation   -                                                 */
&GLOBAL-DEFINE xlSortRows 2
&GLOBAL-DEFINE xlSortColumns 1

/* Enum XlSortType   -                                                        */
&GLOBAL-DEFINE xlSortLabels 2
&GLOBAL-DEFINE xlSortValues 1

/* Enum XlSpecialCellsValue   -                                               */
&GLOBAL-DEFINE xlErrors 16
&GLOBAL-DEFINE xlLogical 4
&GLOBAL-DEFINE xlNumbers 1
&GLOBAL-DEFINE xlTextValues 2

/* Enum XlSubscribeToFormat   -                                               */
&GLOBAL-DEFINE xlSubscribeToPicture -4147
&GLOBAL-DEFINE xlSubscribeToText -4158

/* Enum XlSummaryRow   -                                                      */
&GLOBAL-DEFINE xlSummaryAbove 0
&GLOBAL-DEFINE xlSummaryBelow 1

/* Enum XlSummaryColumn   -                                                   */
&GLOBAL-DEFINE xlSummaryOnLeft -4131
&GLOBAL-DEFINE xlSummaryOnRight -4152

/* Enum XlSummaryReportType   -                                               */
&GLOBAL-DEFINE xlSummaryPivotTable -4148
&GLOBAL-DEFINE xlStandardSummary 1

/* Enum XlTabPosition   -                                                     */
&GLOBAL-DEFINE xlTabPositionFirst 0
&GLOBAL-DEFINE xlTabPositionLast 1

/* Enum XlTextParsingType   -                                                 */
&GLOBAL-DEFINE xlDelimited 1
&GLOBAL-DEFINE xlFixedWidth 2

/* Enum XlTextQualifier   -                                                   */
&GLOBAL-DEFINE xlTextQualifierDoubleQuote 1
&GLOBAL-DEFINE xlTextQualifierNone -4142
&GLOBAL-DEFINE xlTextQualifierSingleQuote 2

/* Enum XlWBATemplate   -                                                     */
&GLOBAL-DEFINE xlWBATChart -4109
&GLOBAL-DEFINE xlWBATExcel4IntlMacroSheet 4
&GLOBAL-DEFINE xlWBATExcel4MacroSheet 3
&GLOBAL-DEFINE xlWBATWorksheet -4167

/* Enum XlWindowView   -                                                      */
&GLOBAL-DEFINE xlNormalView 1
&GLOBAL-DEFINE xlPageBreakPreview 2

/* Enum XlXLMMacroType   -                                                    */
&GLOBAL-DEFINE xlCommand 2
&GLOBAL-DEFINE xlFunction 1
&GLOBAL-DEFINE xlNotXLM 3

/* Enum XlYesNoGuess   -                                                      */
&GLOBAL-DEFINE xlGuess 0
&GLOBAL-DEFINE xlNo 2
&GLOBAL-DEFINE xlYes 1

/* Enum XlBordersIndex   -                                                    */
&GLOBAL-DEFINE xlInsideHorizontal 12
&GLOBAL-DEFINE xlInsideVertical 11
&GLOBAL-DEFINE xlDiagonalDown 5
&GLOBAL-DEFINE xlDiagonalUp 6
&GLOBAL-DEFINE xlEdgeBottom 9
&GLOBAL-DEFINE xlEdgeLeft 7
&GLOBAL-DEFINE xlEdgeRight 10
&GLOBAL-DEFINE xlEdgeTop 8

/* Enum XlToolbarProtection   -                                               */
&GLOBAL-DEFINE xlNoButtonChanges 1
&GLOBAL-DEFINE xlNoChanges 4
&GLOBAL-DEFINE xlNoDockingChanges 3
&GLOBAL-DEFINE xlToolbarProtectionNone -4143
&GLOBAL-DEFINE xlNoShapeChanges 2

/* Enum XlBuiltInDialog   -                                                   */
&GLOBAL-DEFINE xlDialogOpen 1
&GLOBAL-DEFINE xlDialogOpenLinks 2
&GLOBAL-DEFINE xlDialogSaveAs 5
&GLOBAL-DEFINE xlDialogFileDelete 6
&GLOBAL-DEFINE xlDialogPageSetup 7
&GLOBAL-DEFINE xlDialogPrint 8
&GLOBAL-DEFINE xlDialogPrinterSetup 9
&GLOBAL-DEFINE xlDialogArrangeAll 12
&GLOBAL-DEFINE xlDialogWindowSize 13
&GLOBAL-DEFINE xlDialogWindowMove 14
&GLOBAL-DEFINE xlDialogRun 17
&GLOBAL-DEFINE xlDialogSetPrintTitles 23
&GLOBAL-DEFINE xlDialogFont 26
&GLOBAL-DEFINE xlDialogDisplay 27
&GLOBAL-DEFINE xlDialogProtectDocument 28
&GLOBAL-DEFINE xlDialogCalculation 32
&GLOBAL-DEFINE xlDialogExtract 35
&GLOBAL-DEFINE xlDialogDataDelete 36
&GLOBAL-DEFINE xlDialogSort 39
&GLOBAL-DEFINE xlDialogDataSeries 40
&GLOBAL-DEFINE xlDialogTable 41
&GLOBAL-DEFINE xlDialogFormatNumber 42
&GLOBAL-DEFINE xlDialogAlignment 43
&GLOBAL-DEFINE xlDialogStyle 44
&GLOBAL-DEFINE xlDialogBorder 45
&GLOBAL-DEFINE xlDialogCellProtection 46
&GLOBAL-DEFINE xlDialogColumnWidth 47
&GLOBAL-DEFINE xlDialogClear 52
&GLOBAL-DEFINE xlDialogPasteSpecial 53
&GLOBAL-DEFINE xlDialogEditDelete 54
&GLOBAL-DEFINE xlDialogInsert 55
&GLOBAL-DEFINE xlDialogPasteNames 58
&GLOBAL-DEFINE xlDialogDefineName 61
&GLOBAL-DEFINE xlDialogCreateNames 62
&GLOBAL-DEFINE xlDialogFormulaGoto 63
&GLOBAL-DEFINE xlDialogFormulaFind 64
&GLOBAL-DEFINE xlDialogGalleryArea 67
&GLOBAL-DEFINE xlDialogGalleryBar 68
&GLOBAL-DEFINE xlDialogGalleryColumn 69
&GLOBAL-DEFINE xlDialogGalleryLine 70
&GLOBAL-DEFINE xlDialogGalleryPie 71
&GLOBAL-DEFINE xlDialogGalleryScatter 72
&GLOBAL-DEFINE xlDialogCombination 73
&GLOBAL-DEFINE xlDialogGridlines 76
&GLOBAL-DEFINE xlDialogAxes 78
&GLOBAL-DEFINE xlDialogAttachText 80
&GLOBAL-DEFINE xlDialogPatterns 84
&GLOBAL-DEFINE xlDialogMainChart 85
&GLOBAL-DEFINE xlDialogOverlay 86
&GLOBAL-DEFINE xlDialogScale 87
&GLOBAL-DEFINE xlDialogFormatLegend 88
&GLOBAL-DEFINE xlDialogFormatText 89
&GLOBAL-DEFINE xlDialogParse 91
&GLOBAL-DEFINE xlDialogUnhide 94
&GLOBAL-DEFINE xlDialogWorkspace 95
&GLOBAL-DEFINE xlDialogActivate 103
&GLOBAL-DEFINE xlDialogCopyPicture 108
&GLOBAL-DEFINE xlDialogDeleteName 110
&GLOBAL-DEFINE xlDialogDeleteFormat 111
&GLOBAL-DEFINE xlDialogNew 119
&GLOBAL-DEFINE xlDialogRowHeight 127
&GLOBAL-DEFINE xlDialogFormatMove 128
&GLOBAL-DEFINE xlDialogFormatSize 129
&GLOBAL-DEFINE xlDialogFormulaReplace 130
&GLOBAL-DEFINE xlDialogSelectSpecial 132
&GLOBAL-DEFINE xlDialogApplyNames 133
&GLOBAL-DEFINE xlDialogReplaceFont 134
&GLOBAL-DEFINE xlDialogSplit 137
&GLOBAL-DEFINE xlDialogOutline 142
&GLOBAL-DEFINE xlDialogSaveWorkbook 145
&GLOBAL-DEFINE xlDialogCopyChart 147
&GLOBAL-DEFINE xlDialogFormatFont 150
&GLOBAL-DEFINE xlDialogNote 154
&GLOBAL-DEFINE xlDialogSetUpdateStatus 159
&GLOBAL-DEFINE xlDialogColorPalette 161
&GLOBAL-DEFINE xlDialogChangeLink 166
&GLOBAL-DEFINE xlDialogAppMove 170
&GLOBAL-DEFINE xlDialogAppSize 171
&GLOBAL-DEFINE xlDialogMainChartType 185
&GLOBAL-DEFINE xlDialogOverlayChartType 186
&GLOBAL-DEFINE xlDialogOpenMail 188
&GLOBAL-DEFINE xlDialogSendMail 189
&GLOBAL-DEFINE xlDialogStandardFont 190
&GLOBAL-DEFINE xlDialogConsolidate 191
&GLOBAL-DEFINE xlDialogSortSpecial 192
&GLOBAL-DEFINE xlDialogGallery3dArea 193
&GLOBAL-DEFINE xlDialogGallery3dColumn 194
&GLOBAL-DEFINE xlDialogGallery3dLine 195
&GLOBAL-DEFINE xlDialogGallery3dPie 196
&GLOBAL-DEFINE xlDialogView3d 197
&GLOBAL-DEFINE xlDialogGoalSeek 198
&GLOBAL-DEFINE xlDialogWorkgroup 199
&GLOBAL-DEFINE xlDialogFillGroup 200
&GLOBAL-DEFINE xlDialogUpdateLink 201
&GLOBAL-DEFINE xlDialogPromote 202
&GLOBAL-DEFINE xlDialogDemote 203
&GLOBAL-DEFINE xlDialogShowDetail 204
&GLOBAL-DEFINE xlDialogObjectProperties 207
&GLOBAL-DEFINE xlDialogSaveNewObject 208
&GLOBAL-DEFINE xlDialogApplyStyle 212
&GLOBAL-DEFINE xlDialogAssignToObject 213
&GLOBAL-DEFINE xlDialogObjectProtection 214
&GLOBAL-DEFINE xlDialogCreatePublisher 217
&GLOBAL-DEFINE xlDialogSubscribeTo 218
&GLOBAL-DEFINE xlDialogShowToolbar 220
&GLOBAL-DEFINE xlDialogPrintPreview 222
&GLOBAL-DEFINE xlDialogEditColor 223
&GLOBAL-DEFINE xlDialogFormatMain 225
&GLOBAL-DEFINE xlDialogFormatOverlay 226
&GLOBAL-DEFINE xlDialogEditSeries 228
&GLOBAL-DEFINE xlDialogDefineStyle 229
&GLOBAL-DEFINE xlDialogGalleryRadar 249
&GLOBAL-DEFINE xlDialogEditionOptions 251
&GLOBAL-DEFINE xlDialogZoom 256
&GLOBAL-DEFINE xlDialogInsertObject 259
&GLOBAL-DEFINE xlDialogSize 261
&GLOBAL-DEFINE xlDialogMove 262
&GLOBAL-DEFINE xlDialogFormatAuto 269
&GLOBAL-DEFINE xlDialogGallery3dBar 272
&GLOBAL-DEFINE xlDialogGallery3dSurface 273
&GLOBAL-DEFINE xlDialogCustomizeToolbar 276
&GLOBAL-DEFINE xlDialogWorkbookAdd 281
&GLOBAL-DEFINE xlDialogWorkbookMove 282
&GLOBAL-DEFINE xlDialogWorkbookCopy 283
&GLOBAL-DEFINE xlDialogWorkbookOptions 284
&GLOBAL-DEFINE xlDialogSaveWorkspace 285
&GLOBAL-DEFINE xlDialogChartWizard 288
&GLOBAL-DEFINE xlDialogAssignToTool 293
&GLOBAL-DEFINE xlDialogPlacement 300
&GLOBAL-DEFINE xlDialogFillWorkgroup 301
&GLOBAL-DEFINE xlDialogWorkbookNew 302
&GLOBAL-DEFINE xlDialogScenarioCells 305
&GLOBAL-DEFINE xlDialogScenarioAdd 307
&GLOBAL-DEFINE xlDialogScenarioEdit 308
&GLOBAL-DEFINE xlDialogScenarioSummary 311
&GLOBAL-DEFINE xlDialogPivotTableWizard 312
&GLOBAL-DEFINE xlDialogPivotFieldProperties 313
&GLOBAL-DEFINE xlDialogOptionsCalculation 318
&GLOBAL-DEFINE xlDialogOptionsEdit 319
&GLOBAL-DEFINE xlDialogOptionsView 320
&GLOBAL-DEFINE xlDialogAddinManager 321
&GLOBAL-DEFINE xlDialogMenuEditor 322
&GLOBAL-DEFINE xlDialogAttachToolbars 323
&GLOBAL-DEFINE xlDialogOptionsChart 325
&GLOBAL-DEFINE xlDialogVbaInsertFile 328
&GLOBAL-DEFINE xlDialogVbaProcedureDefinition 330
&GLOBAL-DEFINE xlDialogRoutingSlip 336
&GLOBAL-DEFINE xlDialogMailLogon 339
&GLOBAL-DEFINE xlDialogInsertPicture 342
&GLOBAL-DEFINE xlDialogGalleryDoughnut 344
&GLOBAL-DEFINE xlDialogChartTrend 350
&GLOBAL-DEFINE xlDialogWorkbookInsert 354
&GLOBAL-DEFINE xlDialogOptionsTransition 355
&GLOBAL-DEFINE xlDialogOptionsGeneral 356
&GLOBAL-DEFINE xlDialogFilterAdvanced 370
&GLOBAL-DEFINE xlDialogMailNextLetter 378
&GLOBAL-DEFINE xlDialogDataLabel 379
&GLOBAL-DEFINE xlDialogInsertTitle 380
&GLOBAL-DEFINE xlDialogFontProperties 381
&GLOBAL-DEFINE xlDialogMacroOptions 382
&GLOBAL-DEFINE xlDialogWorkbookUnhide 384
&GLOBAL-DEFINE xlDialogWorkbookName 386
&GLOBAL-DEFINE xlDialogGalleryCustom 388
&GLOBAL-DEFINE xlDialogAddChartAutoformat 390
&GLOBAL-DEFINE xlDialogChartAddData 392
&GLOBAL-DEFINE xlDialogTabOrder 394
&GLOBAL-DEFINE xlDialogSubtotalCreate 398
&GLOBAL-DEFINE xlDialogWorkbookTabSplit 415
&GLOBAL-DEFINE xlDialogWorkbookProtect 417
&GLOBAL-DEFINE xlDialogScrollbarProperties 420
&GLOBAL-DEFINE xlDialogPivotShowPages 421
&GLOBAL-DEFINE xlDialogTextToColumns 422
&GLOBAL-DEFINE xlDialogFormatCharttype 423
&GLOBAL-DEFINE xlDialogPivotFieldGroup 433
&GLOBAL-DEFINE xlDialogPivotFieldUngroup 434
&GLOBAL-DEFINE xlDialogCheckboxProperties 435
&GLOBAL-DEFINE xlDialogLabelProperties 436
&GLOBAL-DEFINE xlDialogListboxProperties 437
&GLOBAL-DEFINE xlDialogEditboxProperties 438
&GLOBAL-DEFINE xlDialogOpenText 441
&GLOBAL-DEFINE xlDialogPushbuttonProperties 445
&GLOBAL-DEFINE xlDialogFilter 447
&GLOBAL-DEFINE xlDialogFunctionWizard 450
&GLOBAL-DEFINE xlDialogSaveCopyAs 456
&GLOBAL-DEFINE xlDialogOptionsListsAdd 458
&GLOBAL-DEFINE xlDialogSeriesAxes 460
&GLOBAL-DEFINE xlDialogSeriesX 461
&GLOBAL-DEFINE xlDialogSeriesY 462
&GLOBAL-DEFINE xlDialogErrorbarX 463
&GLOBAL-DEFINE xlDialogErrorbarY 464
&GLOBAL-DEFINE xlDialogFormatChart 465
&GLOBAL-DEFINE xlDialogSeriesOrder 466
&GLOBAL-DEFINE xlDialogMailEditMailer 470
&GLOBAL-DEFINE xlDialogStandardWidth 472
&GLOBAL-DEFINE xlDialogScenarioMerge 473
&GLOBAL-DEFINE xlDialogProperties 474
&GLOBAL-DEFINE xlDialogSummaryInfo 474
&GLOBAL-DEFINE xlDialogFindFile 475
&GLOBAL-DEFINE xlDialogActiveCellFont 476
&GLOBAL-DEFINE xlDialogVbaMakeAddin 478
&GLOBAL-DEFINE xlDialogFileSharing 481
&GLOBAL-DEFINE xlDialogAutoCorrect 485
&GLOBAL-DEFINE xlDialogCustomViews 493
&GLOBAL-DEFINE xlDialogInsertNameLabel 496
&GLOBAL-DEFINE xlDialogSeriesShape 504
&GLOBAL-DEFINE xlDialogChartOptionsDataLabels 505
&GLOBAL-DEFINE xlDialogChartOptionsDataTable 506
&GLOBAL-DEFINE xlDialogSetBackgroundPicture 509
&GLOBAL-DEFINE xlDialogDataValidation 525
&GLOBAL-DEFINE xlDialogChartType 526
&GLOBAL-DEFINE xlDialogChartLocation 527
/* &GLOBAL-DEFINE _xlDialogPhonetic 538 */
&GLOBAL-DEFINE xlDialogChartSourceData 540
/* &GLOBAL-DEFINE _xlDialogChartSourceData 541 */
&GLOBAL-DEFINE xlDialogSeriesOptions 557
&GLOBAL-DEFINE xlDialogPivotTableOptions 567
&GLOBAL-DEFINE xlDialogPivotSolveOrder 568
&GLOBAL-DEFINE xlDialogPivotCalculatedField 570
&GLOBAL-DEFINE xlDialogPivotCalculatedItem 572
&GLOBAL-DEFINE xlDialogConditionalFormatting 583
&GLOBAL-DEFINE xlDialogInsertHyperlink 596
&GLOBAL-DEFINE xlDialogProtectSharing 620
&GLOBAL-DEFINE xlDialogOptionsME 647
&GLOBAL-DEFINE xlDialogPublishAsWebPage 653
&GLOBAL-DEFINE xlDialogPhonetic 656
&GLOBAL-DEFINE xlDialogNewWebQuery 667
&GLOBAL-DEFINE xlDialogImportTextFile 666
&GLOBAL-DEFINE xlDialogExternalDataProperties 530
&GLOBAL-DEFINE xlDialogWebOptionsGeneral 683
&GLOBAL-DEFINE xlDialogWebOptionsFiles 684
&GLOBAL-DEFINE xlDialogWebOptionsPictures 685
&GLOBAL-DEFINE xlDialogWebOptionsEncoding 686
&GLOBAL-DEFINE xlDialogWebOptionsFonts 687
&GLOBAL-DEFINE xlDialogPivotClientServerSet 689

/* Enum XlParameterType   -                                                   */
&GLOBAL-DEFINE xlPrompt 0
&GLOBAL-DEFINE xlConstant 1
&GLOBAL-DEFINE xlRange 2

/* Enum XlParameterDataType   -                                               */
&GLOBAL-DEFINE xlParamTypeUnknown 0
&GLOBAL-DEFINE xlParamTypeChar 1
&GLOBAL-DEFINE xlParamTypeNumeric 2
&GLOBAL-DEFINE xlParamTypeDecimal 3
&GLOBAL-DEFINE xlParamTypeInteger 4
&GLOBAL-DEFINE xlParamTypeSmallInt 5
&GLOBAL-DEFINE xlParamTypeFloat 6
&GLOBAL-DEFINE xlParamTypeReal 7
&GLOBAL-DEFINE xlParamTypeDouble 8
&GLOBAL-DEFINE xlParamTypeVarChar 12
&GLOBAL-DEFINE xlParamTypeDate 9
&GLOBAL-DEFINE xlParamTypeTime 10
&GLOBAL-DEFINE xlParamTypeTimestamp 11
&GLOBAL-DEFINE xlParamTypeLongVarChar -1
&GLOBAL-DEFINE xlParamTypeBinary -2
&GLOBAL-DEFINE xlParamTypeVarBinary -3
&GLOBAL-DEFINE xlParamTypeLongVarBinary -4
&GLOBAL-DEFINE xlParamTypeBigInt -5
&GLOBAL-DEFINE xlParamTypeTinyInt -6
&GLOBAL-DEFINE xlParamTypeBit -7
&GLOBAL-DEFINE xlParamTypeWChar -8

/* Enum XlFormControl   -                                                     */
&GLOBAL-DEFINE xlButtonControl 0
&GLOBAL-DEFINE xlCheckBox 1
&GLOBAL-DEFINE xlDropDown 2
&GLOBAL-DEFINE xlEditBox 3
&GLOBAL-DEFINE xlGroupBox 4
&GLOBAL-DEFINE xlLabel 5
&GLOBAL-DEFINE xlListBox 6
&GLOBAL-DEFINE xlOptionButton 7
&GLOBAL-DEFINE xlScrollBar 8
&GLOBAL-DEFINE xlSpinner 9

/* Enum XlSourceType   -                                                      */
&GLOBAL-DEFINE xlSourceSheet 1
&GLOBAL-DEFINE xlSourcePrintArea 2
&GLOBAL-DEFINE xlSourceAutoFilter 3
&GLOBAL-DEFINE xlSourceRange 4
&GLOBAL-DEFINE xlSourceChart 5
&GLOBAL-DEFINE xlSourcePivotTable 6
&GLOBAL-DEFINE xlSourceQuery 7

/* Enum XlHtmlType   -                                                        */
&GLOBAL-DEFINE xlHtmlStatic 0
&GLOBAL-DEFINE xlHtmlCalc 1
&GLOBAL-DEFINE xlHtmlList 2
&GLOBAL-DEFINE xlHtmlChart 3

/* Enum xlPivotFormatType   -                                                 */
&GLOBAL-DEFINE xlReport1 0
&GLOBAL-DEFINE xlReport2 1
&GLOBAL-DEFINE xlReport3 2
&GLOBAL-DEFINE xlReport4 3
&GLOBAL-DEFINE xlReport5 4
&GLOBAL-DEFINE xlReport6 5
&GLOBAL-DEFINE xlReport7 6
&GLOBAL-DEFINE xlReport8 7
&GLOBAL-DEFINE xlReport9 8
&GLOBAL-DEFINE xlReport10 9
&GLOBAL-DEFINE xlTable1 10
&GLOBAL-DEFINE xlTable2 11
&GLOBAL-DEFINE xlTable3 12
&GLOBAL-DEFINE xlTable4 13
&GLOBAL-DEFINE xlTable5 14
&GLOBAL-DEFINE xlTable6 15
&GLOBAL-DEFINE xlTable7 16
&GLOBAL-DEFINE xlTable8 17
&GLOBAL-DEFINE xlTable9 18
&GLOBAL-DEFINE xlTable10 19
&GLOBAL-DEFINE xlPTClassic 20
&GLOBAL-DEFINE xlPTNone 21

/* Enum XlCmdType   -                                                         */
&GLOBAL-DEFINE xlCmdCube 1
&GLOBAL-DEFINE xlCmdSql 2
&GLOBAL-DEFINE xlCmdTable 3
&GLOBAL-DEFINE xlCmdDefault 4

/* Enum xlColumnDataType   -                                                  */
&GLOBAL-DEFINE xlGeneralFormat 1
&GLOBAL-DEFINE xlTextFormat 2
&GLOBAL-DEFINE xlMDYFormat 3
&GLOBAL-DEFINE xlDMYFormat 4
&GLOBAL-DEFINE xlYMDFormat 5
&GLOBAL-DEFINE xlMYDFormat 6
&GLOBAL-DEFINE xlDYMFormat 7
&GLOBAL-DEFINE xlYDMFormat 8
&GLOBAL-DEFINE xlSkipColumn 9
&GLOBAL-DEFINE xlEMDFormat 10

/* Enum xlQueryType   -                                                       */
&GLOBAL-DEFINE xlODBCQuery 1
&GLOBAL-DEFINE xlDAORecordSet 2
&GLOBAL-DEFINE xlWebQuery 4
&GLOBAL-DEFINE xlOLEDBQuery 5
&GLOBAL-DEFINE xlTextImport 6
&GLOBAL-DEFINE xlADORecordset 7

/* Enum xlWebSelectionType   -                                                */
&GLOBAL-DEFINE xlEntirePage 1
&GLOBAL-DEFINE xlAllTables 2
&GLOBAL-DEFINE xlSpecifiedTables 3

/* Enum XlCubeFieldType   -                                                   */
&GLOBAL-DEFINE xlHierarchy 1
&GLOBAL-DEFINE xlMeasure 2

/* Enum xlWebFormatting   -                                                   */
&GLOBAL-DEFINE xlWebFormattingAll 1
&GLOBAL-DEFINE xlWebFormattingRTF 2
&GLOBAL-DEFINE xlWebFormattingNone 3

/* Enum xlDisplayDrawingObjects   -                                           */
&GLOBAL-DEFINE xlDisplayShapes -4104
&GLOBAL-DEFINE xlHide 3
&GLOBAL-DEFINE xlPlaceholders 2

/* Enum xLSubtototalLocationType   -                                          */
&GLOBAL-DEFINE xlAtTop 1
&GLOBAL-DEFINE xlAtBottom 2

&ENDIF

