module Styles exposing (..)

import Css exposing (..)

import Model exposing (..)

colorName : CellColor -> Color
colorName color = case color of
    Red -> rgb 255 0 0
    Green -> rgb 0 128 0
    Blue -> rgb 0 0 255
    Yellow -> rgb 255 255 0
    Empty -> rgb 255 255 255

borderColorName : CellColor -> Color
borderColorName color = case color of
    Red -> rgb 139 0 0
    Green -> rgb 0 100 0
    Blue -> rgb 0 0 139
    Yellow -> rgb 128 128 0
    Empty -> rgb 255 255 255

cellStyles : CellColor -> List Style
cellStyles color = [
    width (px 40),
    height (px 40),
    backgroundColor (colorName color),
    border3 (px 2) solid (borderColorName color),
    cursor pointer
    ]

tableRowStyles : List Style
tableRowStyles = [displayFlex]

tableStyles : List Style
tableStyles = [
    border3 (px 1) solid (rgb 0 0 0),
    padding (px 5),
    backgroundColor (rgb 255 255 255)
    ]

restartButtonStyles : List Style
restartButtonStyles = [
    marginLeft (px 10),
    backgroundColor (hex "#FAFBFC"),
    border3 (px 1) solid (rgba 27 31 35 0.15),
    borderRadius (px 6),
    color (hex "#24292E"),
    cursor pointer,
    fontSize (px 14),
    lineHeight (px 20),
    padding2 (px 6) (px 16),
    property "transition" "background-color 0.2s cubic-bezier(0.3, 0, 0.5, 1)"
    ]

restartButtonHoverStyle : Style
restartButtonHoverStyle = hover [
    backgroundColor (hex "#F3F4F6"),
    property "transition-duration" "0.1s"
    ]

scoreContainerStyles : List Style
scoreContainerStyles = [
    marginLeft auto,
    marginTop auto,
    padding (px 2)
    ]

gameTitleStyles : List Style
gameTitleStyles = [fontSize (px 40)]

headerStyles : List Style
headerStyles = [
    displayFlex,
    flexDirection row,
    color (rgb 255 255 255)
    ]

gameContainerStyles : List Style
gameContainerStyles = [
    displayFlex,
    flexDirection column
    ]

pageContainerStyles : List Style
pageContainerStyles = [
    width (vw 100),
    height (vh 100),
    displayFlex,
    justifyContent center,
    alignItems center,
    backgroundColor (hex "#202020"),
    fontFamilies ["Helvetica", "sans-serif"]
    ]