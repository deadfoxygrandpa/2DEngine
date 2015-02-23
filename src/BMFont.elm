module BMFont where

import Text
import List
import String
import Char
import Color (Color)
import Dict
import Debug

import Parser (..)
import Parser.Char as PChar
import Parser.Number as Number

import Types
import Engine

-- Modeling of the BMFont format
type alias Info =
    { face : String
    , size : Float
    , bold : Bool
    , italic : Bool
    , charset : String
    , unicode : Bool
    , stretch : Float
    , smooth : Bool
    , superSampling : Int
    , padding : (Int, Int, Int, Int)
    , spacing : (Int, Int)
    , outline : Int
    }

type alias Common =
    { lineHeight : Int
    , base : Int
    , scaleW : Int
    , scaleH : Int
    , pages : Int
    , packed : Bool
    , alphaChannel : Int
    , redChannel : Int
    , greenChannel : Int
    , blueChannel : Int
    }

type alias Page =
    { id : Int
    , file : String
    }

type alias Character =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , xoffset : Int
    , yoffset : Int
    , xadvance : Int
    , page : Int
    , channel : Int
    }

type alias BMFont =
    { info : Info
    , common : Common
    , pages : List Page
    , characters : Dict.Dict Char Character
    }

-- Parsing the BMFont file formwat
infoParser : Parser Info
infoParser =
    let commaSeparatedInt = Number.integer <* symbol ','
        quad = (,,,) `map`
                commaSeparatedInt `and`
                commaSeparatedInt `and`
                commaSeparatedInt `and`
                Number.integer
        pair = (,) `map` commaSeparatedInt `and` Number.integer
    in
        Info `map`
        (token "info" *> (section "face" string `or` section "font" string)) `and`
        section "size" float `and`
        section "bold" bool `and`
        section "italic" bool `and`
        section "charset" string `and`
        section "unicode" bool `and`
        section "stretchH" float `and`
        section "smooth" bool `and`
        sectionInt "aa" `and`
        section "padding" quad `and`
        section "spacing" pair `and`
        (sectionInt "outline" `or` succeed 0)

commonParser : Parser Common
commonParser =
    Common `map`
    (token "common" *> sectionInt "lineHeight") `and`
    sectionInt "base" `and`
    sectionInt "scaleW" `and`
    sectionInt "scaleH" `and`
    sectionInt "pages" `and`
    section "packed" bool `and`
    sectionInt "alphaChnl" `and`
    sectionInt "redChnl" `and`
    sectionInt "greenChnl" `and`
    sectionInt "blueChnl"

pageParser : Parser Page
pageParser =
    Page `map`
    (token "page" *> sectionInt "id") `and`
    section "file" string

charParser : Parser Character
charParser =
    Character `map`
    (token "char" *> sectionInt "id") `and`
    sectionInt "x" `and`
    sectionInt "y" `and`
    sectionInt "width" `and`
    sectionInt "height" `and`
    sectionInt "xoffset" `and`
    sectionInt "yoffset" `and`
    sectionInt "xadvance" `and`
    sectionInt "page" `and`
    sectionInt "chnl"

parser : Parser BMFont
parser =
    let makePair char = (Char.fromCode char.id, char)
        charPairs = token "chars count="
                      *> Number.integer
                      *> newline
                      *> map (List.map makePair) (separatedBy charParser newline)
        charDict = map Dict.fromList charPairs
    in
        BMFont `map`
        infoParser <* newline `and`
        commonParser <* newline `and`
        separatedBy pageParser newline <* newline `and`
        charDict

-- Parsing helper functions
section : String -> Parser a -> Parser a
section s p = whitespace *> token s *> symbol '=' *> p

sectionInt : String -> Parser Int
sectionInt s = section s Number.integer

optionalSection : String -> Parser a -> a -> Parser a
optionalSection s p default = section s p `or` succeed default

optionalSectionInt : String -> Parser Int
optionalSectionInt s = sectionInt s `or` succeed 0

whitespace : Parser ()
whitespace = some (symbol ' ' `or` symbol '\t') *> succeed ()

newline : Parser ()
newline = (token "\r" `or` token "\n" `or` token "\r\n") *> succeed ()

anyChar : Parser Char
anyChar = satisfy (\c -> c /= '"')

char : Parser Char
char = PChar.between '"' '"' anyChar

string : Parser String
string =
    let string' = map String.fromList (many anyChar)
    in
        PChar.between '"' '"' string'

bool : Parser Bool
bool = (symbol '0' *> succeed False) `or` (symbol '1' *> succeed True)

float : Parser Float
float = Number.float `or` (map toFloat Number.integer)

-- API

write : String -> Color -> Types.Texture -> (Int, Int) -> List Types.Tile
write s color texture (startingX, startingY) =
    let chars = String.toList s
        makeTile (char, x') = let {x, y, width, height} = getSpriteCoordinates char
                              in
                                Engine.spriteTile x y width height color texture (x', startingY)
        xs = [startingX .. List.length chars + startingX]
    in
        List.map makeTile <| List.map2 (,) chars xs

getSpriteCoordinates : Char -> {x : Int, y : Int, width : Int, height : Int}
getSpriteCoordinates char =
    let char' = case Dict.get char font.characters of
                        Just c -> c
                        Nothing -> case Dict.get '?' font.characters of
                                    Just c -> c
                                    Nothing -> defaultChar
    in
        {x = char'.x, y = char'.y, width = char'.width, height = char'.height}

defaultChar : Character
defaultChar =
    Character 0 0 0 1 1 0 0 0 0 0

font : BMFont
font = case parse parser bmfont of
    Ok (x::xs) -> x

-- Demo stuff
main = Text.asText <| font

bmfont = """info face="Consolas" size=32 bold=0 italic=0 charset="ANSI" unicode=0 stretchH=100 smooth=1 aa=1 padding=0,0,0,0 spacing=1,1 outline=0
common lineHeight=32 base=25 scaleW=512 scaleH=512 pages=1 packed=0 alphaChnl=0 redChnl=4 greenChnl=4 blueChnl=4
page id=0 file="font2_0.png"
chars count=193
char id=0    x=192   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=13   x=501   y=0     width=3     height=32    xoffset=0     yoffset=0     xadvance=3     page=0  chnl=15
char id=32   x=144   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=33   x=304   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=34   x=0     y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=35   x=197   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=36   x=213   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=37   x=229   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=38   x=129   y=0     width=16    height=32    xoffset=0     yoffset=0     xadvance=16    page=0  chnl=15
char id=39   x=245   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=40   x=261   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=41   x=277   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=42   x=293   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=43   x=309   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=44   x=325   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=45   x=341   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=46   x=357   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=47   x=373   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=48   x=389   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=49   x=405   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=50   x=421   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=51   x=437   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=52   x=453   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=53   x=469   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=54   x=485   y=0     width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=55   x=0     y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=56   x=16    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=57   x=32    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=58   x=48    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=59   x=64    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=60   x=80    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=61   x=96    y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=62   x=112   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=63   x=128   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=64   x=146   y=0     width=16    height=32    xoffset=0     yoffset=0     xadvance=16    page=0  chnl=15
char id=65   x=144   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=66   x=160   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=67   x=176   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=68   x=192   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=69   x=208   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=70   x=224   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=71   x=240   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=72   x=256   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=73   x=272   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=74   x=288   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=75   x=304   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=76   x=320   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=77   x=336   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=78   x=352   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=79   x=368   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=80   x=384   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=81   x=112   y=0     width=16    height=32    xoffset=0     yoffset=0     xadvance=16    page=0  chnl=15
char id=82   x=400   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=83   x=416   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=84   x=432   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=85   x=448   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=86   x=464   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=87   x=480   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=88   x=496   y=33    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=89   x=40    y=0     width=17    height=32    xoffset=0     yoffset=0     xadvance=17    page=0  chnl=15
char id=90   x=0     y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=91   x=16    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=92   x=32    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=93   x=48    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=94   x=64    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=95   x=58    y=0     width=17    height=32    xoffset=0     yoffset=0     xadvance=17    page=0  chnl=15
char id=96   x=80    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=97   x=96    y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=98   x=112   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=99   x=128   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=100  x=144   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=101  x=160   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=102  x=176   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=103  x=192   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=104  x=208   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=105  x=224   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=106  x=240   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=107  x=256   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=108  x=272   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=109  x=288   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=110  x=304   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=111  x=320   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=112  x=336   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=113  x=352   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=114  x=368   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=115  x=384   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=116  x=400   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=117  x=416   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=118  x=432   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=119  x=448   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=120  x=464   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=121  x=480   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=122  x=496   y=66    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=123  x=0     y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=124  x=16    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=125  x=32    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=126  x=48    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=160  x=0     y=0     width=39    height=32    xoffset=0     yoffset=0     xadvance=39    page=0  chnl=15
char id=161  x=64    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=162  x=80    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=163  x=96    y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=164  x=112   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=165  x=128   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=166  x=144   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=167  x=160   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=168  x=176   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=169  x=76    y=0     width=17    height=32    xoffset=0     yoffset=0     xadvance=17    page=0  chnl=15
char id=170  x=208   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=171  x=224   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=172  x=240   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=173  x=256   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=174  x=272   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=175  x=288   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=176  x=304   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=177  x=320   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=178  x=336   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=179  x=352   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=180  x=368   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=181  x=384   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=182  x=400   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=183  x=416   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=184  x=432   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=185  x=448   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=186  x=464   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=187  x=480   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=188  x=496   y=99    width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=189  x=0     y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=190  x=16    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=191  x=32    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=192  x=48    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=193  x=64    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=194  x=80    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=195  x=96    y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=196  x=112   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=197  x=128   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=198  x=163   y=0     width=16    height=32    xoffset=0     yoffset=0     xadvance=16    page=0  chnl=15
char id=199  x=160   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=200  x=176   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=201  x=192   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=202  x=208   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=203  x=224   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=204  x=240   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=205  x=256   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=206  x=272   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=207  x=288   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=208  x=180   y=0     width=16    height=32    xoffset=0     yoffset=0     xadvance=16    page=0  chnl=15
char id=209  x=320   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=210  x=336   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=211  x=352   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=212  x=368   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=213  x=384   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=214  x=400   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=215  x=416   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=216  x=432   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=217  x=448   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=218  x=464   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=219  x=480   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=220  x=496   y=132   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=221  x=94    y=0     width=17    height=32    xoffset=0     yoffset=0     xadvance=17    page=0  chnl=15
char id=222  x=16    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=223  x=32    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=224  x=48    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=225  x=64    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=226  x=80    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=227  x=96    y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=228  x=112   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=229  x=128   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=230  x=144   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=231  x=160   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=232  x=176   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=233  x=192   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=234  x=208   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=235  x=224   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=236  x=240   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=237  x=256   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=238  x=272   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=239  x=288   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=240  x=304   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=241  x=320   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=242  x=336   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=243  x=352   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=244  x=368   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=245  x=384   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=246  x=400   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=247  x=416   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=248  x=432   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=249  x=448   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=250  x=464   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=251  x=480   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=252  x=496   y=165   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=253  x=0     y=198   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=254  x=16    y=198   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
char id=255  x=32    y=198   width=15    height=32    xoffset=0     yoffset=0     xadvance=15    page=0  chnl=15
"""
