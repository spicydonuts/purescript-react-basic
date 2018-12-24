-- | ----------------------------------------
-- | THIS FILE IS GENERATED -- DO NOT EDIT IT
-- | ----------------------------------------

module React.Basic.Hooks.DOM.Generated where

import Prim.Row (class Union)
import React.Basic.Hooks (JSX, element)
import React.Basic.Hooks.DOM.Internal (SharedProps, unsafeCreateDOMComponent)
import React.Basic.Hooks.Events (EventHandler)

type Props_a =
  ( children :: Array JSX
  , coords :: String
  , download :: String
  , href :: String
  , name :: String
  , onClick :: EventHandler
  , rel :: String
  , shape :: String
  , target :: String
  , type :: String
  )

a
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_a)
  => Record attrs
  -> JSX
a = element (unsafeCreateDOMComponent "a")

a_ :: Array JSX -> JSX
a_ children = a { children }

type Props_abbr =
  ( children :: Array JSX
  , title :: String
  )

abbr
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_abbr)
  => Record attrs
  -> JSX
abbr = element (unsafeCreateDOMComponent "abbr")

abbr_ :: Array JSX -> JSX
abbr_ children = abbr { children }

type Props_address =
  ( children :: Array JSX
  )

address
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_address)
  => Record attrs
  -> JSX
address = element (unsafeCreateDOMComponent "address")

address_ :: Array JSX -> JSX
address_ children = address { children }

type Props_area =
  ( alt :: String
  , coords :: String
  , download :: String
  , href :: String
  , rel :: String
  , shape :: String
  , target :: String
  , type :: String
  )

area
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_area)
  => Record attrs
  -> JSX
area = element (unsafeCreateDOMComponent "area")

type Props_article =
  ( children :: Array JSX
  )

article
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_article)
  => Record attrs
  -> JSX
article = element (unsafeCreateDOMComponent "article")

article_ :: Array JSX -> JSX
article_ children = article { children }

type Props_aside =
  ( children :: Array JSX
  )

aside
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_aside)
  => Record attrs
  -> JSX
aside = element (unsafeCreateDOMComponent "aside")

aside_ :: Array JSX -> JSX
aside_ children = aside { children }

type Props_audio =
  ( children :: Array JSX
  , controls :: Boolean
  , loop :: Boolean
  , muted :: Boolean
  , preload :: String
  , src :: String
  )

audio
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_audio)
  => Record attrs
  -> JSX
audio = element (unsafeCreateDOMComponent "audio")

audio_ :: Array JSX -> JSX
audio_ children = audio { children }

type Props_b =
  ( children :: Array JSX
  )

b
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_b)
  => Record attrs
  -> JSX
b = element (unsafeCreateDOMComponent "b")

b_ :: Array JSX -> JSX
b_ children = b { children }

type Props_base =
  ( href :: String
  , target :: String
  )

base
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_base)
  => Record attrs
  -> JSX
base = element (unsafeCreateDOMComponent "base")

type Props_bdi =
  ( children :: Array JSX
  )

bdi
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_bdi)
  => Record attrs
  -> JSX
bdi = element (unsafeCreateDOMComponent "bdi")

bdi_ :: Array JSX -> JSX
bdi_ children = bdi { children }

type Props_bdo =
  ( children :: Array JSX
  , dir :: String
  )

bdo
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_bdo)
  => Record attrs
  -> JSX
bdo = element (unsafeCreateDOMComponent "bdo")

bdo_ :: Array JSX -> JSX
bdo_ children = bdo { children }

type Props_blockquote =
  ( children :: Array JSX
  , cite :: String
  )

blockquote
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_blockquote)
  => Record attrs
  -> JSX
blockquote = element (unsafeCreateDOMComponent "blockquote")

blockquote_ :: Array JSX -> JSX
blockquote_ children = blockquote { children }

type Props_body =
  ( children :: Array JSX
  )

body
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_body)
  => Record attrs
  -> JSX
body = element (unsafeCreateDOMComponent "body")

body_ :: Array JSX -> JSX
body_ children = body { children }

type Props_br =()

br
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_br)
  => Record attrs
  -> JSX
br = element (unsafeCreateDOMComponent "br")

type Props_button =
  ( children :: Array JSX
  , disabled :: Boolean
  , form :: String
  , name :: String
  , type :: String
  , value :: String
  )

button
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_button)
  => Record attrs
  -> JSX
button = element (unsafeCreateDOMComponent "button")

button_ :: Array JSX -> JSX
button_ children = button { children }

type Props_canvas =
  ( children :: Array JSX
  , height :: String
  , width :: String
  )

canvas
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_canvas)
  => Record attrs
  -> JSX
canvas = element (unsafeCreateDOMComponent "canvas")

canvas_ :: Array JSX -> JSX
canvas_ children = canvas { children }

type Props_caption =
  ( children :: Array JSX
  )

caption
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_caption)
  => Record attrs
  -> JSX
caption = element (unsafeCreateDOMComponent "caption")

caption_ :: Array JSX -> JSX
caption_ children = caption { children }

type Props_cite =
  ( children :: Array JSX
  )

cite
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_cite)
  => Record attrs
  -> JSX
cite = element (unsafeCreateDOMComponent "cite")

cite_ :: Array JSX -> JSX
cite_ children = cite { children }

type Props_code =
  ( children :: Array JSX
  )

code
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_code)
  => Record attrs
  -> JSX
code = element (unsafeCreateDOMComponent "code")

code_ :: Array JSX -> JSX
code_ children = code { children }

type Props_col =
  ( span :: Number
  , width :: String
  )

col
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_col)
  => Record attrs
  -> JSX
col = element (unsafeCreateDOMComponent "col")

type Props_colgroup =
  ( children :: Array JSX
  , span :: Number
  , width :: String
  )

colgroup
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_colgroup)
  => Record attrs
  -> JSX
colgroup = element (unsafeCreateDOMComponent "colgroup")

colgroup_ :: Array JSX -> JSX
colgroup_ children = colgroup { children }

type Props_data =
  ( children :: Array JSX
  , value :: String
  )

data'
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_data)
  => Record attrs
  -> JSX
data' = element (unsafeCreateDOMComponent "data")

data_ :: Array JSX -> JSX
data_ children = data' { children }

type Props_datalist =
  ( children :: Array JSX
  )

datalist
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_datalist)
  => Record attrs
  -> JSX
datalist = element (unsafeCreateDOMComponent "datalist")

datalist_ :: Array JSX -> JSX
datalist_ children = datalist { children }

type Props_dd =
  ( children :: Array JSX
  )

dd
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_dd)
  => Record attrs
  -> JSX
dd = element (unsafeCreateDOMComponent "dd")

dd_ :: Array JSX -> JSX
dd_ children = dd { children }

type Props_del =
  ( children :: Array JSX
  , cite :: String
  )

del
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_del)
  => Record attrs
  -> JSX
del = element (unsafeCreateDOMComponent "del")

del_ :: Array JSX -> JSX
del_ children = del { children }

type Props_details =
  ( children :: Array JSX
  , open :: Boolean
  )

details
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_details)
  => Record attrs
  -> JSX
details = element (unsafeCreateDOMComponent "details")

details_ :: Array JSX -> JSX
details_ children = details { children }

type Props_dfn =
  ( children :: Array JSX
  , title :: String
  )

dfn
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_dfn)
  => Record attrs
  -> JSX
dfn = element (unsafeCreateDOMComponent "dfn")

dfn_ :: Array JSX -> JSX
dfn_ children = dfn { children }

type Props_dialog =
  ( children :: Array JSX
  , open :: Boolean
  )

dialog
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_dialog)
  => Record attrs
  -> JSX
dialog = element (unsafeCreateDOMComponent "dialog")

dialog_ :: Array JSX -> JSX
dialog_ children = dialog { children }

type Props_div =
  ( children :: Array JSX
  )

div
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_div)
  => Record attrs
  -> JSX
div = element (unsafeCreateDOMComponent "div")

div_ :: Array JSX -> JSX
div_ children = div { children }

type Props_dl =
  ( children :: Array JSX
  )

dl
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_dl)
  => Record attrs
  -> JSX
dl = element (unsafeCreateDOMComponent "dl")

dl_ :: Array JSX -> JSX
dl_ children = dl { children }

type Props_dt =
  ( children :: Array JSX
  )

dt
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_dt)
  => Record attrs
  -> JSX
dt = element (unsafeCreateDOMComponent "dt")

dt_ :: Array JSX -> JSX
dt_ children = dt { children }

type Props_em =
  ( children :: Array JSX
  )

em
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_em)
  => Record attrs
  -> JSX
em = element (unsafeCreateDOMComponent "em")

em_ :: Array JSX -> JSX
em_ children = em { children }

type Props_embed =
  ( height :: String
  , src :: String
  , type :: String
  , width :: String
  )

embed
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_embed)
  => Record attrs
  -> JSX
embed = element (unsafeCreateDOMComponent "embed")

type Props_fieldset =
  ( children :: Array JSX
  , disabled :: Boolean
  , form :: String
  , name :: String
  )

fieldset
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_fieldset)
  => Record attrs
  -> JSX
fieldset = element (unsafeCreateDOMComponent "fieldset")

fieldset_ :: Array JSX -> JSX
fieldset_ children = fieldset { children }

type Props_figcaption =
  ( children :: Array JSX
  )

figcaption
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_figcaption)
  => Record attrs
  -> JSX
figcaption = element (unsafeCreateDOMComponent "figcaption")

figcaption_ :: Array JSX -> JSX
figcaption_ children = figcaption { children }

type Props_figure =
  ( children :: Array JSX
  )

figure
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_figure)
  => Record attrs
  -> JSX
figure = element (unsafeCreateDOMComponent "figure")

figure_ :: Array JSX -> JSX
figure_ children = figure { children }

type Props_footer =
  ( children :: Array JSX
  )

footer
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_footer)
  => Record attrs
  -> JSX
footer = element (unsafeCreateDOMComponent "footer")

footer_ :: Array JSX -> JSX
footer_ children = footer { children }

type Props_form =
  ( accept :: String
  , action :: String
  , children :: Array JSX
  , method :: String
  , name :: String
  , onChange :: EventHandler
  , onInput :: EventHandler
  , onInvalid :: EventHandler
  , onSubmit :: EventHandler
  , target :: String
  )

form
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_form)
  => Record attrs
  -> JSX
form = element (unsafeCreateDOMComponent "form")

form_ :: Array JSX -> JSX
form_ children = form { children }

type Props_h1 =
  ( children :: Array JSX
  )

h1
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h1)
  => Record attrs
  -> JSX
h1 = element (unsafeCreateDOMComponent "h1")

h1_ :: Array JSX -> JSX
h1_ children = h1 { children }

type Props_h2 =
  ( children :: Array JSX
  )

h2
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h2)
  => Record attrs
  -> JSX
h2 = element (unsafeCreateDOMComponent "h2")

h2_ :: Array JSX -> JSX
h2_ children = h2 { children }

type Props_h3 =
  ( children :: Array JSX
  )

h3
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h3)
  => Record attrs
  -> JSX
h3 = element (unsafeCreateDOMComponent "h3")

h3_ :: Array JSX -> JSX
h3_ children = h3 { children }

type Props_h4 =
  ( children :: Array JSX
  )

h4
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h4)
  => Record attrs
  -> JSX
h4 = element (unsafeCreateDOMComponent "h4")

h4_ :: Array JSX -> JSX
h4_ children = h4 { children }

type Props_h5 =
  ( children :: Array JSX
  )

h5
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h5)
  => Record attrs
  -> JSX
h5 = element (unsafeCreateDOMComponent "h5")

h5_ :: Array JSX -> JSX
h5_ children = h5 { children }

type Props_h6 =
  ( children :: Array JSX
  )

h6
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_h6)
  => Record attrs
  -> JSX
h6 = element (unsafeCreateDOMComponent "h6")

h6_ :: Array JSX -> JSX
h6_ children = h6 { children }

type Props_head =
  ( children :: Array JSX
  , profile :: String
  )

head
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_head)
  => Record attrs
  -> JSX
head = element (unsafeCreateDOMComponent "head")

head_ :: Array JSX -> JSX
head_ children = head { children }

type Props_header =
  ( children :: Array JSX
  )

header
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_header)
  => Record attrs
  -> JSX
header = element (unsafeCreateDOMComponent "header")

header_ :: Array JSX -> JSX
header_ children = header { children }

type Props_hgroup =
  ( children :: Array JSX
  )

hgroup
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_hgroup)
  => Record attrs
  -> JSX
hgroup = element (unsafeCreateDOMComponent "hgroup")

hgroup_ :: Array JSX -> JSX
hgroup_ children = hgroup { children }

type Props_hr =
  ( size :: Number
  , width :: String
  )

hr
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_hr)
  => Record attrs
  -> JSX
hr = element (unsafeCreateDOMComponent "hr")

type Props_html =
  ( children :: Array JSX
  , manifest :: String
  )

html
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_html)
  => Record attrs
  -> JSX
html = element (unsafeCreateDOMComponent "html")

html_ :: Array JSX -> JSX
html_ children = html { children }

type Props_i =
  ( children :: Array JSX
  )

i
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_i)
  => Record attrs
  -> JSX
i = element (unsafeCreateDOMComponent "i")

i_ :: Array JSX -> JSX
i_ children = i { children }

type Props_iframe =
  ( children :: Array JSX
  , height :: String
  , name :: String
  , sandbox :: String
  , scrolling :: String
  , src :: String
  , width :: String
  )

iframe
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_iframe)
  => Record attrs
  -> JSX
iframe = element (unsafeCreateDOMComponent "iframe")

iframe_ :: Array JSX -> JSX
iframe_ children = iframe { children }

type Props_img =
  ( alt :: String
  , height :: String
  , name :: String
  , sizes :: String
  , src :: String
  , width :: String
  )

img
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_img)
  => Record attrs
  -> JSX
img = element (unsafeCreateDOMComponent "img")

type Props_input =
  ( accept :: String
  , alt :: String
  , autoCapitalize :: String
  , autoCorrect :: String
  , autoSave :: String
  , checked :: Boolean
  , defaultChecked :: String
  , defaultValue :: String
  , disabled :: Boolean
  , form :: String
  , height :: String
  , list :: String
  , max :: Number
  , min :: Number
  , multiple :: Boolean
  , name :: String
  , onChange :: EventHandler
  , pattern :: String
  , placeholder :: String
  , required :: Boolean
  , results :: String
  , size :: Number
  , src :: String
  , step :: String
  , title :: String
  , type :: String
  , value :: String
  , width :: String
  )

input
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_input)
  => Record attrs
  -> JSX
input = element (unsafeCreateDOMComponent "input")

type Props_ins =
  ( children :: Array JSX
  , cite :: String
  )

ins
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_ins)
  => Record attrs
  -> JSX
ins = element (unsafeCreateDOMComponent "ins")

ins_ :: Array JSX -> JSX
ins_ children = ins { children }

type Props_kbd =
  ( children :: Array JSX
  )

kbd
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_kbd)
  => Record attrs
  -> JSX
kbd = element (unsafeCreateDOMComponent "kbd")

kbd_ :: Array JSX -> JSX
kbd_ children = kbd { children }

type Props_keygen =
  ( challenge :: String
  , children :: Array JSX
  , disabled :: Boolean
  , form :: String
  , name :: String
  )

keygen
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_keygen)
  => Record attrs
  -> JSX
keygen = element (unsafeCreateDOMComponent "keygen")

keygen_ :: Array JSX -> JSX
keygen_ children = keygen { children }

type Props_label =
  ( children :: Array JSX
  , form :: String
  )

label
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_label)
  => Record attrs
  -> JSX
label = element (unsafeCreateDOMComponent "label")

label_ :: Array JSX -> JSX
label_ children = label { children }

type Props_legend =
  ( children :: Array JSX
  )

legend
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_legend)
  => Record attrs
  -> JSX
legend = element (unsafeCreateDOMComponent "legend")

legend_ :: Array JSX -> JSX
legend_ children = legend { children }

type Props_li =
  ( children :: Array JSX
  , type :: String
  , value :: String
  )

li
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_li)
  => Record attrs
  -> JSX
li = element (unsafeCreateDOMComponent "li")

li_ :: Array JSX -> JSX
li_ children = li { children }

type Props_link =
  ( color :: String
  , href :: String
  , integrity :: String
  , media :: String
  , nonce :: String
  , rel :: String
  , scope :: String
  , sizes :: String
  , target :: String
  , title :: String
  , type :: String
  )

link
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_link)
  => Record attrs
  -> JSX
link = element (unsafeCreateDOMComponent "link")

type Props_main =
  ( children :: Array JSX
  )

main
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_main)
  => Record attrs
  -> JSX
main = element (unsafeCreateDOMComponent "main")

main_ :: Array JSX -> JSX
main_ children = main { children }

type Props_map =
  ( children :: Array JSX
  , name :: String
  )

map
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_map)
  => Record attrs
  -> JSX
map = element (unsafeCreateDOMComponent "map")

map_ :: Array JSX -> JSX
map_ children = map { children }

type Props_mark =
  ( children :: Array JSX
  )

mark
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_mark)
  => Record attrs
  -> JSX
mark = element (unsafeCreateDOMComponent "mark")

mark_ :: Array JSX -> JSX
mark_ children = mark { children }

type Props_math =
  ( children :: Array JSX
  )

math
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_math)
  => Record attrs
  -> JSX
math = element (unsafeCreateDOMComponent "math")

math_ :: Array JSX -> JSX
math_ children = math { children }

type Props_menu =
  ( children :: Array JSX
  )

menu
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_menu)
  => Record attrs
  -> JSX
menu = element (unsafeCreateDOMComponent "menu")

menu_ :: Array JSX -> JSX
menu_ children = menu { children }

type Props_menuitem =
  ( children :: Array JSX
  )

menuitem
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_menuitem)
  => Record attrs
  -> JSX
menuitem = element (unsafeCreateDOMComponent "menuitem")

menuitem_ :: Array JSX -> JSX
menuitem_ children = menuitem { children }

type Props_meta =
  ( content :: String
  , name :: String
  )

meta
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_meta)
  => Record attrs
  -> JSX
meta = element (unsafeCreateDOMComponent "meta")

type Props_meter =
  ( children :: Array JSX
  , high :: String
  , low :: String
  , max :: Number
  , min :: Number
  , optimum :: String
  , value :: String
  )

meter
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_meter)
  => Record attrs
  -> JSX
meter = element (unsafeCreateDOMComponent "meter")

meter_ :: Array JSX -> JSX
meter_ children = meter { children }

type Props_nav =
  ( children :: Array JSX
  )

nav
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_nav)
  => Record attrs
  -> JSX
nav = element (unsafeCreateDOMComponent "nav")

nav_ :: Array JSX -> JSX
nav_ children = nav { children }

type Props_noscript =
  ( children :: Array JSX
  )

noscript
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_noscript)
  => Record attrs
  -> JSX
noscript = element (unsafeCreateDOMComponent "noscript")

noscript_ :: Array JSX -> JSX
noscript_ children = noscript { children }

type Props_object =
  ( children :: Array JSX
  , data :: String
  , form :: String
  , height :: String
  , name :: String
  , type :: String
  , width :: String
  )

object
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_object)
  => Record attrs
  -> JSX
object = element (unsafeCreateDOMComponent "object")

object_ :: Array JSX -> JSX
object_ children = object { children }

type Props_ol =
  ( children :: Array JSX
  , reversed :: Boolean
  , start :: Number
  , type :: String
  )

ol
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_ol)
  => Record attrs
  -> JSX
ol = element (unsafeCreateDOMComponent "ol")

ol_ :: Array JSX -> JSX
ol_ children = ol { children }

type Props_optgroup =
  ( children :: Array JSX
  , disabled :: Boolean
  , label :: String
  )

optgroup
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_optgroup)
  => Record attrs
  -> JSX
optgroup = element (unsafeCreateDOMComponent "optgroup")

optgroup_ :: Array JSX -> JSX
optgroup_ children = optgroup { children }

type Props_option =
  ( children :: Array JSX
  , disabled :: Boolean
  , label :: String
  , selected :: Boolean
  , value :: String
  )

option
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_option)
  => Record attrs
  -> JSX
option = element (unsafeCreateDOMComponent "option")

option_ :: Array JSX -> JSX
option_ children = option { children }

type Props_output =
  ( children :: Array JSX
  , form :: String
  , name :: String
  )

output
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_output)
  => Record attrs
  -> JSX
output = element (unsafeCreateDOMComponent "output")

output_ :: Array JSX -> JSX
output_ children = output { children }

type Props_p =
  ( children :: Array JSX
  )

p
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_p)
  => Record attrs
  -> JSX
p = element (unsafeCreateDOMComponent "p")

p_ :: Array JSX -> JSX
p_ children = p { children }

type Props_param =
  ( name :: String
  , type :: String
  , value :: String
  )

param
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_param)
  => Record attrs
  -> JSX
param = element (unsafeCreateDOMComponent "param")

type Props_picture =
  ( children :: Array JSX
  )

picture
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_picture)
  => Record attrs
  -> JSX
picture = element (unsafeCreateDOMComponent "picture")

picture_ :: Array JSX -> JSX
picture_ children = picture { children }

type Props_pre =
  ( children :: Array JSX
  , width :: String
  )

pre
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_pre)
  => Record attrs
  -> JSX
pre = element (unsafeCreateDOMComponent "pre")

pre_ :: Array JSX -> JSX
pre_ children = pre { children }

type Props_progress =
  ( children :: Array JSX
  , max :: Number
  , value :: String
  )

progress
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_progress)
  => Record attrs
  -> JSX
progress = element (unsafeCreateDOMComponent "progress")

progress_ :: Array JSX -> JSX
progress_ children = progress { children }

type Props_q =
  ( children :: Array JSX
  , cite :: String
  )

q
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_q)
  => Record attrs
  -> JSX
q = element (unsafeCreateDOMComponent "q")

q_ :: Array JSX -> JSX
q_ children = q { children }

type Props_rb =
  ( children :: Array JSX
  )

rb
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_rb)
  => Record attrs
  -> JSX
rb = element (unsafeCreateDOMComponent "rb")

rb_ :: Array JSX -> JSX
rb_ children = rb { children }

type Props_rp =
  ( children :: Array JSX
  )

rp
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_rp)
  => Record attrs
  -> JSX
rp = element (unsafeCreateDOMComponent "rp")

rp_ :: Array JSX -> JSX
rp_ children = rp { children }

type Props_rt =
  ( children :: Array JSX
  )

rt
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_rt)
  => Record attrs
  -> JSX
rt = element (unsafeCreateDOMComponent "rt")

rt_ :: Array JSX -> JSX
rt_ children = rt { children }

type Props_rtc =
  ( children :: Array JSX
  )

rtc
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_rtc)
  => Record attrs
  -> JSX
rtc = element (unsafeCreateDOMComponent "rtc")

rtc_ :: Array JSX -> JSX
rtc_ children = rtc { children }

type Props_ruby =
  ( children :: Array JSX
  )

ruby
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_ruby)
  => Record attrs
  -> JSX
ruby = element (unsafeCreateDOMComponent "ruby")

ruby_ :: Array JSX -> JSX
ruby_ children = ruby { children }

type Props_s =
  ( children :: Array JSX
  )

s
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_s)
  => Record attrs
  -> JSX
s = element (unsafeCreateDOMComponent "s")

s_ :: Array JSX -> JSX
s_ children = s { children }

type Props_samp =
  ( children :: Array JSX
  )

samp
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_samp)
  => Record attrs
  -> JSX
samp = element (unsafeCreateDOMComponent "samp")

samp_ :: Array JSX -> JSX
samp_ children = samp { children }

type Props_script =
  ( async :: Boolean
  , children :: Array JSX
  , defer :: Boolean
  , integrity :: String
  , nonce :: String
  , src :: String
  , type :: String
  )

script
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_script)
  => Record attrs
  -> JSX
script = element (unsafeCreateDOMComponent "script")

script_ :: Array JSX -> JSX
script_ children = script { children }

type Props_section =
  ( children :: Array JSX
  )

section
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_section)
  => Record attrs
  -> JSX
section = element (unsafeCreateDOMComponent "section")

section_ :: Array JSX -> JSX
section_ children = section { children }

type Props_select =
  ( children :: Array JSX
  , defaultValue :: String
  , disabled :: Boolean
  , form :: String
  , multiple :: Boolean
  , name :: String
  , onChange :: EventHandler
  , required :: Boolean
  , size :: Number
  , value :: String
  )

select
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_select)
  => Record attrs
  -> JSX
select = element (unsafeCreateDOMComponent "select")

select_ :: Array JSX -> JSX
select_ children = select { children }

type Props_slot =
  ( children :: Array JSX
  , name :: String
  )

slot
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_slot)
  => Record attrs
  -> JSX
slot = element (unsafeCreateDOMComponent "slot")

slot_ :: Array JSX -> JSX
slot_ children = slot { children }

type Props_small =
  ( children :: Array JSX
  )

small
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_small)
  => Record attrs
  -> JSX
small = element (unsafeCreateDOMComponent "small")

small_ :: Array JSX -> JSX
small_ children = small { children }

type Props_source =
  ( media :: String
  , sizes :: String
  , src :: String
  , type :: String
  )

source
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_source)
  => Record attrs
  -> JSX
source = element (unsafeCreateDOMComponent "source")

type Props_span =
  ( children :: Array JSX
  )

span
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_span)
  => Record attrs
  -> JSX
span = element (unsafeCreateDOMComponent "span")

span_ :: Array JSX -> JSX
span_ children = span { children }

type Props_strong =
  ( children :: Array JSX
  )

strong
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_strong)
  => Record attrs
  -> JSX
strong = element (unsafeCreateDOMComponent "strong")

strong_ :: Array JSX -> JSX
strong_ children = strong { children }

type Props_style =
  ( children :: Array JSX
  , media :: String
  , nonce :: String
  , title :: String
  , type :: String
  )

style
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_style)
  => Record attrs
  -> JSX
style = element (unsafeCreateDOMComponent "style")

style_ :: Array JSX -> JSX
style_ children = style { children }

type Props_sub =
  ( children :: Array JSX
  )

sub
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_sub)
  => Record attrs
  -> JSX
sub = element (unsafeCreateDOMComponent "sub")

sub_ :: Array JSX -> JSX
sub_ children = sub { children }

type Props_summary =
  ( children :: Array JSX
  )

summary
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_summary)
  => Record attrs
  -> JSX
summary = element (unsafeCreateDOMComponent "summary")

summary_ :: Array JSX -> JSX
summary_ children = summary { children }

type Props_sup =
  ( children :: Array JSX
  )

sup
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_sup)
  => Record attrs
  -> JSX
sup = element (unsafeCreateDOMComponent "sup")

sup_ :: Array JSX -> JSX
sup_ children = sup { children }

type Props_svg =
  ( accentHeight :: String
  , accumulate :: String
  , additive :: String
  , alignmentBaseline :: String
  , allowReorder :: String
  , alphabetic :: String
  , amplitude :: String
  , arabicForm :: String
  , ascent :: String
  , attributeName :: String
  , attributeType :: String
  , autoReverse :: String
  , azimuth :: String
  , baseFrequency :: String
  , baseProfile :: String
  , baselineShift :: String
  , bbox :: String
  , begin :: String
  , bias :: String
  , by :: String
  , calcMode :: String
  , capHeight :: String
  , children :: Array JSX
  , clip :: String
  , clipPath :: String
  , clipPathUnits :: String
  , clipRule :: String
  , color :: String
  , colorInterpolation :: String
  , colorInterpolationFilters :: String
  , colorProfile :: String
  , colorRendering :: String
  , contentScriptType :: String
  , contentStyleType :: String
  , cursor :: String
  , cx :: String
  , cy :: String
  , d :: String
  , decelerate :: String
  , descent :: String
  , diffuseConstant :: String
  , direction :: String
  , display :: String
  , divisor :: String
  , dominantBaseline :: String
  , dur :: String
  , dx :: String
  , dy :: String
  , edgeMode :: String
  , elevation :: String
  , enableBackground :: String
  , end :: String
  , exponent :: String
  , externalResourcesRequired :: String
  , fill :: String
  , fillOpacity :: String
  , fillRule :: String
  , filter :: String
  , filterRes :: String
  , filterUnits :: String
  , floodColor :: String
  , floodOpacity :: String
  , focusable :: String
  , fontFamily :: String
  , fontSize :: String
  , fontSizeAdjust :: String
  , fontStretch :: String
  , fontStyle :: String
  , fontVariant :: String
  , fontWeight :: String
  , format :: String
  , from :: String
  , fx :: String
  , fy :: String
  , g1 :: String
  , g2 :: String
  , glyphName :: String
  , glyphOrientationHorizontal :: String
  , glyphOrientationVertical :: String
  , glyphRef :: String
  , gradientTransform :: String
  , gradientUnits :: String
  , hanging :: String
  , height :: String
  , horizAdvX :: String
  , horizOriginX :: String
  , ideographic :: String
  , imageRendering :: String
  , in :: String
  , in2 :: String
  , intercept :: String
  , k :: String
  , k1 :: String
  , k2 :: String
  , k3 :: String
  , k4 :: String
  , kernelMatrix :: String
  , kernelUnitLength :: String
  , kerning :: String
  , keyPoints :: String
  , keySplines :: String
  , keyTimes :: String
  , lengthAdjust :: String
  , letterSpacing :: String
  , lightingColor :: String
  , limitingConeAngle :: String
  , local :: String
  , markerEnd :: String
  , markerHeight :: String
  , markerMid :: String
  , markerStart :: String
  , markerUnits :: String
  , markerWidth :: String
  , mask :: String
  , maskContentUnits :: String
  , maskUnits :: String
  , mathematical :: String
  , mode :: String
  , numOctaves :: String
  , offset :: String
  , opacity :: String
  , operator :: String
  , order :: String
  , orient :: String
  , orientation :: String
  , origin :: String
  , overflow :: String
  , overlinePosition :: String
  , overlineThickness :: String
  , paintOrder :: String
  , panose1 :: String
  , pathLength :: String
  , patternContentUnits :: String
  , patternTransform :: String
  , patternUnits :: String
  , pointerEvents :: String
  , points :: String
  , pointsAtX :: String
  , pointsAtY :: String
  , pointsAtZ :: String
  , preserveAlpha :: String
  , preserveAspectRatio :: String
  , primitiveUnits :: String
  , r :: String
  , radius :: String
  , refX :: String
  , refY :: String
  , renderingIntent :: String
  , repeatCount :: String
  , repeatDur :: String
  , requiredExtensions :: String
  , requiredFeatures :: String
  , restart :: String
  , result :: String
  , rotate :: String
  , rx :: String
  , ry :: String
  , scale :: String
  , seed :: String
  , shapeRendering :: String
  , slope :: String
  , spacing :: String
  , specularConstant :: String
  , specularExponent :: String
  , speed :: String
  , spreadMethod :: String
  , startOffset :: String
  , stdDeviation :: String
  , stemh :: String
  , stemv :: String
  , stitchTiles :: String
  , stopColor :: String
  , stopOpacity :: String
  , strikethroughPosition :: String
  , strikethroughThickness :: String
  , string :: String
  , stroke :: String
  , strokeDasharray :: String
  , strokeDashoffset :: String
  , strokeLinecap :: String
  , strokeLinejoin :: String
  , strokeMiterlimit :: String
  , strokeOpacity :: String
  , strokeWidth :: String
  , surfaceScale :: String
  , systemLanguage :: String
  , tableValues :: String
  , targetX :: String
  , targetY :: String
  , textAnchor :: String
  , textDecoration :: String
  , textLength :: String
  , textRendering :: String
  , to :: String
  , transform :: String
  , u1 :: String
  , u2 :: String
  , underlinePosition :: String
  , underlineThickness :: String
  , unicode :: String
  , unicodeBidi :: String
  , unicodeRange :: String
  , unitsPerEm :: String
  , vAlphabetic :: String
  , vHanging :: String
  , vIdeographic :: String
  , vMathematical :: String
  , values :: String
  , vectorEffect :: String
  , version :: String
  , vertAdvY :: String
  , vertOriginX :: String
  , vertOriginY :: String
  , viewBox :: String
  , viewTarget :: String
  , visibility :: String
  , width :: String
  , widths :: String
  , wordSpacing :: String
  , writingMode :: String
  , x :: String
  , x1 :: String
  , x2 :: String
  , xChannelSelector :: String
  , xHeight :: String
  , xlinkActuate :: String
  , xlinkArcrole :: String
  , xlinkHref :: String
  , xlinkRole :: String
  , xlinkShow :: String
  , xlinkTitle :: String
  , xlinkType :: String
  , xmlBase :: String
  , xmlLang :: String
  , xmlSpace :: String
  , xmlns :: String
  , xmlnsXlink :: String
  , y :: String
  , y1 :: String
  , y2 :: String
  , yChannelSelector :: String
  , z :: String
  , zoomAndPan :: String
  )

svg
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_svg)
  => Record attrs
  -> JSX
svg = element (unsafeCreateDOMComponent "svg")

svg_ :: Array JSX -> JSX
svg_ children = svg { children }

type Props_table =
  ( children :: Array JSX
  , summary :: String
  , width :: String
  )

table
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_table)
  => Record attrs
  -> JSX
table = element (unsafeCreateDOMComponent "table")

table_ :: Array JSX -> JSX
table_ children = table { children }

type Props_tbody =
  ( children :: Array JSX
  )

tbody
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_tbody)
  => Record attrs
  -> JSX
tbody = element (unsafeCreateDOMComponent "tbody")

tbody_ :: Array JSX -> JSX
tbody_ children = tbody { children }

type Props_td =
  ( children :: Array JSX
  , headers :: String
  , height :: String
  , scope :: String
  , width :: String
  )

td
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_td)
  => Record attrs
  -> JSX
td = element (unsafeCreateDOMComponent "td")

td_ :: Array JSX -> JSX
td_ children = td { children }

type Props_template =
  ( children :: Array JSX
  )

template
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_template)
  => Record attrs
  -> JSX
template = element (unsafeCreateDOMComponent "template")

template_ :: Array JSX -> JSX
template_ children = template { children }

type Props_textarea =
  ( autoCapitalize :: String
  , autoCorrect :: String
  , children :: Array JSX
  , cols :: Number
  , defaultValue :: String
  , disabled :: Boolean
  , form :: String
  , name :: String
  , onChange :: EventHandler
  , placeholder :: String
  , required :: Boolean
  , rows :: Number
  , value :: String
  , wrap :: String
  )

textarea
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_textarea)
  => Record attrs
  -> JSX
textarea = element (unsafeCreateDOMComponent "textarea")

textarea_ :: Array JSX -> JSX
textarea_ children = textarea { children }

type Props_tfoot =
  ( children :: Array JSX
  )

tfoot
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_tfoot)
  => Record attrs
  -> JSX
tfoot = element (unsafeCreateDOMComponent "tfoot")

tfoot_ :: Array JSX -> JSX
tfoot_ children = tfoot { children }

type Props_th =
  ( children :: Array JSX
  , headers :: String
  , height :: String
  , scope :: String
  , width :: String
  )

th
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_th)
  => Record attrs
  -> JSX
th = element (unsafeCreateDOMComponent "th")

th_ :: Array JSX -> JSX
th_ children = th { children }

type Props_thead =
  ( children :: Array JSX
  )

thead
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_thead)
  => Record attrs
  -> JSX
thead = element (unsafeCreateDOMComponent "thead")

thead_ :: Array JSX -> JSX
thead_ children = thead { children }

type Props_time =
  ( children :: Array JSX
  )

time
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_time)
  => Record attrs
  -> JSX
time = element (unsafeCreateDOMComponent "time")

time_ :: Array JSX -> JSX
time_ children = time { children }

type Props_title =
  ( children :: Array JSX
  )

title
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_title)
  => Record attrs
  -> JSX
title = element (unsafeCreateDOMComponent "title")

title_ :: Array JSX -> JSX
title_ children = title { children }

type Props_tr =
  ( children :: Array JSX
  )

tr
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_tr)
  => Record attrs
  -> JSX
tr = element (unsafeCreateDOMComponent "tr")

tr_ :: Array JSX -> JSX
tr_ children = tr { children }

type Props_track =
  ( default :: Boolean
  , kind :: String
  , label :: String
  , src :: String
  )

track
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_track)
  => Record attrs
  -> JSX
track = element (unsafeCreateDOMComponent "track")

type Props_u =
  ( children :: Array JSX
  )

u
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_u)
  => Record attrs
  -> JSX
u = element (unsafeCreateDOMComponent "u")

u_ :: Array JSX -> JSX
u_ children = u { children }

type Props_ul =
  ( children :: Array JSX
  , type :: String
  )

ul
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_ul)
  => Record attrs
  -> JSX
ul = element (unsafeCreateDOMComponent "ul")

ul_ :: Array JSX -> JSX
ul_ children = ul { children }

type Props_var =
  ( children :: Array JSX
  )

var
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_var)
  => Record attrs
  -> JSX
var = element (unsafeCreateDOMComponent "var")

var_ :: Array JSX -> JSX
var_ children = var { children }

type Props_video =
  ( children :: Array JSX
  , controls :: Boolean
  , height :: String
  , loop :: Boolean
  , muted :: Boolean
  , playsInline :: Boolean
  , poster :: String
  , preload :: String
  , src :: String
  , width :: String
  )

video
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_video)
  => Record attrs
  -> JSX
video = element (unsafeCreateDOMComponent "video")

video_ :: Array JSX -> JSX
video_ children = video { children }

type Props_wbr =()

wbr
  :: forall attrs attrs_
   . Union attrs attrs_ (SharedProps Props_wbr)
  => Record attrs
  -> JSX
wbr = element (unsafeCreateDOMComponent "wbr")
