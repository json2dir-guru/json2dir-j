NB. json2dir: create the directory tree a JSON document describes, in the current directory.
NB. Implements RFC J2D-1. J base library only: the JSON parser is written here,
NB. file system access goes through J foreigns and 15!:0 (cd) calls into libc.
NB. Usage: jconsole json2dir.ijs < document.json

LIBC =: 'libc.so.6 '
NUL =: {. a.
WS =: ' ',TAB,LF,CR

MSG =: ''
die =: 3 : 0
  MSG =: y
  13!:8 ] 3
)

NB. ---------------------------------------------------------------- UTF-8 (array style)
NB. every non-continuation byte is a lead; its length must reach exactly the next lead,
NB. and the second byte of E0/ED/F0/F4 sequences is range-checked (overlongs, surrogates, > U+10FFFF).
utf8ok =: 3 : 0
  b =. a. i. y
  if. 0 = # b do. 1 return. end.
  if. +./ (b > 244) +. b e. 192 193 do. 0 return. end.
  cont =. (b >: 128) *. b < 192
  lead =. I. -. cont
  n =. lead { 1 + (b >: 192) + (b >: 224) + b >: 240
  if. -. (lead , #b) -: 0 , lead + n do. 0 return. end.
  l =. lead { b
  s =. (lead + 1) { b , 0
  -. +./ ((l = 224) *. s < 160) +. ((l = 237) *. s > 159) +. ((l = 240) *. s < 144) +. (l = 244) *. s > 143
)

NB. ---------------------------------------------------------------- JSON parser
NB. S: input, P: position, NW: positions of non-whitespace, SP: positions of '"', '\' and
NB. control characters (both with sentinel #S), so scanning is a binary search, not a loop.
peek =: 3 : '(P <. #S) { S , NUL'
ws =: 3 : 'P =: NW {~ NW I. P'
expect =: 4 : 0
  ws ''
  if. x ~: peek '' do. die y end.
  P =: P + 1
)

HEXD =: '0123456789abcdefABCDEF'
hex4 =: 3 : 0
  h =. (P + i. 4) { S , 4 # NUL
  if. -. *./ h e. HEXD do. die 'invalid \u escape' end.
  P =: P + 4
  16 #. ((i. 16) , 10 + i. 6) {~ HEXD i. h
)

uesc =: 3 : 0
  u =. hex4 ''
  if. (u >: 16bdc00) *. u <: 16bdfff do. die 'string contains an unpaired surrogate' end.
  if. (u >: 16bd800) *. u <: 16bdbff do.
    if. -. '\u' -: (P + 0 1) { S , NUL , NUL do. die 'string contains an unpaired surrogate' end.
    P =: P + 2
    l =. hex4 ''
    if. -. (l >: 16bdc00) *. l <: 16bdfff do. die 'string contains an unpaired surrogate' end.
    u =. 16b10000 + (1024 * u - 16bd800) + l - 16bdc00
  end.
  8 u: 10 u: u
)

str =: 3 : 0
  P =: P + 1
  r =. ''
  while. 1 do.
    k =. SP {~ SP I. P
    if. k = # S do. die 'unterminated string' end.
    r =. r , (P + i. k - P) { S
    c =. k { S
    P =: k + 1
    if. c = '"' do. break. end.
    if. c ~: '\' do. die 'control character in string' end.
    c =. peek ''
    P =: P + 1
    if. c e. '"\/' do. r =. r , c
    elseif. c e. 'bfnrt' do. r =. r , (8 12 10 13 9 { a.) {~ 'bfnrt' i. c
    elseif. c = 'u' do. r =. r , uesc ''
    else. die 'invalid escape in string' end.
  end.
  r
)

checkname =: 3 : 0
  if. (0 = # y) +. ((<y) e. '.';'..') +. +./ y e. '/',NUL do. die 'invalid name "',y,'"' end.
  y
)

ARRERR =: 'an array must be ["link", target] or ["script", content]'
array =: 3 : 0
  P =: P + 1
  '"' expect ARRERR
  P =: P - 1
  k =. str ''
  ',' expect ARRERR
  '"' expect ARRERR
  P =: P - 1
  t =. str ''
  ']' expect ARRERR
  if. -. (<k) e. 'link';'script' do. die 'unknown array kind "',k,'"' end.
  if. (k -: 'link') *. NUL e. t do. die 'a link target cannot contain NUL' end.
  ((k -: 'script') { 'lx') ; t
)

NB. a value is  type ; payload  with type 'o' (payload names;values), 'f' file, 'l' link, 'x' script
value =: 3 : 0
  ws ''
  c =. peek ''
  if. c = '{' do. object ''
  elseif. c = '"' do. 'f' ; str ''
  elseif. c = '[' do. array ''
  else. die 'invalid JSON, or a number/true/false/null value (not allowed)' end.
)

object =: 3 : 0
  P =: P + 1
  names =. vals =. 0 $ < ''
  ws ''
  if. '}' = peek '' do. P =: P + 1
  else.
    while. 1 do.
      ws ''
      if. '"' ~: peek '' do. die 'expected a member name' end.
      nm =. checkname str ''
      ':' expect 'expected ":"'
      names =. names , < nm
      vals =. vals , < value ''
      ws ''
      c =. peek ''
      P =: P + 1
      if. c = '}' do. break. end.
      if. c ~: ',' do. die 'expected "," or "}"' end.
    end.
  end.
  'o' ; < names ; < vals
)

parse =: 3 : 0
  if. 239 187 191 -: a. i. 3 {. y do. y =. 3 }. y end.
  if. -. utf8ok y do. die 'input is not valid UTF-8' end.
  S =: y
  P =: 0
  NW =: (I. -. S e. WS) , # S
  SP =: (I. (S e. '"\') +. 32 > a. i. S) , # S
  ws ''
  if. '{' ~: peek '' do. die 'the root of the document must be an object' end.
  r =. object ''
  ws ''
  if. P < # S do. die 'trailing data after the JSON value' end.
  r
)

NB. ---------------------------------------------------------------- file system
NB. kind of an existing entry, without following symlinks: 0 none, 1 directory, 2 other
lkind =: 3 : 0
  r =. (LIBC , 'lstat i *c *c') cd (y , NUL) ; 144 # NUL
  if. 0 ~: > {. r do. 0 return. end.
  mode =. {. _2 ic 4 {. 24 }. > {: r
  2 - 16b4000 = 16bf000 (17 b.) mode
)

remove =: 3 : 0
  if. 0 ~: > {. (LIBC , 'unlink i *c') cd < y , NUL do. die y , ': cannot remove' end.
)

OFLAGS =: 1 + 64 + 128 + 131072   NB. O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW

writefile =: 3 : 0
  'p c x' =. y
  fd =. > {. (LIBC , 'open i *c i i') cd (p , NUL) ; OFLAGS ; 8b666
  if. fd < 0 do. die p , ': cannot create file' end.
  while. # c do.
    n =. > {. (LIBC , 'write x i *c x') cd fd ; c ; # c
    if. n <: 0 do. die p , ': write failed' end.
    c =. n }. c
  end.
  if. x do.
    um =. > {. (LIBC , 'umask i i') cd < 0
    (LIBC , 'umask i i') cd < um
    m =. 8b111 (23 b.) 8b666 (17 b.) 8b777 - um
    if. 0 ~: > {. (LIBC , 'fchmod i i i') cd fd ; m do. die p , ': chmod failed' end.
  end.
  (LIBC , 'close i i') cd < fd
)

NB. x: path prefix ('' or 'dir/'), y: names ; values. Last duplicate wins; byte order.
apply =: 4 : 0
  'names vals' =. y
  u =. ~. names
  last =. names i: u
  for_i. /: u do.
    p =. x , > i { u
    v =. > (i { last) { vals
    t =. > {. v
    k =. lkind p
    if. t = 'o' do.
      if. k ~: 1 do.
        if. k = 2 do. remove p end.
        1!:5 < p
      end.
      (p , '/') apply > {: v
    else.
      if. k = 1 do. die p , ': a directory is in the way' end.
      if. k = 2 do. remove p end.
      if. t = 'l' do.
        if. 0 ~: > {. (LIBC , 'symlink i *c *c') cd ((> {: v) , NUL) ; p , NUL do. die p , ': cannot create symlink' end.
      else.
        writefile p ; (> {: v) ; t = 'x'
      end.
    end.
  end.
  i. 0 0
)

main =: 3 : 0
  if. 2 < # ARGV do.
    ('usage: json2dir < document.json' , LF) 1!:2 ] 5
    2!:55 ] 2
  end.
  try.
    '' apply > {: parse 1!:1 ] 3
  catch.
    if. 0 = # MSG do. MSG =: LF -.~ 13!:12 '' end.
    ('json2dir: ' , MSG , LF) 1!:2 ] 5
    2!:55 ] 1
  end.
  2!:55 ] 0
)

main ''
