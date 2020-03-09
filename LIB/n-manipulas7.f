\ фиксированно-именованные локальные переменные на стеке параметров

\ положить адрес выбранного самого нижнего параметра на стеке в регистр ESI 
: sset \ offset --
 0xF58B W,            \ MOV     ESI , EBP
 0xC683 W, C,         \ ADD     ESI , # offset
; 
\  положить ESI на стек возвратов
M: rs   RS=S          \ PUSH    ESI
;
\  снять со стека возвратов и положить в ESI
M: sr   S=RS          \ POP     ESI
;
\ положить выбранный параметр на вершину стека
: N> \ n --
0x4589 W, 0xFC C,     \ MOV     FC [EBP] , EAX
0x6D8D W, 0xFC C,     \ LEA     EBP , FC [EBP]
0x468B W, C,          \ MOV     EAX ,  n [ESI] 
;
 
\ положить выбранный параметр в EDX
: d=@s 
  0x568B W, C,        \ MOV     EDX ,  n [ESI]
;
\ положить параметр из EDX в выбранную ячейку стека
: @s=d 
  0x5689 W, C,        \ MOV     n [ESI] ,  EDX   
;
\ снять параметр с вершины стека в выбранную ячейку стека
: @s=a  \ n --
  0x4689 W, C, 
  0x458B W, 0x00 C, 
  0x6D8D W, 0x4 C, 
;
: s@s=a \ n --
  0x4689 W, C, 
; 
\ положить выбранный параметр на вершину стека ( на вершину стека)
: a=@s  0x468B W, C,  \ MOV     EAX , n [ESI] 
;
: o0x0 
 0x6D8D W, C,         \ LEA     EBP , n [EBP] 
 0x458B W, -4 C,      \ MOV     FC [EBP] , EAX
; 
: o1x1 
 0x6D8D W, C,         \ LEA     EBP , n [EBP] 
; 
: o2x2 
 0x5D8B W, 0x00 C,    \ MOV     EBX , 0 [EBP]
 0x6D8D W, C,         \ LEA     EBP , n [EBP]
 0x5D89 W, 0x00 C,    \ MOV     0 [EBP] , EBX
; 
: o3x3 
 0x5D8B W, 0x00 C,    \ MOV     EBX , 0 [EBP]
 0x4D8B W, 0x04 C,    \ MOV     ECX , 4 [EBP]
 0x6D8D W, C,         \ LEA     EBP , n [EBP]
 0x5D89 W, 0x00 C,    \ MOV     0 [EBP] , EBX
 0x4D89 W, 0x04 C,    \ MOV     4 [EBP] , ECX
; 
: o4x4 
 0x5D8B W, 0x00 C,    \ MOV     EBX , 0 [EBP]
 0x4D8B W, 0x04 C,    \ MOV     ECX , 4 [EBP]
 0x558B W, 0x08 C,    \ MOV     EDX , 8 [EBP]
 0x6D8D W, C,         \ LEA     EBP , n [EBP]
 0x5D89 W, 0x00 C,    \ MOV     0 [EBP] , EBX
 0x4D89 W, 0x04 C,    \ MOV     4 [EBP] , ECX
 0x5589 W, 0x08 C,    \ MOV     8 [EBP] , EDX
; 
: n&  0x4623 W, C, ; 
: n^  0x4633 W, C, ; 
: n|  0x460B W, C, ; 
: n+  0x4603 W, C, ; 
: n-  0x462B W, C, ; 
: n=  0x4633 W, C, 
      0xE883 W, 1 C, 
      0xC01B W, 
; 
: n#  0x4633 W, C, 
      0xD8F7 W, 
      0xC01B W, ;
: n>  0x463B W, C,
      0x9D0F W, 0xC0 C,
      0xE083 W, 1 C,
      0x48   C,
      0x6D8D W, 4 C,
; 
: n<  0x463B W,      C,
      0x9E0F W, 0xC0 C,
      0xE083 W, 1 C,
      0x48   C,
      0x6D8D W, 4 C,
;
: n*  0x6EF7 W, C,
      0x6D8D W, 4 C,
;
: n/  0xC88B W,
      0x463B W, C,
      0x99   C,
      0xF9F7 W,
      0x6D8D W, 4 C,
;
: n%  0xC88B W,
      0x463B W, C,
      0x99   C,
      0xF9F7 W,
      0x6D8D W, 4 C,
      0xC28B W,
;
: nM  0x463B W, 0 C,
      0xE37C W,     
      0x6D8D W, 4 C,
;
: nm  0x463B W, 0 C,
      0xC77F W,
      0x6D8D W, 4 C,
;

\ стековые манипуляторы, расширенные операторами (для быстрого прототипирования и отладки программных модулей)

: SPDROP ( p*n n --)  P+A DROP ;  \ убрать со стека n байтов
: SPMOVE ( p*n addr n --) $ 4 B=aP D=A D+@P L1: $ -4 Da C=@B @D=C $ 4 Ba $ 4 A-# L1 J0<> 2DROP ; \ скопировать n байтов стека в память

M: aDO   OVER + SWAP ?DO ;    \ макрос ( BOUND ?DO )
M: I+  ( n -- )  R@ + RP@ ! ; \ макрос: увеличить счетчик цикла на n со стека

: odin  1 ;
: hex   16 ;
: 4cell 32 ;
: kbyte 1024 ;
: mbyte 1048576 ;

: hAlloc  RS=S ALLOCATE THROW S=RS ;
: hFree   RS=S     FREE THROW S=RS ;
: hResize RS=S   RESIZE THROW S=RS ;

I: LAMBDA{ ( -- res xt ) 0 BRANCH, >MARK HERE ;
I: }LAMBDA ( res xt -- ) RET, >R >RESOLVE1 R> LIT, ;

I: FO ` OF ` ` ;  M: EF ENDOF ;

: conv-so   ui! ai! ai i!
  2000 ALLOCATE THROW ao! 2000 uo! ao o! ao uo ERASE
  i+( i 1+ is i )  s( o C! o 1+ is o )  
  BEGIN ''' s  i C@ i+ s i+ ''' s BL s 'F' s 'O' s BL s
        BEGIN i C@ BL > IF i C@ i+ s 0 ELSE i+ BL s 'E' s 'F' s BL s 1 THEN UNTIL
  i ai - ui >
  UNTIL ao o ao -
;
: Move RS=S MOVE S=RS ;
: Search RS=S SEARCH S=RS ;
: ).(  RS=S . S=RS ;
: Type RS=S TYPE S=RS ;
: Emit RS=S EMIT S=RS ;
: Cr RS=S CR S=RS ;
: Compare RS=S COMPARE S=RS ;

: S:  : IMMEDIATE load-str conv-so DLIT, ` EVALUATE ` ; ;
S: SYM->OPERATOR 
a ABS                      ! !          0 FALSE
b C@                       _ FILL       
c Emit                     $ ?DO        
d DUP                      % MOD        
e ELSE                     & AND        
f hFree                             
g CELL       H WITHIN               
h hAlloc     G aDO         * *          
i 1+         I I           + +          
j 1+!        J J           , LITERAL    
k KEY        K EKEY        - -
l LSHIFT     L LOOP        . ).(
m MIN        M MAX         / /
n NEGATE     N +LOOP       < <
o AGAIN      O BEGIN       = =
p DS>F       P DEPTH       > >
q Compare    Q LEAVE       ? IF
r RSHIFT     R REPEAT      @ @
s Search     S SPACES      # <>        
t THEN       T Type               
u hResize    U UNTIL       ^ XOR
v EVALUATE   V Move        
w C!         W WHILE       { LAMBDA{
x DROP       X EXECUTE     | OR
y odin       Y TRUE        } }LAMBDA
z 0<>        Z 0=          ~ INVERT                                                                  
;                   
M: XTSYM  ` LAMBDA{ I 1+ C@ CASE SYM->OPERATOR ENDCASE  ` }LAMBDA 1 I+  
;

: DOR     ( d1 d2 -- d3)  D=@P $ 8 Pa $ -4 A|@P @P|D ;
: DXOR    ( d1 d2 -- d3)  D=@P $ 8 Pa $ -4 A^@P @P^D ;
: DAND    ( d1 d2 -- d3)  D=@P $ 8 Pa $ -4 A&@P @P&D ;
: DINVERT ( d1 -- d2 )    @P~ A~ ;
: DLSHIFT ( d n -- 'd ) $ 4 D=@P $ 20 C=# C-A D>> C=A @P<< $ 4 @P<< @P|D DROP ;
: DRSHIFT ( d n -- 'd ) D=@P $ 20 C=# C-A D<< C=A @P>> $ 4 @P>> $ 4 @P|D DROP ;
: D0<>    ( d -- f ) D0= INVERT ;
: 1-! ( addr --)   @A-- DROP ;
: Dodin 1. ;
: Dnull 0. ;
: n.0b ( n -- ) 2 BASE ! .0 DECIMAL ;
: n.0h ( n -- ) HEX .0 DECIMAL ;
: .BL ( n -- ) >R 0 <# #S #> R> OVER - 0 MAX DUP IF 0 DO BL EMIT LOOP ELSE DROP THEN TYPE ;
: sqrt ( n -- sqrt )  $ -4 @P=A $ -4 0=@P 0SQRT $ -4 @P=0- $ -4 A=@P ;

\ символы с префиксом " 
S: SYM->DOPERATOR 
a DABS       Z D0=         ! 2!         0 Dnull
b W@                       & DAND        
d 2DUP                     + D+          
g 4cell                    , 2LITERAL    
l DLSHIFT                  - D-         
n DNEGATE                  . D.         
r DRSHIFT                  < D<         
w W!                       = D=         
x 2DROP                    > D>         
y Dodin                    @ 2@         
z D0<>                     ^ DXOR
                           | DOR
                           ~ DINVERT
; 
USER-VALUE XNST 0 TO XNST
: NOTFOUND { a u }
  z19( '1' ':' WITHIN )
  zAF( 'A' 'G' WITHIN )
  PDS( z19 IF '0' ELSE '7' THEN - )
  offs( 1 - -4 * )
  s1[ I    C@ ]
  s2[ I 1+ C@ ]
  s3[ I 2+ C@ ]
  p1?[ s1` z19 s1` zAF OR ]
  p2?[ s2` z19 s2` zAF OR ]
  p3?[ s3` z19 s3` zAF OR ]
  pp?[ p1?` p2?` AND 1 AND ]
  pu?[ p1?` '[' s2` = AND 2 AND ]
  up?[ ']' s1` = p2?` AND 4 AND ]
  po?[ p1?` '+' s2` = '-' s2` = OR '*' s2` = OR '/' s2` = OR '%' s2` = OR '=' s2` = OR
            '#' s2` = OR '<' s2` = OR '>' s2` = OR 'M' s2` = OR 'm' s2` = OR AND 8 AND ]
\  po?[ p1?` 0 I 1+ C@ TO s S" +-*/%=><#Mm" aDO I C@ s = IF DROP TRUE LEAVE THEN LOOP AND 8 AND ]
  sp?[ '`' s1` = p2?` AND 16 AND ]
  so?[ ''' s1` = '"' s1` = OR 32 AND ]
  p:p?[ p1?` ':' s2` = AND p3?` AND 64 AND ]
  o:p?[ ':' s1` = p2?` AND 128 AND ]
  a  C@ '/' = u 1 > AND 0= IF a u NOTFOUND EXIT THEN
  a u + a 1+ ?DO pp?` pu?` OR up?` OR po?` OR sp?` OR so?` OR p:p?` OR o:p?` OR
(                 CASE
                 1 OF s1` DUP DUP PDS offs N> XNST > IF TO XNST ELSE DROP THEN  1 I+ ENDOF

                 2 OF s1` DUP PDS TO XNST XNST 2- 4 * sset 1 I+                ENDOF
                 4 OF XNST 4 * s2`
                      CASE
                       '0' OF o0x0 ENDOF
                       '1' OF o1x1 ENDOF
                       '2' OF o2x2 ENDOF
                       '3' OF o3x3 ENDOF
                       '4' OF o4x4 ENDOF
                      ENDCASE 1 I+ 0 TO XNST ENDOF
                 8 OF s2` DUP PDS offs s2`
                      CASE
                       '^' OF n^  ENDOF
                       '&' OF n&  ENDOF
                       '|' OF n|  ENDOF
                       '+' OF n+  ENDOF
                       '-' OF n-  ENDOF
                       '=' OF n=  ENDOF
                       '#' OF n#  ENDOF
                       '>' OF n>  ENDOF
                       '<' OF n<  ENDOF
                       '*' OF n*  ENDOF
                       '/' OF n/  ENDOF
                       '%' OF n%  ENDOF
                       'M' OF nM  ENDOF
                       'm' OF nm  ENDOF
                      ENDCASE           1 I+ ENDOF
                16 OF s1` DUP PDS LIT,  1 I+ ENDOF
                32 OF s1` DUP ''' = IF XTSYM ELSE SYM->DOPERATOR THEN 1 I+ ENDOF
                64 OF s1` DUP PDS XNST < s3` DUP PDS XNST < AND
                      IF   s1`  DUP PDS offs d=@s s3` DUP PDS offs @s=d
                      ELSE s1`  DUP PDS XNST =
                           IF   s3` DUP PDS offs s@s=a
                           ELSE s1` DUP PDS offs a=@s
                           THEN
                      THEN 2 I+ ENDOF
               128 OF s3` DUP PDS offs @s=a 1 I+ ENDOF
\                   I C@ CASE SYM->OPERATOR  ';' FO EXIT EF '\' FO Cr EF ENDCASE

               ENDCASE )
             LOOP 
;
\EOF
\ работа с числами с плав. точкой - односимвольный вариант в отдельном словаре ( n| ) 

S: S>FOPERATOR
+ F+         - F-         * F*      / F/      a FABS    n FNEGATE i F1+            \ арифметика    
| OR         & AND        ^ XOR     ~ INVERT                                       \ логика
d FDUP       x FDROP      P FDEPTH                                                 \ стек          
< F<         = F=         m FMIN    M FMAX    z F0<     Z F0=     H WITHIN         \ сравнения     
0 FALSE      y odin       Y TRUE                                                   \ константы     
@ F@         ! F!         j 1+!     h hAlloc  f hFree   u hResize V MOVE    # FILL \ память
c FCOS       s FSIN       S FSQRT   T FTAN                                         \ функции 
? IF         e ELSE       t THEN    [ CASE    ( OF      ) ENDOF   ] ENDCASE        \ ветвление
O BEGIN      o AGAIN      U UNTIL   W WHILE   R REPEAT                             \ циклы
$ ?DO        G aDO        I I       J J       Q LEAVE   L LOOP    N +LOOP          \ циклы со счетчиком
p DATA>FLOAT q FLOAT>DATA % DS>F                                                   \ конверсия в числа с плав. точкой
_ ACCEPT     k KEY        K EKEY                                                   \ ввод
. F.         g CELL                                                                \ вывод         
, FLITERAL   v EVALUATE   { LAMBDA{ } }LAMBDA X EXECUTE r RET,                     \ компиляция-исполнение
;
: NOTFOUND ( a u -- ) u! a! XSF sf!
  z18?( C@ '1' '9' WITHIN )
  z19?( C@ '1' ':' WITHIN )
  zAF?( C@ 'A' 'G' WITHIN )
  a z18?                    fz1!
  a    C@ '|'        =      fr1!
  a 1+ C@ '|'        =      fr2!
  fz1 fr2 AND fr1 OR u 1 > AND 0= IF a u NOTFOUND EXIT THEN
  fr1 XSF 0= AND  fz1 fr2 AND OR
  IF  128 ALLOCATE THROW is sf sf TO XSF THEN
  fz1 IF a C@ '0' - DUP 
         >R 0 DO `  FLOAT>DATA LOOP  
         R> 0 DO I 2* CELLS LIT, sf LIT, ` + ` 2! LOOP 
      THEN
  a u + a fz1 IF 2+ ELSE 1+ THEN
  ?DO I z19? I zAF? OR
      IF   I C@  I z19? IF '0' ELSE '7' THEN -  1- 2* CELLS LIT, sf LIT, ` + ` 2@ ` DATA>FLOAT  
      ELSE I C@ ':' =
           IF ` FLOAT>DATA  I 1+ C@ I 1+ z19? IF '0' ELSE '7' THEN - 1- 2* CELLS LIT,  sf LIT, ` + ` 2! 1 I+  
           ELSE I C@ '`' = I 1+ z19? I 1+ zAF? OR AND
                IF I 1+ C@ I 1+ z19? IF '0' ELSE '7' THEN - DS>F ` FLITERAL  1 I+                                                     
                ELSE I C@ ''' = I 1+ z19?  I 1+ zAF? OR AND                                  
                     IF   I 1+ C@ I 1+ z19? IF '0' ELSE '7' THEN - 1- CELLS LIT, sf LIT, ` + 1 I+                                                                           
                     ELSE I C@ CASE S>FOPERATOR '\' FO CR EF ';' FO EXIT EF ENDCASE                                                                    
                     THEN
                THEN 
           THEN      
      THEN           
  LOOP               
;
\EOF 
\ как пример использования - манипуляторный вариант конвертора чисел с плавающей запятой из формы 32,123 в форму 32.123е
\ числа с плавающей запятой в виде 3,1415 
\             1 2       3   4   5   6   7   8    9  A  B  C 
: NOTFOUND  ( a u -- ) '0' ':' ',' '.' 'e' '-' ( sq sz pt an ) 
  8/0:90:A0:B0:C12GIb34H?'9jtIb5=?I1-:B'AjtL
  /1b8=?92`i`i=e92`i=tAy=&Z?12 NOTFOUND /;t
  /2iih:C1C2V7C2+w6CB+w0C2i+wC2iv
; 

\ Символы     Символы с префиксами " `         

a ABS         "a DABS         `a FABS        'S  - XT слова обозначенного символом S
b C@          "b W@           `b .BL         '1..'9 'A..'F - адреса ячеек буфера, куда сбрасываются со стека параметры    
c EMIT        "d 2DUP         `c FCOS                        и откуда они кладутся(копируются) на стек
d DUP         "g 4cell        `d FDUP        1..9 A..F - содержимое ячеек буфера
e ELSE        "l DLSHIFT      `f            `1..`9 `A..`F - значения 1..9 A..F
f hFree       "n DNEGATE      `h hex    
g CELL        "r DRSHIFT      `i 1-     
h hAlloc      "w W!           `j 1-!    
i 1+          "x 2DROP        `k kbyte  
j 1+!         "y Dodin        `K mbyte  
k KEY         "z D0<>         `m FMIN   
l LSHIFT      "D              `n FNEGATE
m MIN         "F              `p FPI    
n NEGATE      "X              `q sqrt                                  
o AGAIN       "Z D0=          `Q FSQRT  
p DP          "+ D+           `s FSIN   
q COMPARE     "- D-           `t n.0b   
r RSHIFT      "< D<           `T n.0h   
s SEARCH      "= D=           `x FDROP  
t THEN        "> D>           `z F0<    
u hResize     "@ 2@           `I F1+    
v EVALUATE    "! 2!           `P .0     
w C!          "^ DXOR         `M FMAX   
x DROP        "| DOR          `Z F0=    
y odin        "& DAND         `+ F+     
z 0<>         "~ DINVERT      `- F-     
G aDO         ", 2LITERAL     `* F*     
H WITHIN      ". D.           `/ F/     
I I           "0 Dnull        `< F<     
J J           "h ALLOT        `= F=     
K EKEY                        `@ F@     
L LOOP                        `! F!     
M MAX                         `. F.     
N +LOOP                       `; RET,   
O BEGIN                       `r R>   
P DEPTH                       `R >R
Q LEAVE                       `X R@
R REPEAT 
S SPACES 
T TYPE   
U UNTIL  
V MOVE   
W WHILE  
X EXECUTE
Y TRUE   
Z 0=     
! !      
# FILL   
$ ?DO    
% MOD    
& AND    
( OF     
) ENDOF  
* *      
+ +      
, LITERAL
- -      
. .      
/ /      
; EXIT   
< <      
= =      
> >      
? IF     
@ @      
[ CASE   
\ CR     
] ENDCASE
^ XOR    
_ ACCEPT 
{ LAMBDA{
| OR     
} }LAMBDA
~ INVERT 















