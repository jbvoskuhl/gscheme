grammar GScheme;

expression : SYMBOL      # symbolExpression
           | boolean     # booleanExpression
           | INTEGER     # integerExpression
           | vector      # integerExpression
           | character   # characterExpression
           | str         # strExpression
           | list        # listExpression
           ;

vector : '[' expression* ']' ;

character : '#\\'CHARACTER 
          | '#\\x'HEXCODE
          | '#\\X'HEXCODE
          | '#\\alarm'
          | '#\\backspace'
          | '#\\delete'
          | '#\\escape'
          | '#\\newline' 
          | '#\\null'
          | '#\\return'
          | '#\\space'
          | '#\\tab' ;

str : '"' character* '"' ;

list : '(' expression* ')'
     | '(' expression+ '.' expression ')' ;

boolean : '#t'
        | '#f'
        | '#true'
        | '#false' ;
CHARACTER : [a-zA-Z] ;
SYMBOL : [a-zA-Z]+ ;
INTEGER : [0-9]+ ;
HEXCODE : [0-9a-fA-F]+ ;
WS : [ \t\r\n]+ -> skip;