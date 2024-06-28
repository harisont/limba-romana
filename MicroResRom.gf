resource MicroResRom = open Prelude in {
  -- source: Laurentia Jinga, "Grammatica romena per italiani" 
  
  param
    Number = S | P ;
    Gender = M | F | N ;
    Case = NA | GD ; -- nominativo-accusativo + genitivo-dativo
    Definiteness = D | I ; -- presenza o assenza di articolo enclitico
}
