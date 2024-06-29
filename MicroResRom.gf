resource MicroResRom = open Prelude in {
  -- fonte: Laurentia Jinga, "Grammatica romena per italiani" 
  
  param
    Number = S | P ;
    Gender = M | F | N ;
    Case = NA | GD ; -- nominativo-accusativo + genitivo-dativo (no vocativo)
    Definiteness = I | D ; -- presenza o assenza di articolo enclitico
  
  oper
    Noun : Type = {s: Number => Case => Definiteness => Str ; g: Gender} ;
    
    mkNoun : Str -> Str -> Str -> Str -> Gender -> Noun =
      \nas, nap, gds, gdp, g -> {
        s = table {
          S => table {
            NA => table {
              I => nas ;
              D => ad nas S g
            } ;
            GD => table {
              I => gds ;
              D => ad gds S g
            }
          } ;
          P => table {
            NA => table {
              I => nap ;
              D => ad nap P g
            } ;
            GD => table {
              I => gdp ;
              D => ad gdp P g
            }
          }
        } ;
        g = g
      } ;

    --smartNoun : Str -> Str -> Noun = \nas, nap -> case <nas, nap> of {
    --  -- ...
    --  <z + i,_> => mkFeminineNoun ;
    --} ;

    -- da singolare NA a plurale NA
    -- NB: ignora le alternanze fonetiche di cui al par. 7.2.2
    plur : Str -> Gender -> Str = \s,g ->
      case g of {
        M => case s of { -- cf. pag. 42
          metr + ("u" | "e" | "ă") => metr + "i" ;
          pom => pom + "i "  
        } ;
        -- cf. pag. 42-43, dando per scontato che i vari casi siano ordinati 
        -- per frequenza
        F => case s of { 
          urs + ("oaie" | "oare") => s ;
          limb + ("ie" | "ee" |"ă" | "e") => limb + "i" ;
          st + "ea" => st + "ele" ;
          cas + "ă" => cas + "e" ;
          sh + "a" => sh + "ei" ;
          zi => zi + "le "
        } ;
        N => case s of { -- cf. pag. 44
          ferast + ("ău" | "âu") => ferast + "aie" ;
          obic + "ei" => obic + "eie" ;
          sicr + "iu" => sicr + "ie" ;
          capat + "âi" => capat + "âie" ;
          but + "oi" => but + "oaie" ;
          c + "ui" => c + "uie" ;
          bic + "i" => bic + "e" ;
          codic + "e" => s ;
          caiet => caiet + "e" 
        } 
      } ;

    -- aggiunge l'AD (Articolo Determinato) enclitico (cf. pag. 25) alla forma
    -- flessa di un sostantivo
    ad : Str -> Number -> Gender -> Str = \s, n, g -> 
      case n of {
        S => case g of {
          M | N => case s of {
            frat + "e" => s + "le" ;
            teatr + "u" => s + "l" ;
            tat + "ă" => s + "l" ;
            pom => s + "ul"
            } ;
          F => case s of {
            cas + "ă" => cas + "a" ;
            vulp + "e" => s + "a"
          } 
        } ;
        P => case g of {
          M => s + "i" ;
          F | N => s + "le" 
        }
      } ;
}
