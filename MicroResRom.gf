resource MicroResRom = open Prelude in {
  -- fonte: Laurentia Jinga, "Grammatica romena per italiani" 
  
  param
    Number = S | P ;
    Gender = M | F | N ;
    Case = NA | GD ; -- nominativo-accusativo + genitivo-dativo (no vocativo)
    Definiteness = I | D ; -- presenza o assenza di articolo enclitico
  
  oper
    Noun : Type = {s: Number => Case => Definiteness => Str ; g: Gender} ;
    
    mkNoun : Str -> Str -> Str -> Gender -> Noun = \na, gd, pl, g -> {
      s = table {
        S => table {
          NA => table {
            I => na ;
            D => ad na S g NA
          } ;
          GD => table {
            I => gd ;
            D => ad gd S g GD
          }
        } ;
        P => table {
          NA => table {
            I => pl ;
            D => ad pl P g NA
          } ;
          GD => table {
            I => pl ;
            D => ad pl P g GD
          }
        }
      } ;
      g = g
    } ;

    smartNoun : Str -> Str -> Noun = \s,p -> let g = gend s p in case g of {
      M | N => mkNoun s s p g ;
      F => mkNoun s p p g
    } ;

    -- indovina genere da sing NA + plur (cf. pag. 42-44)
    gend : Str -> Str -> Gender = \s, p -> case <s, p> of {
      <_ + ("ău" | "âu"), _ + "e"> => N ;
      <_ + "ă", _ + ("e" | "i"  | "uri")> => F ;
      <_ + "ee", _ + "i"> => F ;
      <_ + "oaie", _ + "oaie"> => F ;
      <_ + "oare", _ + ("oare" | "ori")> => F ;
      <_ + "a", _ + "ei"> => F ;
      <_ + "e", _ + "uri"> => F ;
      <_ + ("a" | "ea" | "i"), _ + "le"> => F ;
      <_ + "ie", _ + ("i" | "ii")> => F ;
      <_ + ("u" | "e"), _ + "i"> => M ;
      <_ + "ai", _ + "e"> => N ;
      <_ + ("ău" | "âu"), _ + "e"> => N ;
      <_ + "ei", _ + "eie"> => N ;
      <_ + ("iu" | "âi"), _ + "ie"> => N ;
      <_ + "oi", _ + "oaie"> => N ;
      <_ + "ui", _ + "ie"> => N ;
      <_ + ("i" | "e"), _ + "e"> => N ;
      <_, _ + "i"> => M ; 
      <_, _ + "e"> => N ;
      <_,_> => N -- arbitrario
    } ;

    -- da singolare NA a plurale NA
    -- NB: ignora le alternanze fonetiche di cui al par. 7.2.2
    plur : Str -> Str = \s ->
      case s of {
        metr + ("u" | "e" | "ă") => metr + "i" ;
        urs + ("oaie" | "oare") => s ;
        limb + ("ie" | "ee" |"ă" | "e") => limb + "i" ;
        st + "ea" => st + "ele" ;
        cas + "ă" => cas + "e" ;
        sh + "a" => sh + "ei" ;
        zi => zi + "le " ;
        ferast + ("ău" | "âu") => ferast + "aie" ;
        obic + "ei" => obic + "eie" ;
        sicr + "iu" => sicr + "ie" ;
        capat + "âi" => capat + "âie" ;
        but + "oi" => but + "oaie" ;
        c + "ui" => c + "uie" ;
        bic + "i" => bic + "e" ;
        codic + "e" => s ;
        pom => pom + "i" ; -- maschile
        caiet => caiet + "e" -- neutro (purtroppo non usato)
      } ;

    -- aggiunge l'AD (Articolo Determinato) enclitico (cf. pag. 25) alla forma
    -- flessa di un sostantivo
    ad : Str -> Number -> Gender -> Case -> Str = \s, n, g, c -> 
      case n of {
        S => case g of {
          M | N => case s of {
            frat + "e" => case c of {
              NA => s + "le" ;
              GD => s + "lui"
              } ;
            teatr + "u" => case c of {
              NA => s + "l" ;
              GD => s + "lui "
              } ;
            tat + "ă" => case c of {
              NA => s + "l" ;
              GD => s + "lui"
              } ;
            pom => case c of {
              NA => s + "ul" ;
              GD => s + "ului"
              }
            } ;
          F => case s of {
            cas + "ă" => case c of {
              NA => cas + "a" ;
              GD => cas + "ei" 
              } ;
            vulp + "e" => case c of {
              NA => s + "a" ;
              GD => vulp + "ii"
              } ;
            x => case c of {
              NA => x + "a" ;
              GD => x + "i"
            } 
          } 
        } ;
        P => case g of {
          M => case c of {
            NA => s + "i" ;
            GD => s + "ior"
          } ;
          F | N => case c of {
            NA => s + "le" ;
            GD => s + "lor" 
          }
        }
      } ;
}
