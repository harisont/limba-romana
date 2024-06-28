concrete MicroLangRom of MicroLang = open MicroResRom, Prelude in {
  -- source: Laurentia Jinga, "Grammatica romena per italiani" 
  
  lincat
    Det = {s: Gender => Case => Str ; n: Number} ;
    N = {s: Number => Definiteness => Case => Str ; g: Gender} ;

  lin 
    a_Det = {
      s = table {
        M | N => table { -- al singolare, N == M (cf. 7.1.1)
          NA => "un" ;
          GD => "unui"
        } ;
        F => table {
          NA => "o" ;
          GD => "unei"
        }
      } ;
      n = S 
    } ;

    aPl_Det = {
      s = table {
        M | F | N => table {
          NA => "niște" ;
          GD => "unor"
        } 
      } ;
        n = P 
    } ;

    -- gli A.D. sono enclitici, come in Svedese (cf. 6.3.1)

    the_Det = {
      s = \\_,_ => [] ;
      n = S
    } ;

    thePl_Det = {
      s = \\_,_ => [] ;
      n = P
    } ;
}