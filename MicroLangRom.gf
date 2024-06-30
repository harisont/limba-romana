concrete MicroLangRom of MicroLang = open MicroResRom, Prelude in {
  -- fonte: Laurentia Jinga, "Grammatica romena per italiani" 
  
  lincat
    Det = {s: Gender => Case => Str ; n: Number} ;
    N = Noun ;

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

    --already_Adv = 
    animal_N = mkN "animal" "animale" ; 
    apple_N = mkN "măr" "mere" ;
    baby_N = mkN "copil" "copii" N ;
    --bad_A = 
    beer_N = mkN "bere" "beri" N ;
    --big_A = 
    bike_N = mkN "bicicletă" "biciclete" ; 
    bird_N = mkN "pasăre" "pasări" F ;
    --black_A = 
    blood_N = mkN "sânge" "sânge" ; -- plurale?
    --blue_A = 
    boat_N = mkN "barcă" ;
    book_N = mkN "carte" "cartea" "cărți" F ;
    boy_N = mkN "băiat" "băiat" "băieții" M ; -- wrong plural 
    --bread_N = 
    --break_V2 = 
    --buy_V2 = 
    --car_N = 
    --cat_N = 
    --child_N = 
    --city_N = 
    --clean_A = 
    --clever_A = 
    --cloud_N = 
    --cold_A = 
    --come_V = 
    --computer_N = 
    --cow_N = 
    --dirty_A = 
    --dog_N = 
    --drink_V2 = 
    --eat_V2 = 
    --find_V2 = 
    --fire_N = 
    --fish_N = 
    --flower_N = 
    --friend_N = 
    --girl_N = 
    --good_A = 
    --go_V = 
    --grammar_N = 
    --green_A = 
    --heavy_A = 
    --horse_N = 
    --hot_A = 
    --house_N = 
    --jump_V = 
    --kill_V2 = 
    --language_N = 
    --live_V = 
    --love_V2 = 
    --man_N = 
    --milk_N = 
    --music_N = 
    --new_A = 
    --now_Adv = 
    --old_A = 
    --play_V = 
    --read_V2 = 
    --ready_A = 
    --red_A = 
    --river_N = 
    --run_V = 
    --sea_N = 
    --see_V2 = 
    --ship_N = 
    --sleep_V = 
    --small_A = 
    --star_N = 
    --swim_V = 
    --teach_V2 = 
    --train_N = 
    --travel_V = 
    --tree_N = 
    --understand_V2 = 
    --wait_V2 = 
    --walk_V = 
    --warm_A = 
    --water_N = 
    --white_A = 
    --wine_N = 
    --woman_N = 
    --yellow_A = 
    --young_A = 

  oper 
    mkN = overload {
      mkN : Str -> Noun
        = \s -> lin N (smartNoun s (plur s)) ;
      mkN : Str -> Str -> Noun
        = \s, p -> lin N (smartNoun s p) ;
      mkN : Str -> Str -> Gender -> Noun 
        = \s, p, g -> lin N (mkNoun s p p g) ;
      mkN : Str -> Str -> Str -> Noun
        = \na, gd, pl -> lin N (mkNoun na gd pl (gend na pl)) ;
      mkN : Str -> Str -> Str -> Gender -> Noun
        = \na, gd, pl, g -> lin N (mkNoun na gd pl g) 
    } ;
}