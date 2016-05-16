open CommonGrade
open Hw7_2

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint 
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S,SkiLiquid.K),
                           SkiLiquid.I),
              SkiLiquid.V "x"))))
)

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.K,(SkiLiquid.V "x")),
              SkiLiquid.M (SkiLiquid.I,(SkiLiquid.V "x"))))))
)

let _ = output (fun () ->
  "(((x y) z) w)" =
    (SkiLiquid.pprint
       (SkiLiquid.M 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.V "x",SkiLiquid.V "y"),
              SkiLiquid.V "z"),
           SkiLiquid.V "w")))
)

let _ = output (fun () -> 
   "(((doo0 K) (S I)) K)" = 
      (SkiLiquid.pprint(SkiLiquid.react (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M(SkiLiquid.M(SkiLiquid.M(SkiLiquid.I, SkiLiquid.I), SkiLiquid.S), SkiLiquid.I), 
         (SkiLiquid.M (SkiLiquid.K, SkiLiquid.K))), (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.I, (SkiLiquid.V "doo0")), SkiLiquid.K), 
         (SkiLiquid.M (SkiLiquid.I, (SkiLiquid.M (SkiLiquid.S, SkiLiquid.I)))))))))))