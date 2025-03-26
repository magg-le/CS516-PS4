(* val inX : str -> bool

   tests whether a string over the alphabet {0, 1} is in B *)

fun inB x =
      (* given a string, test all the post-zero phrases of A on the suffixes *)
      Set.all
      (fn y => 
      case y of
        [] => true
        |  s :: rest => 
            if Sym.equal(s, Sym.fromString "0") then 
            ((Str.prefix(Str.fromString "001", rest)) orelse 
            (Str.prefix(Str.fromString "011", rest)) orelse 
            (Str.prefix(Str.fromString "101", rest)) orelse (Str.prefix(Str.fromString "111", rest)))
            else true) (StrSet.suffixes x);

(* val upto : int -> str set

   if n >= 0, then upto n returns all strings over alphabet {0, 1} of
   length no more than n *)

fun upto 0 : str set = Set.sing nil
  | upto n           =
      let val xs = upto(n - 1)
          val ys = Set.filter (fn x => length x = n - 1) xs
      in StrSet.union
         (xs, StrSet.concat(StrSet.fromString "0, 1", ys))
      end;

(* val partition : int -> str set * str set

   if n >= 0, then partition n returns (xs, ys) where:

   xs is all elements of upto n that are in X; and

   ys is all elements of upto n that are not in X *)

fun partition n = Set.partition inB (upto n);

(* val test = fn : int -> fa -> str option * str option

   if n >= 0, then test n returns a function f such that, for all FAs
   fa, f fa returns a pair (xOpt, yOpt) such that:

     If there is an element of {0, 1}* of length no more than n that
     is in X but is not accepted by fa, then xOpt = SOME x for some
     such x; otherwise, xOpt = NONE.

     If there is an element of {0, 1}* of length no more than n that
     is not in X but is accepted by fa, then yOpt = SOME y for some
     such y; otherwise, yOpt = NONE. *)

fun test n =
      let val (goods, bads) = partition n
      in fn fa =>
              let val accepted      = FA.accepted fa
                  val goodNotAccOpt = Set.position (not o accepted) goods
                  val badAccOpt     = Set.position accepted bads
              in ((case goodNotAccOpt of
                        NONE   => NONE
                      | SOME i => SOME(ListAux.sub(Set.toList goods, i))),
                  (case badAccOpt of
                        NONE   => NONE
                      | SOME i => SOME(ListAux.sub(Set.toList bads, i))))
              end
      end;