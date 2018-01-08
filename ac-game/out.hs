[1 of 1] Compiling Util.List1       ( Util/List1.hs, /tmp/Util/List1.o )

==================== FINAL INTERFACE ====================
interface main@main:Util.List1 8001
  interface hash: 1ebb5aa96f10a4824e97115225e8d9f3
  ABI hash: 0bb2835b2647634323e800d3adce1e83
  export-list hash: d62756f3aa21d05466e7d674543238c9
  orphan hash: 693e9af84d3dfcc71e640e005bdc5e2e
  flag hash: f1024ca1b4f7faa578c1fc4b15056218
  sig of: Nothing
  used TH splices: False
  where
exports:
  Util.List1.pop
  Util.List1.push
  Util.List1.singleton
  Util.List1.toList
  Util.List1.top
  Util.List1.List1{Util.List1.List1}
module dependencies:
package dependencies: base-4.9.0.0* ghc-prim-0.5.0.0
                      integer-gmp-1.0.0.1
orphans: base-4.9.0.0:GHC.Base base-4.9.0.0:GHC.Float
family instance modules: base-4.9.0.0:Control.Applicative
                         base-4.9.0.0:Data.Either base-4.9.0.0:Data.Functor.Const
                         base-4.9.0.0:Data.Monoid base-4.9.0.0:Data.Type.Equality
                         base-4.9.0.0:GHC.Generics base-4.9.0.0:GHC.IO.Exception
                         base-4.9.0.0:GHC.TypeLits
import  -/  base-4.9.0.0:Prelude b9bddb97799837fe67a8d00939a5b6c3
491964d40aba2e0805e4d9f3b9151b7e
  $tc'List1 :: GHC.Types.TyCon
  {- HasNoCafRefs, Strictness: m,
     Unfolding: (GHC.Types.TyCon
                   14065018669395452462##
                   13705541659505691975##
                   Util.List1.$trModule
                   Util.List1.$tc'List2) -}
2b21750cdcaa648854b581f0862e7b41
  $tc'List2 :: GHC.Types.TrName
  {- HasNoCafRefs, Strictness: m1,
     Unfolding: (GHC.Types.TrNameS "'List1"#) -}
990326b18da2b83092061e11ea596683
  $tcList1 :: GHC.Types.TyCon
  {- HasNoCafRefs, Strictness: m,
     Unfolding: (GHC.Types.TyCon
                   188761108797221697##
                   1121243761216819633##
                   Util.List1.$trModule
                   Util.List1.$tcList2) -}
2b1b8ea1b54e00fd9ca2d05475968729
  $tcList2 :: GHC.Types.TrName
  {- HasNoCafRefs, Strictness: m1,
     Unfolding: (GHC.Types.TrNameS "List1"#) -}
6847aa3c31e8f0dd8379cd40cc1c44f6
  $trModule :: GHC.Types.Module
  {- HasNoCafRefs, Strictness: m,
     Unfolding: (GHC.Types.Module
                   Util.List1.$trModule2
                   Util.List1.$trModule1) -}
9fbcaa1bca4d6e3825b7e055d792404a
  $trModule1 :: GHC.Types.TrName
  {- HasNoCafRefs, Strictness: m1,
     Unfolding: (GHC.Types.TrNameS "Util.List1"#) -}
ba0a4ebedf3fc5a29a6bd763e4882e2a
  $trModule2 :: GHC.Types.TrName
  {- HasNoCafRefs, Strictness: m1,
     Unfolding: (GHC.Types.TrNameS "main"#) -}
08dff453c076f6feb17aa8e371d5b8b1
  newtype List1 a = List1 [a]
bfe606f55fc6e397466e556583b7b5bc
  pop :: Util.List1.List1 a -> GHC.Base.Maybe (Util.List1.List1 a)
  {- Arity: 1, Strictness: <S,1*U>,
     Unfolding: InlineRule (1, True, False)
                (\ @ a (ds :: Util.List1.List1 a) ->
                 case ds `cast` (Util.List1.N:List1[0] <a>_N) of wild {
                   [] -> Util.List1.pop1 @ a
                   : ds1 xs
                   -> case xs of wild1 {
                        [] -> GHC.Base.Nothing @ (Util.List1.List1 a)
                        : ipv ipv1
                        -> GHC.Base.Just
                             @ (Util.List1.List1 a)
                             wild1 `cast` (Sym (Util.List1.N:List1[0]) <a>_N) } }) -}
6edea93f5d915f4b565de505546f9ec5
  pop1 :: GHC.Base.Maybe (Util.List1.List1 a)
  {- Strictness: x -}
5af7617560836ea8f78a3fbadb058c9e
  push :: a -> Util.List1.List1 a -> Util.List1.List1 a
  {- Arity: 2, HasNoCafRefs, Strictness: <L,U><L,U>m2,
     Unfolding: InlineRule (0, True, True)
                GHC.Types.:
                  `cast`
                (forall (a :: <*>_N).
                 <a>_R
                 ->_R Sym (Util.List1.N:List1[0]) <a>_N
                 ->_R Sym (Util.List1.N:List1[0]) <a>_N) -}
fd40b8aa5996adea2fef8bc86abf712f
  singleton :: a -> Util.List1.List1 a
  {- Arity: 1, HasNoCafRefs, Strictness: <L,U>m2,
     Unfolding: InlineRule (0, True, True)
                Util.List1.singleton1
                  `cast`
                (forall (a :: <*>_N).
                 <a>_R ->_R Sym (Util.List1.N:List1[0]) <a>_N) -}
0d16eda013584a8a0ca46f8d98f4d22b
  singleton1 :: a -> [a]
  {- Arity: 1, HasNoCafRefs, Strictness: <L,U>m2,
     Unfolding: InlineRule (1, True, False)
                (\ @ a (a1 :: a) -> GHC.Types.: @ a a1 (GHC.Types.[] @ a)) -}
c24d50e3629afa2bef253c1c4c24516f
  toList :: Util.List1.List1 a -> [a]
  {- Arity: 1, HasNoCafRefs, Strictness: <S,1*U>,
     Unfolding: InlineRule (0, True, True)
                Util.List1.toList1
                  `cast`
                (forall (a :: <*>_N).
                 <Util.List1.List1 a>_R ->_R Util.List1.N:List1[0] <a>_N) -}
5baa2d2d4642841b505dd8b06579f319
  toList1 :: Util.List1.List1 a -> Util.List1.List1 a
  {- Arity: 1, HasNoCafRefs, Strictness: <S,1*U>,
     Unfolding: InlineRule (1, True, True)
                (\ @ a (ds :: Util.List1.List1 a) -> ds) -}
f1a4a125e2543b29a3df3108f581fa65
  top :: Util.List1.List1 a -> a
  {- Arity: 1, Strictness: <S,1*U>,
     Unfolding: InlineRule (1, True, False)
                (\ @ a (ds :: Util.List1.List1 a) ->
                 case ds `cast` (Util.List1.N:List1[0] <a>_N) of wild {
                   [] -> Util.List1.top1 @ a : x ds1 -> x }) -}
22e2355d7f398c8ca599401ae0c7aab7
  top1 :: a
  {- Strictness: x -}
vectorised variables:
vectorised tycons:
vectorised reused tycons:
parallel variables:
parallel tycons:
trusted: safe
require own pkg trusted: False


